package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be. The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: NewMapTypeInfo,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    val expectedKeys = Environment.Base.idToObjectWithType.keys
    val additionalKeys = env.idToObjectWithType.keys.toSet -- expectedKeys.toSet

    val result: Outcome[NewMapObjectWithType, String] = expression match {
      case NaturalNumberParse(i: Long) => Success(NewMapObjectWithType.untyped(Index(i)))
      case IdentifierParse(s: String, true) => Success(NewMapObjectWithType.untyped(IdentifierInstance(s)))
      case IdentifierParse(s: String, false) => {
        env.lookup(s) match {
          case Some(objectWithType) => Success(objectWithType)
          case None => Success(NewMapObjectWithType.untyped(IdentifierInstance(s)))
        }
      }
      case ApplyParse(startingFunction: ParseTree, applications: Vector[ParseTree]) => {
        for {
          functionTypeChecked <- {
            typeCheck(
              startingFunction,
              NewMapTypeInfo.init,
              env
            )
          }

          result <- processMultipleFunctionApplications(functionTypeChecked, applications, env)
        } yield result
      }
      case CommandList(values: Vector[ParseTree]) => {
        resolveTypeInfo(expectedType, env) match {
          case ExplicitlyTyped(MapT(IdentifierT, TypeT, completeness, featureSet)) => {
            val mapT = MapT(IdentifierT, TypeT, completeness, featureSet)
            // TODO - do we really want to consider this a parameter list, or just treat it like another map?
            typeCheckParamsStandalone(values, env, mapT)
          }
          case ExplicitlyTyped(MapT(keyType, valueType, completeness, featureSet)) => {
            val mapT = MapT(keyType, valueType, completeness, featureSet)
            for {
              keyTypeT <- Evaluator.convertObjectToType(keyType, env)
              _ <- Evaluator.convertObjectToType(valueType, env)

              allowPatternMatch = featureSet != BasicMap

              mapValues <- typeCheckLiteralMap(
                values,
                Some(keyType),
                Some(valueType),
                env,
                patternMatchingAllowed = allowPatternMatch
              )

              isCovered <- {
                if (completeness != RequireCompleteness) Success(true)
                else {
                  doMapValuesCoverType(mapValues, keyTypeT)
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + keyType
              )
            } yield {
              NewMapObjectWithType.withTypeE(MapInstance(mapValues), mapT)
            }
          }
          case ExplicitlyTyped(StructT(fieldType, params)) => {
            for {
              parameterList <- structParamsIntoParameterList(params, env)
              result <- typeCheckStruct(parameterList, values, env)
            } yield {
              val nObject = StructInstance(result.map(x => x._1 -> x._3))
              NewMapObjectWithType.withTypeE(nObject, StructT(fieldType, params))
            }
          }
          case ExplicitlyTyped(CaseT(casesType, MapInstance(caseToType))) => {
            if (values.length != 1) {
              Failure("A case may only select 1 value")
            } else {
              val firstValue = values(0)
              firstValue match {
                case BindingCommandItem(valueIdentifier, valueObject) => {
                  val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

                  valueIdOpt match {
                    case Some(valueId) => {
                      caseToType.toMap.get(IdentifierInstance(valueId)) match {
                        case Some(nType) => {
                          for {
                            nTypeT <- Evaluator.convertObjectToType(nType, env)
                            nObjectWithType <- typeCheck(valueObject, ExplicitlyTyped(nTypeT), env)
                          } yield {
                            val valueObject = nObjectWithType.nObject
                            NewMapObjectWithType(
                              CaseInstance(IdentifierInstance(valueId), valueObject),
                              expectedType
                            )
                          }
                        }
                        case _ => {
                          Failure("Identifier " + valueId + " not a member of case class: " + valueObject)
                        }
                      }
                    }
                    case None => {
                      Failure("Expected idenfier: " + valueIdentifier)
                    }
                  }
                }
                case _ => {
                  Failure("Value " + firstValue + " not a member of case class: " + caseToType)
                }
              }
            }
          }
          case ExplicitlyTyped(TypeT) => {
            typeCheckParamsStandalone(values, env, TypeT)
          }
          case ImplicitlyTyped(Vector()) => {
            Failure("CommandLists must be explicitly typed")
          }
          case _ => {
            Failure("CommandLists not working yet with this expected type: " + values + " exp: " + expectedType)
          }
        }
      }
      case BindingCommandItem(key, value) => {
        typeCheck(CommandList(Vector(expression)), expectedType, env)
      }
      case LambdaParse(params, expression) if (expectedType == ExplicitlyTyped(TypeT)) => {
        for {
          inputType <- typeSpecificTypeChecker(params, env)
          newEnv <- inputType match {
            case StructT(fieldType, params) => {
              for {
                parameterList <- structParamsIntoParameterList(params, env)
                inputParamsT <- convertParamsObjectToType(parameterList, env)
              } yield {
                env.newParams(inputParamsT)
              }
            }
            case _ => Success(env)
          }

          outputType <- typeSpecificTypeChecker(expression, newEnv)
        } yield {
          val mapType = MapT(
            inputType,
            outputType,
            completeness = RequireCompleteness,
            featureSet = FullFunction
          )

          NewMapObjectWithType.withTypeE(mapType, TypeT)
        }
      }
      case LambdaParse(params, expression) => {
        val ErrorMessageForBasicMap = {
          // Better error message?
          "A Basic Map is expected, and a lambda expression does not cover this"
        }

        for {
          newParams <- params match {
            case CommandList(values) => typeCheckParameterList(values, env)
            case BindingCommandItem(key, value) => typeCheckParameterList(Vector(params), env)
            case IdentifierParse(id, _) => {
              expectedType match {
                case ExplicitlyTyped(MapT(inputType, outputType, completeness, featureSet)) => {
                  for {
                    _ <- Outcome.failWhen(featureSet == BasicMap, ErrorMessageForBasicMap)
                    inputT <- Evaluator.convertObjectToType(inputType, env)
                  } yield {
                    Vector(id -> inputT)
                  }
                }
                case _ => {
                  Failure("Lambda Expression is untyped, and this is not implemented yet.")
                }
              }

            }
            case _ => Failure("Lambda Values must be variable bindings " + params + " -- " + expression)
          }
          expectedExpressionType <- expectedType match {
            case ExplicitlyTyped(MapT(inputType, outputType, completeness, featureSet)) => {
              for {
                _ <- Outcome.failWhen(featureSet == BasicMap, ErrorMessageForBasicMap)
                outputT <- Evaluator.convertObjectToType(outputType, env)
              } yield ExplicitlyTyped(outputT)
            }
            case _ => Success(NewMapTypeInfo.init)
          }

          tc <- typeCheck(expression, expectedExpressionType, env.newParams(newParams))

          typeFound <- tc.nTypeInfo match {
            case ExplicitlyTyped(nType) => Success(nType)
            case ImplicitlyTyped(convs) => {
              expectedType match {
                //case ExplicitlyTyped(TypeT) => {
                //  // TODO: more cases
                //  Success(TypeT)
                //}
                // TODO - I don't think this is right
                case ExplicitlyTyped(t) => Success(t)
                // TODO: this case must be further filled out
                case _ => Failure("Cannot do implicitly typed lambda statements for expected " + expectedType)
              }
            }
          }
        } yield {
          val newParamsAsObj = newParams.map(x => IdentifierInstance(x._1) -> x._2)
          val fieldType = {
            Subtype(
              IdentifierT,
              MapInstance(newParamsAsObj.map(x => x._1 -> Index(1)))
            )
          }

          NewMapObjectWithType.withTypeE(
            LambdaInstance(StructParams(newParams), tc.nObject),
            Environment.simpleFuncT(StructT(fieldType, MapInstance(newParamsAsObj)), typeFound)
          )
        }
      }
    }

    for {
      objectWithType <- result

      // TODO: figure out what to really do here
      withAdditionalType <- expectedType match {
        case ExplicitlyTyped(nType) => additionalExpectedType(objectWithType, nType, env)
        case ImplicitlyTyped(convs) => additionalExpectedTypes(objectWithType, convs, env)
      }
    } yield withAdditionalType
  }

  def convertParamsObjectToType(
    params: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
    params match {
      case (IdentifierInstance(id), nmObject) +: restOfParams => {
        for {
          restConverted <- convertParamsObjectToType(restOfParams, env)
          nmType <- Evaluator.convertObjectToType(nmObject, env)
        } yield {
          (id, nmType) +: restConverted
        }
      }
      case (id, nmObject) +: restOfParams => Failure(s"Expecting IdentifierInstance: $id")
      case _ => Success(Vector.empty)
    }
    
  }

  // For ReqMaps, we need to ensure that all of the values are accounted for.
  def doMapValuesCoverType(
    values: Vector[(NewMapObject, NewMapObject)],
    nType: NewMapType
  ): Outcome[Boolean, String] = {
    val keys = values.map(_._1).toSet

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    val genericPatternExists = keys.exists(key => key match {
      case ParameterObj(_) => true
      case _ => false
    })

    if (genericPatternExists) Success(true)
    else {
      for {
        keysToMatch <- findAllKeysToMatch(nType)
      } yield {
        (keysToMatch -- keys).isEmpty && (keys -- keysToMatch).isEmpty
      }
    }
  }

  def findAllKeysToMatch(nType: NewMapType): Outcome[Set[NewMapObject], String] = nType match {
    case Index(i) => {
      Success((0 until i.toInt).map(j => Index(j.toLong)).toSet)
    }
    case Subtype(parentType, simpleFunction) => simpleFunction match {
      // TODO: What if values is too large? Should we make some restrictions here?
      case MapInstance(values) => Success(values.map(_._1).toSet)
      // TODO(2022): if simpleFunction is boolean function on a small finite parentType, we should be able to enumerate those
      // TODO(2022): this is also one of those advanced cases where we want to know if one set is a subset of another through some advanced argument (like monotonicity)
      case _ => Failure("Can't enumerate the allowed values of " + simpleFunction)
    }
    case _ => {
      Failure("Can't create a ReqMap with keyType " + nType + ". Either this is an infinite type, or the functionality is unimplemented")
    }
  }

  // This map could include pattern matching
  def typeCheckLiteralMap(
    values: Vector[ParseTree],
    expectedKeyType: Option[NewMapObject],
    expectedValueType: Option[NewMapObject],
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        // TODO(2022): We shouldn't have to do all these conversions in the future
        val kTypeOutcome = expectedKeyType match {
          case None => Success(NewMapTypeInfo.init)
          case Some(e) => {
            for {
              eType <- Evaluator.convertObjectToType(e, env)
            } yield ExplicitlyTyped(eType)
          }
        }

        val vTypeOutcome = expectedValueType match {
          case None => Success(NewMapTypeInfo.init)
          case Some(e) => {
            for {
              eType <- Evaluator.convertObjectToType(e, env)
            } yield ExplicitlyTyped(eType)
          }
        }

        for {
          kType <- kTypeOutcome
          vType <- vTypeOutcome
          resultKey <- typeCheckWithPatternMatching(k, kType, env, patternMatchingAllowed)
          objectFoundKey = resultKey.typeCheckResult.nObject

          tc2 <- typeCheck(v, vType, resultKey.newEnvironment)
          objectFoundValue = tc2.nObject

          restOfMap <- typeCheckLiteralMap(
            restOfValues,
            expectedKeyType,
            expectedValueType,
            env,
            patternMatchingAllowed
          )
        } yield {
          (objectFoundKey -> objectFoundValue) +: restOfMap
        }
      }
      case s +: _ => {
        Failure("No binding found in map for item " + s)
      }
      case _ => Success(Vector.empty)
    }
  }

  case class TypeCheckWithPatternMatchingResult(
    typeCheckResult: NewMapObjectWithType,
    newEnvironment: Environment
  )

  // TODO: This should be integrated into the regular type-check script.
  // we just need a way to know whether we are in an area where pattern matching is allowed or not!
  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapTypeInfo,
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    expression match {
      case IdentifierParse(id, false) if (patternMatchingAllowed) => {
        val expectingIdentifier = expectedType == ExplicitlyTyped(IdentifierT)

        if (!expectingIdentifier && env.lookup(id).nonEmpty) { // This is the generic pattern
          // TODO: rethink what to do if the identifier is already defined in the environement
          Failure(s"Pattern matching clashes with defined variable: $id")
        } else {
          val nObject = if (expectingIdentifier) IdentifierInstance(id) else ParameterObj(id)
          val tcResult = NewMapObjectWithType(nObject, expectedType)
          val envCommand = FullEnvironmentCommand(id, NewMapObjectWithType(ParameterObj(id), expectedType))
          val newEnv = env.newCommand(envCommand)

          Success(TypeCheckWithPatternMatchingResult(tcResult, newEnv))
        }
      }
      case _ => {
        // No patterns matched, fall back to regular type checking
        for {
          tc <- typeCheck(expression, expectedType, env)
        } yield {
          TypeCheckWithPatternMatchingResult(tc, env)
        }
      }
    }

  }

  /** 
   * There is a requirement that the objectWithType is a member of typeExpected, or at least convertible.
   * If this is already the case, then return the original objectWithType
   * If objectWithType cannot be converted, then fail
   * If objectWithType is implicitly typed and can be of typeExpected, but it requires appending more type information to it 
   * @param objectWithType the object under consideration
   * @param typeExpected the type that the object must fit into
   * @param env the environment
   */
  def additionalExpectedType(
    objectWithType: NewMapObjectWithType,
    typeExpected: NewMapType,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    objectWithType.nTypeInfo match {
      case ExplicitlyTyped(nType) => {
        if (isTypeConvertible(nType, typeExpected, env)) {
          Success(objectWithType) 
        } else {
          Failure("In expression " + objectWithType.nObject.toString + " expected type " + typeExpected.toString + " found type " + nType.toString + " and cannot convert\n")
        }
      }
      case ImplicitlyTyped(typeConversions) => {
        if (typeConversions.isEmpty) {
          for {
            unit <- isRawObjectConvertibleToType(objectWithType.nObject, typeExpected, env)
          } yield {
            NewMapObjectWithType(objectWithType.nObject, ImplicitlyTyped(Vector(typeExpected)))
          }
        } else if (typeConversions.length == 1) {
          // TODO, this head is fine, but clean up your scala MAX!
          val singleType = typeConversions.head
          if (isTypeConvertible(typeExpected, singleType, env)) {
            // We already know we can convert to singleType, so no change neccesary
            Success(objectWithType)
          } else if (isTypeConvertible(singleType, typeExpected, env)) {
            // In this case, go with single type because it is more specific
            Success(NewMapObjectWithType(objectWithType.nObject, ImplicitlyTyped(Vector(singleType))))
          } else {
            // TODO: there are cases where this should be possible, so add those here.
            Failure("Object "+ objectWithType.nObject.toString + " cannot be a part of both type " + singleType.toString + " and type " + typeExpected.toString)
          }
        } else {
          Failure("Cannot do ImplicitlyTyped stuff yet for multiple implicit types")
        }
      }
    }
  }

  def additionalExpectedTypes(
    objectWithType: NewMapObjectWithType,
    typeConvs: Vector[NewMapType],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    typeConvs match {
      case nType +: restOfTypes => {
        for {
          withFirstType <- additionalExpectedType(objectWithType, nType, env)
          withOtherTypes <- additionalExpectedTypes(objectWithType, restOfTypes, env)
        } yield withOtherTypes
      }
      case _ => Success(objectWithType)
    }
  }

  def isTypeConvertibleAsObjects(
    startingType: NewMapObject,
    endingType: NewMapObject,
    env: Environment
  ): Boolean = {
    val result = for {
      startingT <- Evaluator.convertObjectToType(startingType, env)
      endingT <- Evaluator.convertObjectToType(endingType, env)
    } yield isTypeConvertible(startingT, endingT, env)

    result match {
      case Success(r) => r
      case Failure(_) => false
    }
  }

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  // ALSO: this is for automatic conversion. There should be another conversion, which is a superset of this, which is less automatic
  //  - To be used in cases where you want the programmer to specifically ask for a conversion!
  def isTypeConvertible(
    startingType: NewMapType,
    endingType: NewMapType,
    env: Environment
  ): Boolean = {
    val resolvedStartingType = resolveType(startingType, env)
    val resolvedEndingType = resolveType(endingType, env)

    (resolvedStartingType, resolvedEndingType) match {
      case (_, TypeT) => refersToAType(resolvedStartingType, env)
      case (Index(j), Index(i)) => {
        // The indecies must match, even though you can theoretically convert a smaller index to a bigger index.
        // This conversion is not made explicit to prevent people from accessing an array or map with the wrong index value
        (j == i)
      }
      // TODO: if these mapinstances contain pattern matches, we can run into trouble
      //  - work this out further!
      case (Subtype(startingParentType, MapInstance(startingMi)), Subtype(endingParentType, MapInstance(endingMi))) => {
        val parentResult = for {
          startingParentT <- Evaluator.convertObjectToType(startingParentType, env)
          endingParentT <- Evaluator.convertObjectToType(endingParentType, env)
        } yield isTypeConvertible(startingParentT, endingParentT, env)

        val parentsAreConverible = parentResult match {
          case Success(true) => true
          case _ => false
        }

        parentsAreConverible && (startingMi.map(_._1).forall(v => endingMi.map(_._1).exists(_ == v)))
      }
      case (
        MapT(startingInputType, startingOutputType, startingCompleteness, startingFeatureSet),
        MapT(endingInputType, endingOutputType, endingCompleteness, endingFeatureSet)
      ) => {
        // TODO: there could be more cases, and I'm pretty sure this is done elsewhere
        // TODO(2022): Removed during StructT refactoring.. not sure if this is needed
        /*val newEnv = startingInputType match {
          case StructT(params) => {
            convertParamsObjectToType(params, env) match {
              case Failure(_) => throw new Exception("This shouldn't happen.. investigate!")
              case Success(newParams) => env.newParams(newParams)
            }
          }
          case _ => env
        }*/
        val newEnv = env

        val isFeatureSetConvertible = startingFeatureSet match {
          case BasicMap => true
          case SimpleFunction => (endingFeatureSet != BasicMap)
          case FullFunction => (endingFeatureSet == FullFunction)
        }

        // TODO: We may be able to convert between different completeness types, but add this as needed
        val isMapCompletenessConvertible = (startingCompleteness == endingCompleteness)

        isTypeConvertibleAsObjects(startingInputType, endingInputType, env) &&
          isTypeConvertibleAsObjects(startingOutputType, endingOutputType, newEnv) &&
          isFeatureSetConvertible &&
          isMapCompletenessConvertible
      }
      // TODO: we need to know the underlying type of v here to make a move!
      // - In the future, types will require all underlying NewMapObjects already be type type-checked
      // - Therefore, we can solve this more easily!
      case (Subtype(parentType, MapInstance(v)), otherType) => {
        val parentResult = for {
          parentT <- Evaluator.convertObjectToType(parentType, env)
        } yield isTypeConvertible(parentT, otherType, env)
        
        parentResult match {
          case Success(true) => true
          case _ => false
        }
      }
      case _ => {
        (resolvedEndingType == resolvedStartingType) || {
          resolvedStartingType match {
            case StructT(fieldType, params) => {
              val outcomeResult = for {
                parameterList <- structParamsIntoParameterList(params, env)
                _ <- Outcome.failWhen(parameterList.length != 1, "requires a single param only")
                paramT <- Evaluator.convertObjectToType(parameterList.head._2, env)
              } yield isTypeConvertible(paramT, resolvedEndingType, env)

              outcomeResult.toOption.getOrElse(false)
            }
            case _ => false
          }
        }
      }
    }
  }

  // Can this untyped object be interpreted as type nType?
  def isRawObjectConvertibleToType(
    nObject: NewMapObject,
    nType: NewMapType,
    env: Environment
  ): Outcome[Unit, String] = {
    def failMsg(extra: String) = {
      Failure(extra + " Could not implicitly interpret object " + nObject + " as type " + nType)
    }

    val success = Success(())

    resolveType(nType, env) match {
      case Index(i) => nObject match {
        case Index(j) => if (j < i) success else {
          failMsg("A")
        }
        case _ => failMsg("B")
      }
      case TypeT => {
        for {
          _ <- Evaluator.convertObjectToType(nObject, env)
        } yield ()
      }
      case CommandTypeT => {
        // TODO(2022): remember that we should be able to define our own command types in the future!
        for {
          resultType <- Evaluator.convertObjectToType(nObject, env)
          default <- Evaluator.getDefaultValueOfCommandType(resultType, env)
        } yield ()
      }
      case CountT => nObject match {
        case Index(_) => success
        case _ => failMsg("Only counting numbers can convert to the count type.\n")
      } 
      case IdentifierT => nObject match {
        case IdentifierInstance(_) => success
        case _ => failMsg("D")
      }
      case MapT(input, output, _, _) => nObject match {
        case MapInstance(values) => {
          // TODO(2022): Make sure this is tested, and this is the behavior we want!
          for {
            inputT <- Evaluator.convertObjectToType(input, env)
            outputT <- Evaluator.convertObjectToType(output, env)

            successCondition = {
              values.forall(x => {
                isRawObjectConvertibleToType(x._1, inputT, env).isSuccess &&
                isRawObjectConvertibleToType(x._2, outputT, env).isSuccess
              })
            }

            _ <- Outcome.failWhen(!successCondition, "E")
          } yield ()
        }
        case _ => failMsg("F")
      }
      case StructT(fieldType, params) => nObject match {
        case StructInstance(value) => {
          //////////////////// TODO  not always
          success
        }
        case otherObject => {
          // This could also be the first input to the struct

          params match {
            case MapInstance(firstParam +: otherParams) => {
              if (otherParams.length == 0) {
                for {
                  firstParamType <- Evaluator.convertObjectToType(firstParam._2, env)
                  result <- isRawObjectConvertibleToType(nObject, firstParamType, env)
                } yield result
              } else {
                failMsg("Can't convert non-struct to multiple-valued struct")
              }
            }
            case _ => failMsg("G")
          }
        }
      }
      case CaseT(casesType, MapInstance(caseToType)) => nObject match {
        case CaseInstance(constructor, input) => {
          caseToType.toMap.get(constructor) match {
            case None => failMsg("No constructor for type: " + constructor)
            case Some(t) => for {
              tT <- Evaluator.convertObjectToType(t, env)
              result <- isRawObjectConvertibleToType(input, tT, env)
            } yield result
          }
        }
        // TODO: there can be some conversions here; we don't neccesarily need to go to the fail case
        case _ => failMsg("G Case")
      }
      case CaseT(_, _) => {
        // TODO(2022): Definitely resolve this
        Failure("Cases without explicit mapinstances as types are not converible yet")
      }
      case SubstitutableT(s) => {
        failMsg("I")
      }
      case Subtype(parentType, MapInstance(mi)) => {
        for {
          parentT <- Evaluator.convertObjectToType(parentType, env)
          _ <- isRawObjectConvertibleToType(nObject, parentT, env)

          // TODO: the equality here doesn't work - for example 
          _ <- Outcome.failWhen(
            !mi.map(x => x._1).exists(_ == nObject),
            "[Subtype] Could not implicitly interpret object " + nObject + " as type " + nType
          )
        } yield ()
      }
      case Subtype(parentType, func) => {
        // TODO: maybe nObject can be passed to func and come out true.. we'll have to see!
        // - Is it okay to apply nObject to func? think this through!!
        failMsg("J")
      }
    }
  }

  def substituteType(
    newMapType: NewMapObject,
    env: Environment
  ): Outcome[NewMapType, String] = {
    Evaluator.convertObjectToType(
      Evaluator.makeRelevantSubstitutions(newMapType, env),
      env
    )
  }

  // TODO - merge with substituteType
  def resolveType(
    typeFound: NewMapObject,
    env: Environment
  ): NewMapType = {
    // TODO: not type safe
    substituteType(typeFound, env) match {
      case Success(nType) => nType
      case Failure(reason) => {
        println(reason)
        // TODO: figure out what's going wrong here
        //Thread.dumpStack()
        Index(0)
      }
    }
  }

  def resolveTypeInfo(
    typeInfoFound: NewMapTypeInfo,
    env: Environment
  ): NewMapTypeInfo = typeInfoFound match {
    case ExplicitlyTyped(nType) => ExplicitlyTyped(resolveType(nType, env))
    case ImplicitlyTyped(nTypes) => ImplicitlyTyped(nTypes.map(nType => resolveType(nType, env)))
  }

  abstract sealed class IdentifierCheckResult
  case class FoundIdentifier(s: String) extends IdentifierCheckResult
  case object FoundIdentifierUnknownValue extends IdentifierCheckResult
  case object NotAnIdentifier extends IdentifierCheckResult

  // TODO
  // I think we need to rethink the case where an identifier refers to another identifier
  // because the type would be IdentifierT, and we wouldn't know the actual letter!
  def checkForIdentifier(
    expression: ParseTree,
    env: Environment
  ): IdentifierCheckResult = {
    expression match {
      case IdentifierParse(name, force) => {
        env.objectOf(name) match {
          case Some(ParameterObj(name)) => FoundIdentifier(name)
          case Some(substitutedType) if (!force) => {
            // In this case, we have an identifier but it's actually supposed to be replaced with something else
            // For all we know, this will be replaced with a real identifier
            // But even if we can prove it does (because substitutedType is an IdentifierType), we won't know the
            //  name of the identifier.
            // TODO (check if I'm doing the right thing here)

            FoundIdentifierUnknownValue
          }
          case _ => FoundIdentifier(name)
        }
      }
      case _ => NotAnIdentifier
    }
  }

  def checkForKnownIdentifier(
    expression: ParseTree,
    env: Environment
  ): Option[String] = {
    checkForIdentifier(expression, env) match {
      case FoundIdentifier(name) => Some(name)
      case _ => None
    }
  }

  // TODO: This fails on a struct!!!
  def typeCheckParameterList(
    parameterList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
    parameterList match {
      case BindingCommandItem(identifier, typeOfIdentifier) +: otherIdentifiers => {
        checkForIdentifier(identifier, env) match {
          case NotAnIdentifier => Failure("Expected Identifier: " + identifier.toString)
          case FoundIdentifier(name) => {
            // Now we have the variable.. next step we need the type
            // TODO - we need a type check that's specific to a type
            for {
              _ <- typeCheck(typeOfIdentifier, ExplicitlyTyped(TypeT), env)
              typeOfIdentifierAsType <- typeSpecificTypeChecker(typeOfIdentifier, env)
              expandedEnv = env.newParam(name, typeOfIdentifierAsType)
              restOfParams <- typeCheckParameterList(otherIdentifiers, expandedEnv)
            } yield {
              (name -> typeOfIdentifierAsType) +: restOfParams
            }
          }
          case FoundIdentifierUnknownValue => {
            for {
              _ <- typeCheck(typeOfIdentifier, ExplicitlyTyped(TypeT), env)
              restOfParams <- typeCheckParameterList(otherIdentifiers, env)
            } yield {
              restOfParams
            }
          }
        }
      }
      case otherItem +: _ => {
        Failure("Must bind an identifier and a type in the parameter list.")
      }
      case _ => Success(Vector.empty)
    }
  }

  def typeCheckParamsStandalone(
    parameterList: Vector[ParseTree],
    env: Environment,
    expectedType: NewMapType
  ): Outcome[NewMapObjectWithType, String] = for {
    newParams <- typeCheckParameterList(parameterList, env)
  } yield {
    NewMapObjectWithType.withTypeE(paramsToObject(newParams), expectedType)
  }

  def paramsToObject(
    params: Vector[(String, NewMapType)]
  ): MapInstance = {
    val paramsAsObjects: Vector[(NewMapObject, NewMapObject)] = for {
      (name, nmt) <- params
    } yield {
      IdentifierInstance(name) -> nmt
    }
    //// TODO
    MapInstance(paramsAsObjects)
  }

  // Assume that params is already type checked
  // We just want to concrete list of fields
  def structParamsIntoParameterList(
    params: NewMapObject,
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    params match {
      case MapInstance(values) => Success(values)
      case _ => Failure(s"Cannot convert params into parameter list: $params")
    }
  }

  /*
   * We want to ensure that the struct was created correctly
   * TODO(2022): This should no longer by the struct checking function because it manipulates the environment
   * - We use this feature, though, so we need to wait until it's separated out
   */
  def typeCheckStruct(
    parameterList: Vector[(NewMapObject, NewMapObject)],
    //parameterList: Vector[(String, NewMapObject)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(String, NewMapType, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((IdentifierInstance(paramId), typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              typeOfIdentifierT <- Evaluator.convertObjectToType(typeOfIdentifier, env)
              tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifierT), env)

              substObj = Evaluator.makeRelevantSubstitutions(tc.nObject, env)

              envCommand = Environment.eCommand(paramId, typeOfIdentifierT, substObj)
              newEnv = env.newCommand(envCommand)
              result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
            } yield {
              (paramId, typeOfIdentifierT, substObj) +: result
            }
          }
          case Some(valueId) => {
            Failure("Ids don't match: " + paramId + " --- " + valueId)
          }
          case None => {
            Failure("Id note found for " + paramId)
          }
        }
      }      
      case (((IdentifierInstance(paramId), typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          typeOfIdentifierT <- Evaluator.convertObjectToType(typeOfIdentifier, env)
          tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifierT), env)
          substObj = Evaluator.makeRelevantSubstitutions(tc.nObject, env)
          envCommand = Environment.eCommand(paramId, typeOfIdentifierT, substObj)
          newEnv = env.newCommand(envCommand)
          result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
        } yield {
          (paramId, typeOfIdentifierT, substObj) +: result
        }
      }
      case _ => {
        if (parameterList.isEmpty && valueList.isEmpty) {
          Success(Vector.empty)
        } else if (parameterList.nonEmpty) {
          Failure("Additional parameters not specified " + parameterList.toString)
        } else {
          Failure("To many parameters given: " + valueList.toString)
        }
      }
    }
  }

  // In this case, we want the object to be a type, so we return that type
  // TODO - this repeats a lot of stuff - perhaps combine this with something else?
  def typeSpecificTypeChecker(
    parseTree: ParseTree,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      tc <- typeCheck(parseTree, ExplicitlyTyped(TypeT), env)
      nmt <- Evaluator.convertObjectToType(tc.nObject, env)
    } yield nmt
  }

  sealed abstract class FunctionTypeChecked(val inputType: NewMapType)

  case class StaticTypeFunctionChecked(
    override val inputType: NewMapType,
    outputType: NewMapType
  ) extends FunctionTypeChecked(inputType)

  // This is what happens in a struct, where different inputs produce different outputs
  case class StructTypeFunctionChecked(
    override val inputType: NewMapType,
    params: Vector[(NewMapObject, NewMapObject)]
  ) extends FunctionTypeChecked(inputType)

  // The type in question is purported to be a function (or anything applyable)
  // Therefore, it should have an input type, and an output type
  // If not, there's a problem
  def typeCheckFunctionType(
    functionType: NewMapType,
    functionObject: NewMapObject,
    env: Environment
  ): Outcome[FunctionTypeChecked, String] = {
    functionType match {
      case Index(_) | TypeT | CommandTypeT | IdentifierT | SubstitutableT(_) | Subtype(_, _) | CountT | CaseT(_, _) => {
        Failure("Type " + functionType + " not generally callable.")
      }
      case MapT(
        inputType: NewMapObject,
        outputType: NewMapObject,
        completeness: MapCompleteness,
        featureSet: MapFeatureSet
      ) => {
        for {
          keyT <- Evaluator.convertObjectToType(inputType, env)
          valueT <- Evaluator.convertObjectToType(outputType, env)
        } yield StaticTypeFunctionChecked(keyT, valueT)
      }
      case StructT(fieldType, params) => {
        for {
          paramList <- structParamsIntoParameterList(params, env)
          fieldT <- Evaluator.convertObjectToType(fieldType, env)
        } yield StructTypeFunctionChecked(fieldT, paramList)
      }
    }
  }

  /*
   * Returns true if the values of type nType are themselves types!
   */
  def refersToAType(
    nType: NewMapType,
    env: Environment
  ): Boolean = {
    typeDepth(nType, env) > 0
  }

  /*
   * Returns 0 if the objects in nType are NOT types
   * Returns 1 if they are types
   * Returns 2 if their members are also types
   * Etc...
   *
   * TODO: maybe this should be done in the type object (ie not in the type checker itself)
   */
  def typeDepth(
    nType: NewMapType,
    env: Environment
  ): Long = nType match {
    case Index(_) | CountT => Long.MaxValue
    case TypeT | CommandTypeT => 1
    case IdentifierT => 0
    case StructT(fieldType, params) => {
      // TODO - define a maxByOption
      // TODO(2022) - redo this if not deleted!
      //if (params.isEmpty) 0L else params.map(_._2).map(t => typeDepth(t, env)).max
      0

    }
    case CaseT(_, _) => {
      // TODO - define a minByOption
      // TODO - default should be infinite, not long value
      // TODO(2022) - redo this if not deleted!
      //if (params.isEmpty) Long.MaxValue else params.map(_._2).map(t => typeDepth(t, env)).min
      0
    }
    case MapT(_, _, _, _) => 0
    case SubstitutableT(s: String) => {
      // In the future, we may know that this value is a subtype of type, which will change our answer
      env.typeOf(s) match {
        case Failure(_) => -2 // TODO: this should be an error
        case Success(ExplicitlyTyped(nType)) => {
          val depth = typeDepth(nType, env)
          //if (depth == 0) -1 // TODO: this should also be an error.. this type shouldn't exist
          //else depth - 1
          depth - 1
        }
        case Success(ImplicitlyTyped(types)) => {
          -3 // TODO: Figure out what to do here 
        }
      }
    }
    case Subtype(parentType, func) => {
      Evaluator.convertObjectToType(parentType, env) match {
        case Success(parentT) => typeDepth(parentT, env)
        case _ => {
          -1 // Error?
        }
      }
    }
  }

  def processMultipleFunctionApplications(
    startingFunction: NewMapObjectWithType,
    applications: Vector[ParseTree],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    applications match {
      case (firstApplication +: restOfApplications) => {
        for {
          functionType <- startingFunction.nTypeInfo match {
            case ExplicitlyTyped(nType) => Success(nType)
            case ImplicitlyTyped(convertibles) => {
              if (convertibles.length > 1) {
                // TODO(2022): This can be removed when we ban implicit convertibles entirely!
                Failure(
                  "Implicit function types are not yet supported for multiple convertibles\n" ++
                  startingFunction.nObject.toString
                )
              }

              convertibles.headOption match {
                case None => Failure("Function (" + startingFunction + ") not assigned any type")
                case Some(nType) => Success(nType)
              }
            }
          }

          functionTypeChecked <- typeCheckFunctionType(functionType, startingFunction.nObject, env)
          resultOfFirstApplication <- processFunctionApplication(startingFunction, functionTypeChecked, firstApplication, env)
          result <- processMultipleFunctionApplications(resultOfFirstApplication, restOfApplications, env)
        } yield result
      }
      case _ => {
        Success(startingFunction)
      }
    }
  }

  def processFunctionApplication(
    startingFunction: NewMapObjectWithType,
    functionTypeChecked: FunctionTypeChecked,
    input: ParseTree,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    for {
      // Ensures that the input can be converted to the right type
      successfullyTypeChecked <- typeCheck(
        input,
        ExplicitlyTyped(functionTypeChecked.inputType),
        env
      )

      inputValue = successfullyTypeChecked.nObject

      outputType <- functionTypeChecked match {
        case StaticTypeFunctionChecked(_, outputType) => Success(outputType)
        case StructTypeFunctionChecked(_, params) => {
          params.toMap.get(inputValue) match {
            case Some(param) => Evaluator.convertObjectToType(param, env)
            case None => Failure("Unknown Field: " + inputValue)
          }
        }
      }
    } yield {
      NewMapObjectWithType.withTypeE(
        ApplyFunction(
          startingFunction.nObject,
          inputValue
        ),
        outputType
      )
    }
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObjectWithType, String] = {
    typeCheck(expression, NewMapTypeInfo.init, Environment.Base)
  }
}