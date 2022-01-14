package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be . The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
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
        val ret = resolveTypeInfo(expectedType, env)

        resolveTypeInfo(expectedType, env) match {
          case ExplicitlyTyped(MapT(IdentifierT, TypeT, default)) => {
            val mapT = MapT(IdentifierT, TypeT, default)
            // TODO - do we really want to consider this a parameter list, or just treat it like another map?
            typeCheckParamsStandalone(values, env, mapT, default)
          }
          case ExplicitlyTyped(MapT(keyType, valueType, defaultValue)) => {
            val mapT = MapT(keyType, valueType, defaultValue)
            for {
              mapValues <- typeCheckLiteralMap(values, Some(keyType), Some(valueType), env, patternMatchingAllowed = false)
            } yield {
              NewMapObjectWithType.withTypeE(MapInstance(mapValues, defaultValue), mapT)
            }
          }
          case ExplicitlyTyped(ReqMapT(keyType, valueType)) => {
            val reqMapT = ReqMapT(keyType, valueType)
            for {
              mapValues <- typeCheckLiteralMap(values, Some(keyType), Some(valueType), env, patternMatchingAllowed = true)
              isCovered <- doMapValuesCoverType(mapValues, keyType)
              _ <- Outcome.failWhen(!isCovered, "Incomplete mapping of " + keyType)
            } yield {
              NewMapObjectWithType.withTypeE(ReqMapInstance(mapValues), reqMapT)
            }
          }
          case ExplicitlyTyped(StructT(params: Vector[(String, NewMapType)])) => {
            typeCheckStructStandalone(params, values, env)
          }
          case ExplicitlyTyped(CaseT(params: Vector[(String, NewMapType)])) => {
            if (values.length != 1) {
              Failure("A case may only select 1 value")
            } else {
              val firstValue = values(0)
              firstValue match {
                case BindingCommandItem(valueIdentifier, valueObject) => {
                  val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

                  valueIdOpt match {
                    case Some(valueId) => {
                      params.toMap.get(valueId) match {
                        case Some(nType) => {
                          for {
                            nObjectWithType <- typeCheck(valueObject, ExplicitlyTyped(nType), env)
                          } yield {
                            val valueObject = nObjectWithType.nObject
                            NewMapObjectWithType(CaseInstance(valueId, valueObject), expectedType)
                          }
                        }
                        case _ => {
                          Failure("Identifier " + valueId + " not a member of case class: " + params)
                        }
                      }
                    }
                    case None => {
                      Failure("Expected idenfier: " + valueIdentifier)
                    }
                  }
                }
                case _ => {
                  Failure("Value " + firstValue + " not a member of case class: " + params)
                }
              }
            }
          }
          case ExplicitlyTyped(TypeT) => {
            // Since this commandList is a type, we're going to assume that it's a struct or case description
            // TODO: This should be Index(0) if this is a "Case" - need to figure out that possibility
            val default = Index(1) 
            typeCheckParamsStandalone(values, env, TypeT, default)
          }
          case ExplicitlyTyped(Subtype(parentType)) => {
            for {
              mapValues <- typeCheckLiteralMap(
                values,
                Some(parentType),
                Some(IndexT(2)),
                env,
                // NOTE - it's unclear if pattern matching should be allowed here, since
                // technically this is not a ReqMap (which has patterns), but it seems like
                // patterns would be useful here. Let's revisit
                patternMatchingAllowed = false
              )
            } yield {
              NewMapObjectWithType.withTypeE(
                SubtypeFromMap(ReqMapInstance(mapValues)),
                Subtype(parentType)
              )
            }
          }
          case ImplicitlyTyped(Vector()) => {
            // Assume that it's a reqmap?
            // TODO - maybe there needs to be more logic here
            for {
              mapValues <- typeCheckLiteralMap(values, None, None, env, patternMatchingAllowed = true)
            } yield {
              // TODO: it's untyped, but should accumulate implicit types though typeCheckLiteralMap
              val reqMap = ReqMapInstance(mapValues)
              NewMapObjectWithType(ReqMapInstance(mapValues), ExplicitlyTyped(
                ReqMapT(
                  SubtypeFromMapType(reqMap),
                  SubtypeFromMapType(ReqMapInstance(mapValues.map(x => (x._2 -> Index(1)))))
                )
              ))
            }
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
          newEnv = inputType match {
            case StructT(inputParams) => env.newParams(inputParams)
            case _ => env
          }

          outputType <- typeSpecificTypeChecker(expression, newEnv)
        } yield {
          val typeTransformer = MapInstance(Vector(
            ConvertNewMapTypeToObject(inputType) -> ConvertNewMapTypeToObject(outputType)
          ), Index(0))
          NewMapObjectWithType.withTypeE(LambdaType(typeTransformer), TypeT)
        }
      }
      case LambdaParse(params, expression) => {
        for {
          newParams <- params match {
            case CommandList(values) => typeCheckParameterList(values, env)
            case BindingCommandItem(key, value) => typeCheckParameterList(Vector(params), env)
            case IdentifierParse(id, _) => {
              expectedType match {
                case ExplicitlyTyped(LambdaT(typeTransformer)) => {
                  for {
                    result <- convertTypeTransformerToInputOutput("D -- " + params + " -- " + expression, typeTransformer, env)
                  } yield {
                    Vector(id -> result._1)
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
            case ExplicitlyTyped(LambdaT(typeTransformer)) => {
              for {
                result <- convertTypeTransformerToInputOutput("E ", typeTransformer, env)
              } yield {
                ExplicitlyTyped(result._2)
              }
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
          val lambdaParams: Vector[(String, NewMapObject)] = {
            ConvertNewMapTypeToObject.paramsToObjectParams(newParams)
          }

          NewMapObjectWithType.withTypeE(
            LambdaInstance(StructParams(lambdaParams), tc.nObject),
            Environment.simpleFuncT(StructT(newParams), typeFound)
          )
        }
      }
      case LambdaTransformerParse(expression) => {
        for {
          baseExp <- typeCheck(expression, ExplicitlyTyped(MapT(TypeT, TypeT, Index(0))), env)
        } yield {
          NewMapObjectWithType.withTypeE(LambdaType(baseExp.nObject), TypeT)
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
    case IndexT(i) => {
      Success((0 until i.toInt).map(j => Index(j.toLong)).toSet)
    }
    case SubtypeFromMapType(valuesInType) => {
      Success(valuesInType.values.map(_._2).toSet)
    }
    case _ => {
      Failure("Can't create a ReqMap with keyType " + nType + ". Either this is an infinite type, or the functionality is unimplemented")
    }
  }

  def convertTypeTransformerToInputOutput(
    msg: String,
    typeTransformer: NewMapObject,
    env: Environment
  ): Outcome[(NewMapType, NewMapType), String] = {
    typeTransformer match {
      case MapInstance(values, _) => {
        if (values.length == 1) {
          for {
            inputType <- Evaluator.convertObjectToType(values(0)._1, env)
            outputType <- Evaluator.convertObjectToType(values(0)._2, env)
          } yield (inputType -> outputType)
        } else {
          Failure(msg + " Type Transformer is mapped with multiple types, and this is not implemented yet.")
        }
      }
      case LambdaInstance(paramStrategy, expression) => {
        paramStrategy match {
          case IdentifierParam(id, TypeType) => {
            val inputType = SubstitutableT(id)
            for {
              outputType <- Evaluator.convertObjectToType(expression, env.newParam(id, TypeT))
            } yield (inputType -> outputType)
          }
          case IdentifierParam(id, _) => Failure(msg + " LambdaInstance param strategy not implmented yet: " + paramStrategy)
          case StructParams(params) => {
            // TODO - not type safe!!
            val objParams = params.map(p => p._1 -> Evaluator.convertObjectToType(p._2, env).toOption.get)
            val inputType = StructT(objParams)
            for {
              outputType <- Evaluator.convertObjectToType(expression, env.newParams(objParams))
            } yield (inputType -> outputType)
          }
          case InputStackParam(typeAsObj) => Failure(msg + " LambdaInstance param strategy not implmented yet: " + paramStrategy)
        }
      }
      case LambdaType(tt) => {
        Failure(msg + " You got me!")
      }
      case _ => {
        Failure(msg + " Type Transformers must be a Map Instance or Lambda Instance.")
      }
    }
  }

  // This map could include pattern matching
  def typeCheckLiteralMap(
    values: Vector[ParseTree],
    expectedKeyType: Option[NewMapType],
    expectedValueType: Option[NewMapType],
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        val kType = expectedKeyType.map(e => ExplicitlyTyped(e)).getOrElse(NewMapTypeInfo.init)
        val vType = expectedValueType.map(e => ExplicitlyTyped(e)).getOrElse(NewMapTypeInfo.init)

        for {
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
        // This is the generic pattern
        if (env.lookup(id).nonEmpty) {
          // TODO: rethink what to do if the identifier is already defined in the environement
          Failure(s"Pattern matching clashes with defined variable: $id")
        } else {
          val tcResult = NewMapObjectWithType(ParameterObj(id), expectedType)
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

  val LambdaTransformType = LambdaT(MapInstance(Vector(TypeType -> TypeType), Index(0)))

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

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  def isTypeConvertible(
    startingType: NewMapType,
    endingType: NewMapType,
    env: Environment
  ): Boolean = {
    val resolvedStartingType = resolveType(startingType, env)
    val resolvedEndingType = resolveType(endingType, env)

    (resolvedStartingType, resolvedEndingType) match {
      case (_, TypeT) => refersToAType(resolvedStartingType, env)
      case (IndexT(j), IndexT(i)) => {
        // The indecies must match, even though you can theoretically convert a smaller index to a bigger index.
        // This conversion is not made explicit to prevent people from accessing an array or map with the wrong index value
        (j == i)
      }
      case (SubtypeFromMapType(startingMi), SubtypeFromMapType(endingMi)) => {
        (startingMi.values.forall(v => endingMi.values.exists(_._1 == v._1)))
      }
      case (LambdaT(startingTypeTransformer), LambdaT(endingTypeTransformer)) => {
        val resultOutcome = for {
          startingResult <- convertTypeTransformerToInputOutput("A ", startingTypeTransformer, env)
          endingResult <- convertTypeTransformerToInputOutput("B ", endingTypeTransformer, env)
        } yield {
          val (startingInputType, startingOutputType) = startingResult
          val (endingInputType, endingOutputType) = endingResult
          val isInputTypeConvertible = isTypeConvertible(startingInputType, endingInputType, env)

          // TODO: there could be more cases, and I'm pretty sure this is done elsewhere
          val newEnv = startingInputType match {
            case StructT(params) => env.newParams(params)
            case _ => env
          }

          isInputTypeConvertible && isTypeConvertible(startingOutputType, endingOutputType, newEnv)
        }

        resultOutcome.toOption.getOrElse(false)
      }
      case (SubtypeFromMapType(v), _) => {
        v.values.forall(value => {
          (value._2 == Index(0)) || isRawObjectConvertibleToType(value._1, resolvedEndingType, env).isSuccess
        })
      }
      case _ => {
        (resolvedEndingType == resolvedStartingType) || {
          resolvedStartingType match {
            case StructT(params) if (params.length == 1) => {
              isTypeConvertible(params.head._2, resolvedEndingType, env)
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
      case IndexT(i) => nObject match {
        case Index(j) => if (j < i) success else failMsg("A")
        case _ => failMsg("B")
      }
      case TypeT => {
        for {
          _ <- Evaluator.convertObjectToType(nObject, env)
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
      case MapT(key, value, defaultInType) => nObject match {
        case MapInstance(values, default) => {
          // TODO: we need a helper function here to get the right message
          if (
            (defaultInType == default) && (
              values.forall(x => {
                isRawObjectConvertibleToType(x._1, key, env).isSuccess &&
                isRawObjectConvertibleToType(x._2, value, env).isSuccess
              })
            )
          ) success else failMsg("E")
        }
        case _ => failMsg("F")
      }
      case ReqMapT(key, value) => nObject match {
        case ReqMapInstance(values) => {
          // TODO: we need a helper function here to get the right message
          if (
            (
              values.forall(x => {
                isRawObjectConvertibleToType(x._1, key, env).isSuccess &&
                isRawObjectConvertibleToType(x._2, value, env).isSuccess
              })
            )
          ) success else failMsg("Req E")
        }
        case _ => failMsg("Req F")
      }
      case StructT(params) => nObject match {
        case StructInstance(value) => {
          //////////////////// TODO  not always
          success
        }
        case otherObject => {
          // This could also be the first input to the struct

          params match {
            case firstParam +: otherParams => {
              if (otherParams.length == 0) {
                val firstParamType: NewMapType = firstParam._2
                isRawObjectConvertibleToType(nObject, firstParamType, env)
              } else {
                failMsg("Can't convert non-struct to multiple-valued struct")
              }
            }
            case _ => failMsg("G")
          }
        }
      }
      case CaseT(params) => nObject match {
        case CaseInstance(constructor, input) => {
          params.toMap.get(constructor) match {
            case None => failMsg("No constructor for type: " + constructor)
            case Some(t) => isRawObjectConvertibleToType(input, t, env)
          }
        }
        // TODO: there can be some conversions here; we don't neccesarily need to go to the fail case
        case _ => failMsg("G Case")
      }
      case LambdaT(typeTransformer) => nObject match {
        case LambdaInstance(params, expression) => {
          //////////////////// TODO  not always
          success
        }
        case _ => failMsg("H")
      }
      case SubstitutableT(s) => {
        failMsg("I")
      }
      case Subtype(parent) => nObject match {
        case SubtypeFromMap(values) => {
          //////////////////// TODO  not always
          success
        }
        case _ => failMsg("J")
      }
      case SubtypeFromMapType(mi) => {
        println("***")
        println(mi)
        println(mi.values)
        println(nObject)
        println(nType)
        if (mi.values.exists(_._1 == nObject)) success else failMsg("K")
      }
      case AppliedFunctionT(func, input) => failMsg("M")
      //case MutableT(_, _, _, _) => failMsg("Not implemented: MutableT\n")
    }
  }

  def subsituteType(
    newMapType: NewMapType,
    env: Environment
  ): Outcome[NewMapType, String] = {
    Evaluator.convertObjectToType(
      Evaluator.makeRelevantSubsitutions(ConvertNewMapTypeToObject(newMapType), env),
      env
    )
  }

  // TODO - merge with subsituteType
  def resolveType(
    typeFound: NewMapType,
    env: Environment
  ): NewMapType = {
    // TODO: not type safe
    subsituteType(typeFound, env) match {
      case Success(nType) => nType
      case Failure(reason) => {
        println(reason)
        // TODO: figure out what's going wrong here
        //Thread.dumpStack()
        IndexT(0)
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
    expectedType: NewMapType,
    default: NewMapObject
  ): Outcome[NewMapObjectWithType, String] = for {
    newParams <- typeCheckParameterList(parameterList, env)
  } yield {
    NewMapObjectWithType.withTypeE(ConvertNewMapTypeToObject.paramsToObject(newParams, default), expectedType)
  }

  /*
   * We want to ensure that the struct was created correctly
   */
  def typeCheckStruct(
    parameterList: Vector[(String, NewMapType)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(String, NewMapType, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifier), env)

              substObj = Evaluator.makeRelevantSubsitutions(tc.nObject, env)

              envCommand = Environment.eCommand(paramId, typeOfIdentifier, substObj)
              newEnv = env.newCommand(envCommand)
              result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
            } yield {
              (paramId, typeOfIdentifier, substObj) +: result
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
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifier), env)
          substObj = Evaluator.makeRelevantSubsitutions(tc.nObject, env)
          envCommand = Environment.eCommand(paramId, typeOfIdentifier, substObj)
          newEnv = env.newCommand(envCommand)
          result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
        } yield {
          (paramId, typeOfIdentifier, substObj) +: result
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

  def typeCheckStructStandalone(
    parameterList: Vector[(String, NewMapType)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = for {
    envCommands <- typeCheckStruct(parameterList, valueList, env)
  } yield {
    val typeFound = StructT(envCommands.map(c => c._1 -> c._2))
    val objectFound = StructInstance(envCommands.map(c => c._1 -> c._3))
    NewMapObjectWithType.withTypeE(objectFound, typeFound)
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
  case class DynamicTypeFunctionChecked(
    override val inputType: NewMapType,
    params: Vector[(NewMapObject, NewMapObject)],
    paramsDefault: NewMapObject
  ) extends FunctionTypeChecked(inputType)

  // TODO: there should be a case here for overloaded functions (accepts several different types and returns a type based on the input type)

  // The type in question is purported to be a function (or anything applyable)
  // Therefore, it should have an input type, and an output type
  // If not, there's a problem
  def typeCheckFunctionType(
    functionType: NewMapType,
    functionObject: NewMapObject,
    env: Environment
  ): Outcome[FunctionTypeChecked, String] = {
    functionType match {
      case IndexT(_) | TypeT | IdentifierT | SubstitutableT(_) | Subtype(_) | SubtypeFromMapType(_) | CountT  => {
        Failure("Type " + functionType + " not generally callable.")
      }
      case MapT(key: NewMapType, value: NewMapType, default: NewMapObject) => {
        Success(StaticTypeFunctionChecked(key, value))
      }
      case ReqMapT(key: NewMapType, value: NewMapType) => {
        Success(StaticTypeFunctionChecked(key, value))
      }
      case StructT(params: Vector[(String, NewMapType)]) => {
        // TODO: this is really complex.. hopefully we can simplify this whole process
        val dynamicParams: Vector[(NewMapObject, NewMapObject)] = functionObject match {
          case StructInstance(values) => {
            var result: Vector[(NewMapObject, NewMapObject)] = Vector.empty
            var newEnv: Environment = env

            for (i <- (0 until params.length)) {
              val (id, nType) = params(i)
              val (oid, nObj) = values(i)

              val substType = Evaluator.makeRelevantSubsitutions(ConvertNewMapTypeToObject(nType), newEnv)

              result :+= (IdentifierInstance(id), substType)

              // TODO - not type safe!!!
              val substTypeAsType = Evaluator.convertObjectToType(substType, env).toOption.get
              val substObj = Evaluator.makeRelevantSubsitutions(nObj, newEnv)

              val newCommand = Environment.eCommand(id, substTypeAsType, substObj)
              newEnv = newEnv.newCommand(newCommand)
            }

            result
          }
          case _ => ConvertNewMapTypeToObject.paramsToObject(params, Index(1)).values
        }

        Success(DynamicTypeFunctionChecked(
          // TODO - this first param should really be SubtypeFromMapType(ConvertNewMapTypeToObject.paramsToObject(params))
          //  However, the identifier currently can't be recognized as that type, even if it's a param.
          //  So once that's fixed, this can be changed over 
          IdentifierT,
          dynamicParams,
          Index(1)
        ))
      }
      case CaseT(params: Vector[(String, NewMapType)]) => {
        Failure("Case types are not functions: " + functionType)      
      }
      case LambdaT(typeTransformer: NewMapObject) => {
        for {
          result <- convertTypeTransformerToInputOutput("C ", typeTransformer, env)
        } yield {
          // TODO: obviously this should go dynamic with the type transformer
          StaticTypeFunctionChecked(result._1, result._2)
        }
      }
      case AppliedFunctionT(func, input) => {
        Failure("Unimplemented: finding the function type from an already applied function")
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
    case IndexT(i) => Long.MaxValue
    case TypeT => 1
    case IdentifierT => 0
    case StructT(params) => {
      // TODO - define a maxByOption
      if (params.isEmpty) 0L else params.map(_._2).map(t => typeDepth(t, env)).max
    }
    case CaseT(params) => {
      // TODO - define a minByOption
      // TODO - default should be infinite, not long value
      if (params.isEmpty) Long.MaxValue else params.map(_._2).map(t => typeDepth(t, env)).min
    }
    case MapT(key, value, default) => {
      if (isTypeConvertible(key, IdentifierT, env) && isTypeConvertible(value, TypeT, env)) {
        // Index(0) indicates a case, and Index(1) indicates a struct. Both are in IndexT(2)
        if (isRawObjectConvertibleToType(default, IndexT(2), env).isSuccess) 1 else 0
      } else {
        0
      }
    }
    case ReqMapT(key, value) => 0
    case LambdaT(_) => 0
    case SubstitutableT(s: String) => {
      env.typeOf(s) match {
        case Failure(_) => -2 // TODO: this should be an error
        case Success(ExplicitlyTyped(nType)) => {
          val depth = typeDepth(nType, env)
          if (depth == 0) -1 // TODO: this should also be an error.. this type shouldn't exist
          else depth - 1
          depth - 1
        }
        case Success(ImplicitlyTyped(types)) => {
          -3 // TODO: Figure out what to do here 
        }
      }
    }
    case Subtype(t: NewMapType) => {
      val baseTypeDef = typeDepth(t, env)
      // TODO: this logic might be faulty
      // This weird MaxValue test is due to the fact that we need a true infinite here. Working on it!
      if (baseTypeDef == Long.MaxValue) Long.MaxValue
      else typeDepth(t, env) + 1
    }
    case SubtypeFromMapType(baseMap: ReqMapInstance) => {
      1 // TODO: this is fishy, make a test
    }
    case CountT => Long.MaxValue // TODO: should be infinite
    case AppliedFunctionT(func, input) => {
      // TODO: this can be more than 0
      0
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
      typeCheckFunctionInputResult <- typeCheckFunctionInput(
        input,
        functionTypeChecked,
        env
      )

      successfullyTypeChecked = typeCheckFunctionInputResult.newMapObjectWithType

      inputValue = successfullyTypeChecked.nObject

      fullOutputType <- functionTypeChecked match {
        case StaticTypeFunctionChecked(_, outputType) => Success(outputType)
        case DynamicTypeFunctionChecked(_, params, paramsDefault) => {
          dynamicTyping(inputValue, params, paramsDefault, env)
        }
      }

      // We're either peeling away one parameter, or we're going to the full output
      outputType <- if (typeCheckFunctionInputResult.firstParamOnly) {
        functionTypeChecked.inputType match {
          case StructT(params) => {
            if (params.length == 1) Success(fullOutputType) else {
              var newEnv = env.newCommand(FullEnvironmentCommand(params(0)._1, successfullyTypeChecked))

              var newParams: Vector[(String, NewMapType)] = Vector.empty

              params.drop(1).foreach(param => {
                newParams :+= (param._1 -> resolveType(param._2, newEnv))
                newEnv = newEnv.newParam(param._1, resolveType(param._2, newEnv))
              })

              for {
                newOutputType <- subsituteType(fullOutputType, newEnv)
              } yield {
                val typeTransformer = MapInstance(Vector(
                  ConvertNewMapTypeToObject(StructT(newParams)) -> ConvertNewMapTypeToObject(newOutputType)
                ), Index(0))
                LambdaT(typeTransformer)
              }
            }
          }
          // TODO: refactor to avoid this case?
          case _ => Failure("This shouldn't happen")
        }
      } else Success(fullOutputType)

      nObjectWithType = NewMapObjectWithType.withTypeE(
        ApplyFunction(
          startingFunction.nObject,
          inputValue
        ),
        outputType
      )

      result <- Evaluator(nObjectWithType, env)
    } yield NewMapObjectWithType.withTypeE(result, outputType)
  }

  def dynamicTyping(
    inputValue: NewMapObject,
    params: Vector[(NewMapObject, NewMapObject)],
    paramsDefault: NewMapObject,
    env: Environment
  ): Outcome[NewMapType, String] = params match {
    case firstParam +: restOfParams => {
      if (firstParam._1 == inputValue) {
        Evaluator.convertObjectToType(firstParam._2, env)
      } else {
        for {
          firstParamType <- Evaluator.convertObjectToType(firstParam._2, env)

          newEnv = firstParam._1 match {
            case IdentifierInstance(id) => env.newParam(id, firstParamType)
            case _ => {
              // TODO: I think this is unusable right now, check if we can get rid of this.
              env
            }
          }

          result <- dynamicTyping(inputValue, restOfParams, paramsDefault, newEnv)
        } yield result
        
      }
    }
    case _ => {
      Evaluator.convertObjectToType(paramsDefault, env)
    }
  }

  case class TypeCheckFunctionInputResult(
    newMapObjectWithType: NewMapObjectWithType,
    firstParamOnly: Boolean
  )

  def typeCheckFunctionInput(
    input: ParseTree,
    functionTypeChecked: FunctionTypeChecked,
    env: Environment
  ): Outcome[TypeCheckFunctionInputResult, String] = {
    typeCheck(
      input,
      ExplicitlyTyped(functionTypeChecked.inputType),
      env
    ) match {
      case Success(nmo) => Success(TypeCheckFunctionInputResult(nmo, false))
      case Failure(msg) => {
        // Now we have a second try, because the input might be the first value of an input struct
        functionTypeChecked.inputType match {
          case StructT(params) => {
            // TODO: type unsafe until we enforce at least one param in scala
            val firstParam = params.head

            typeCheck(input, ExplicitlyTyped(firstParam._2), env) match {
              case Success(nmo) => Success(TypeCheckFunctionInputResult(nmo, true))
              case Failure(msg2) => {
                val introStr = "Could not interpret input " + input + " as valid for function explicitly typed as " + firstParam._2 + "."
                val err1 = "Error for direct approach: " + msg
                val err2 = "Error for trying to match it to the first parameter: " + msg2
                Failure(introStr + "\n" + err1 + "\n" + err2)
              }
            }
          }
          case _ => Failure(msg)
        }
      }
    }
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObjectWithType, String] = {
    typeCheck(expression, NewMapTypeInfo.init, Environment.Base)
  }
}