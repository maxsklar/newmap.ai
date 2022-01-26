package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be.
   *   The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: Option[NewMapSubtype],
    env: Environment
  ): Outcome[NewMapObject, String] = {
    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    val result = expression match {
      case NaturalNumberParse(i: Long) => {
        expectedType match {
          case Some(expType) => {
            RetrieveType.getParentType(expType) match {
              case CountT => {
                val proposedObject = Index(i)
                checkProposedObjectInSubtype(proposedObject, expType, env)
              }
              case TypeT(0) => {
                val proposedObject = NewMapO.rangeT(i)
                checkProposedObjectInSubtype(proposedObject, expType, env)
              }
              case _ => Failure(s"Can't turn number $i into type $expType")
            }
          }
          case None => Success(Index(i))
        }
      }
      case IdentifierParse(s: String, true) => Success(IdentifierInstance(s))
      case IdentifierParse(s: String, false) => {
        // TODO: Make sure that we are getting the expected type
        env.lookup(s) match {
          case Some(nObject) => Success(nObject)
          case None => Success(IdentifierInstance(s))
        }
      }
      case ApplyParse(startingFunction: ParseTree, application: ParseTree) => {
        for {
          // TODO: create a typeCheckFunction to handle this specifically
          functionTypeChecked <- typeCheck(
            startingFunction,
            None,
            env
          )

          result <- functionTypeChecked match {
            case nAppliable: NewMapFunction => processFunctionApplication(nAppliable, application, env)
            case _ => Failure(s"Object $functionTypeChecked not generally callable.")
          }
        } yield result
      }
      case CommandList(values: Vector[ParseTree]) => {
        expectedType.map(resolveType(_, env)) match {
          case Some(MapT(IdentifierT, TypeT(0), completeness, featureSet)) => {
            val mapT = MapT(IdentifierT, TypeT(0), completeness, featureSet)
            // TODO - do we really want to consider this a parameter list, or just treat it like another map?
            typeCheckParamsStandalone(values, env, mapT)
          }
          case Some(mapT@MapT(keyTypeT, valueT, completeness, featureSet)) => {
            val mapT = MapT(keyTypeT, valueT, completeness, featureSet)
            for {
              mapValues <- typeCheckLiteralMap(
                values,
                keyTypeT,
                valueT,
                env,
                patternMatchingAllowed = featureSet != BasicMap
              )

              // TODO(2022): typeCheckLiteralMap looks at the feature set, perhaps it should also handle this!
              isCovered <- {
                if (completeness != RequireCompleteness) Success(true)
                else {
                  doMapValuesCoverType(mapValues, keyTypeT)
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + keyTypeT
              )
            } yield {
              MapInstance(mapValues, mapT)
            }
          }
          case Some(StructT(params)) => {
            for {
              parameterList <- structParamsIntoParameterList(params, env)
              result <- typeCheckStruct(parameterList, values, env)
            } yield {
              StructInstance(result, StructT(params))
            }
          }
          case Some(caseT@CaseT(MapInstance(caseToType, _))) => {
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
                            valueObject <- typeCheck(valueObject, Some(nTypeT), env)
                          } yield {
                            CaseInstance(IdentifierInstance(valueId), valueObject, caseT)
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
          case Some(TypeT(0)) => {
            typeCheckParamsStandalone(values, env, TypeT(0))
          }
          case None => {
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
      case LambdaParse(params, expression) if (expectedType == Some(TypeT(0))) => {
        for {
          inputType <- typeSpecificTypeChecker(params, env)
          newEnv <- inputType match {
            case StructT(params) => {
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
          MapT(
            inputType,
            outputType,
            completeness = RequireCompleteness,
            featureSet = FullFunction
          )
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
                case Some(MapT(inputT, outputType, completeness, featureSet)) => {
                  for {
                    _ <- Outcome.failWhen(featureSet == BasicMap, ErrorMessageForBasicMap)
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

          // Calculate the expected output type of the expression
          expectedExpressionTypeOpt <- expectedType match {
            case Some(MapT(inputType, outputT, completeness, featureSet)) => {
              for {
                _ <- Outcome.failWhen(featureSet == BasicMap, ErrorMessageForBasicMap)
              } yield Some(outputT)
            }
            case _ => Success(None)
          }

          tc <- typeCheck(expression, expectedExpressionTypeOpt, env.newParams(newParams))
        } yield {
          val newParamsAsObj = newParams.map(x => IdentifierInstance(x._1) -> x._2)

          val fieldType = {
            SubtypeT(
              MapInstance(
                newParamsAsObj.map(x => x._1 -> Index(1)),
                MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
              )
            )
          }
          
          LambdaInstance(StructParams(newParams), tc),
        }
      }
    }

    for {
      nObject <- result

      // TODO: figure out what to really do here
      // TODO - perhaps if we were doing this all along with the parse analysis, we wouldn't need to do this.
      // Check that the object is part of the required subtype if given
      // Is it possible that this will always be true given the code above?
      // Once we can verify this, maybe we can remove this code
      _ <- expectedType match {
        case Some(typeExpected) => {
          val nType = RetrieveType(nObject)
          if (isTypeConvertible(nType, typeExpected, env)) Success()
          else Failure(s"In expression $nObject expected type $typeExpected found type $nType and cannot convert")
        }
        case None => Success()
      }
    } yield nObject
  }

  def checkProposedObjectInSubtype(
    proposedObject: NewMapObject,
    expectedType: NewMapSubtype,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    for {
      isMember <- Evaluator.isMemberOfSubtype(proposedObject, expectedType, env)
      _ <- Outcome.failWhen(!isMember, s"Proposed object $proposedObject is not in subtype $expectedType")
    } yield proposedObject
  }

  def convertParamsObjectToType(
    params: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapSubtype)], String] = {
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

  // Check that the function doesn't return the default value for any input
  def doesFunctionCoverParentType(
    nFunction: NewMapFunction
  ): Outcome[Boolean, String] = {
    nFunction match {
      case MapInstance(values, MapT(inputType, _, CommandOutput, BasicMap)) => {
        doMapValuesCoverType(values, inputType)
      }
      case _ => Success(false)
    }
  }

  // For ReqMaps, we need to ensure that all of the values are accounted for.
  def doMapValuesCoverType(
    values: Vector[(NewMapObject, NewMapObject)],
    nType: NewMapSubtype
  ): Outcome[Boolean, String] = {
    val keys = values.map(_._1).toSet

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    val genericPatternExists = keys.exists(key => key match {
      case ParameterObj(_, _) => true
      case _ => false
    })

    if (genericPatternExists) Success(true)
    else {
      for {
        keysToMatch <- enumerateAllValuesIfPossible(nType)
      } yield {
        (keysToMatch -- keys).isEmpty && (keys -- keysToMatch).isEmpty
      }
    }
  }

  def enumerateAllValuesIfPossible(nType: NewMapSubtype): Outcome[Set[NewMapObject], String] = {
    nType match {
      // TODO: What if values is too large? Should we make some restrictions here?
      case SubtypeT(MapInstance(values, _)) => Success(values.map(_._1).toSet)
      // TODO(2022): if simpleFunction is boolean function on a small finite parentType, we should be able to enumerate those
      // TODO(2022): this is also one of those advanced cases where we want to know if one set is a subset of another through some advanced argument (like monotonicity)
      case SubtypeT(RangeFunc(i))  => Success((0 until i.toInt).map(j => Index(j.toLong)).toSet)
      // TODO - structs and cases
      case _ => Failure(s"Can't enumerate the allowed values of $nType -- could be unimplemented")
    }
  }

  // This map could include pattern matching
  def typeCheckLiteralMap(
    values: Vector[ParseTree],
    keyType: NewMapSubtype,
    valueType: NewMapSubtype,
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, patternMatchingAllowed)
          objectFoundKey = resultKey.typeCheckResult

          objectFoundValue <- typeCheck(v, Some(valueType), resultKey.newEnvironment)

          restOfMap <- typeCheckLiteralMap(
            restOfValues,
            keyType,
            valueType,
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
    typeCheckResult: NewMapObject,
    newEnvironment: Environment
  )

  // TODO(2022): This should just be a MatchInstance - redo this and really make the language powerful!
  // TODO: This should be integrated into the regular type-check script.
  // we just need a way to know whether we are in an area where pattern matching is allowed or not!
  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapSubtype,
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    expression match {
      case IdentifierParse(id, false) if (patternMatchingAllowed) => {
        val expectingIdentifier = expectedType == IdentifierT

        if (!expectingIdentifier && env.lookup(id).nonEmpty) { // This is the generic pattern
          // TODO: rethink what to do if the identifier is already defined in the environement
          Failure(s"Pattern matching clashes with defined variable: $id")
        } else {
          val nObject = if (expectingIdentifier) IdentifierInstance(id) else ParameterObj(id, expectedType)
          val envCommand = FullEnvironmentCommand(id, ParameterObj(id, expectedType))
          val newEnv = env.newCommand(envCommand)

          Success(TypeCheckWithPatternMatchingResult(nObject, newEnv))
        }
      }
      case _ => {
        // No patterns matched, fall back to regular type checking
        for {
          tc <- typeCheck(expression, Some(expectedType), env)
        } yield {
          TypeCheckWithPatternMatchingResult(tc, env)
        }
      }
    }
  }

  // TODO: This can be made to work in much broader circumstances
  // - For example, if the endingType excludes only a finite amount of objects, then we can check
  //    to make sure that all of those are not in startingType
  def isTypeConvertible(
    startingType: NewMapSubtype,
    endingType: NewMapSubtype,
    env: Environment
  ): Boolean = {
    if (isPureTypeConvertible(startingType, endingType, env)) {
      val doesEndtypeCoverParentType = endingType match {
        case SubtypeT(isMember) => doesFunctionCoverParentType(isMember).toOption.getOrElse(false)
        case _ => true
      }

      if (doesEndtypeCoverParentType) {
        true
      } else {
        // End type does not cover parent type
        // So, let's go through all the values of starting type (if we can) and see if we can brute force it
        val allConvertedOutcome = for {
          allValues <- enumerateAllValuesIfPossible(startingType)
          doAllConvert <- Evaluator.allMembersOfSubtype(allValues.toVector, endingType, env)
        } yield doAllConvert

        allConvertedOutcome.toOption.getOrElse(false)
      }
    } else false
  }

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  // ALSO: this is for automatic conversion. There should be another conversion, which is a superset of this, which is less automatic
  //  - To be used in cases where you want the programmer to specifically ask for a conversion!
  def isPureTypeConvertible(
    startingType: NewMapSubtype,
    endingType: NewMapSubtype,
    env: Environment
  ): Boolean = {
    // TODO? Check that types are of the same depth
    val resolvedStartingType = resolveType(startingType, env)
    val resolvedEndingType = resolveType(endingType, env)

    (resolvedStartingType, resolvedEndingType) match {
      case (TypeT(i), TypeT(j)) => i == j
      case (CountT, CountT) => true
      case (
        MapT(startingInputType, startingOutputType, startingCompleteness, startingFeatureSet),
        MapT(endingInputType, endingOutputType, endingCompleteness, endingFeatureSet)
      ) => {
        val isFeatureSetConvertible = startingFeatureSet match {
          case BasicMap => true
          case SimpleFunction => (endingFeatureSet != BasicMap)
          case FullFunction => (endingFeatureSet == FullFunction)
        }

        // TODO: We may be able to convert between different completeness types, but add this as needed
        val isMapCompletenessConvertible = (startingCompleteness == endingCompleteness)

        // Note: the input type is CONTRAvariant, the output type is COvariant, hence the reversal
        // Eventually, we'll have to implement covariance in generic types
        isTypeConvertible(endingInputType, startingInputType, env) &&
          isTypeConvertible(startingOutputType, endingOutputType, env) &&
          isFeatureSetConvertible &&
          isMapCompletenessConvertible
      }
      case(
        StructT(startingParams),
        StructT(endingParams)
      ) => {
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(startingParams),
          RetrieveType.retrieveInputTypeFromFunction(endingParams),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (StructT(MapInstance(values, mi)), _) if (values.length == 1) => {
        Evaluator.convertObjectToType(values.head._2, env)
          .toOption
          .map(x => isTypeConvertible(x, resolvedEndingType, env))
          .getOrElse(false)
      }
      case (CaseT(startingCases), CaseT(endingCases)) => {
        // Note the contravariance (ending cases first)
        // This is because a case class with fewer cases can be converted into one with more
        isTypeConvertible(
          RetrieveType.retrieveInputTypeFromFunction(endingCases),
          RetrieveType.retrieveInputTypeFromFunction(startingCases),
          env
        )
        // TODO: The outputs have to agree as well
      }
      case (CaseT(startingCases), _) => {
        //Check to see if this is a singleton case, if so, see if that's convertible into the other
        false
      }
      case _ => {
        (resolvedEndingType == resolvedStartingType)
      }
    }
  }

  // TODO - do we need a duplicate of this in the evaluator?
  // Perhaps make a separate Substitutor
  def resolveType(
    typeFound: NewMapSubtype,
    env: Environment
  ): NewMapSubtype = Evaluator.makeRelevantSubstitutionsOfType(typeFound, env)

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
        env.lookup(name) match {
          case Some(ParameterObj(name, _)) => FoundIdentifier(name)
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
  ): Outcome[Vector[(String, NewMapSubtype)], String] = {
    parameterList match {
      case BindingCommandItem(identifier, typeOfIdentifier) +: otherIdentifiers => {
        checkForIdentifier(identifier, env) match {
          case NotAnIdentifier => Failure("Expected Identifier: " + identifier.toString)
          case FoundIdentifier(name) => {
            // Now we have the variable.. next step we need the type
            // TODO - we need a type check that's specific to a type
            for {
              _ <- typeCheck(typeOfIdentifier, Some(TypeT(0)), env)
              typeOfIdentifierAsType <- typeSpecificTypeChecker(typeOfIdentifier, env)
              expandedEnv = env.newParam(name, typeOfIdentifierAsType)
              restOfParams <- typeCheckParameterList(otherIdentifiers, expandedEnv)
            } yield {
              (name -> typeOfIdentifierAsType) +: restOfParams
            }
          }
          case FoundIdentifierUnknownValue => {
            for {
              _ <- typeCheck(typeOfIdentifier, Some(TypeT(0)), env)
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
    expectedType: NewMapSubtype
  ): Outcome[NewMapObject, String] = for {
    newParams <- typeCheckParameterList(parameterList, env)
  } yield {
    paramsToObject(newParams)
  }

  def paramsToObject(
    params: Vector[(String, NewMapSubtype)]
  ): MapInstance = {
    val paramsAsObjects: Vector[(NewMapObject, NewMapObject)] = for {
      (name, nmt) <- params
    } yield {
      IdentifierInstance(name) -> nmt
    }

    val fieldType = {
      SubtypeT(
        MapInstance(
          paramsAsObjects.map(x => x._1 -> Index(1)),
          MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
        )
      )
    }

    MapInstance(paramsAsObjects, MapT(fieldType, TypeT(0), RequireCompleteness, BasicMap))
  }

  // Assume that params is already type checked
  // We just want to concrete list of fields
  def structParamsIntoParameterList(
    params: NewMapObject,
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    params match {
      case MapInstance(values, _) => Success(values)
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
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(String, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((IdentifierInstance(paramId), typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              typeOfIdentifierT <- Evaluator.convertObjectToType(typeOfIdentifier, env)
              tc <- typeCheck(valueObject, Some(typeOfIdentifierT), env)

              substObj = Evaluator.makeRelevantSubstitutions(tc, env)

              envCommand = Environment.eCommand(paramId, substObj)
              newEnv = env.newCommand(envCommand)
              result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
            } yield {
              (paramId, substObj) +: result
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
          tc <- typeCheck(valueObject, Some(typeOfIdentifierT), env)
          substObj = Evaluator.makeRelevantSubstitutions(tc, env)
          envCommand = Environment.eCommand(paramId, substObj)
          newEnv = env.newCommand(envCommand)
          result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
        } yield {
          (paramId, substObj) +: result
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
  // TODO(2022): This is it's own class!
  def typeSpecificTypeChecker(
    parseTree: ParseTree,
    env: Environment
  ): Outcome[NewMapSubtype, String] = {
    // Try looking for a couple of levels of types
    // TODO: need to do this in a principled way!
    // var x: Type = ...
    // should be abolished in favor of
    // type x = ...
    // - and then a level can be specified as well if not 0

    val typeCheckOfType = typeCheck(parseTree, Some(TypeT(0)), env) match {
      case Success(nObject) => Success(nObject)
      case _ => typeCheck(parseTree, Some(TypeT(1)), env)
    }

    for {
      tc <- typeCheckOfType
      nmt <- Evaluator.convertObjectToType(tc, env)
    } yield nmt
  }

  def processFunctionApplication(
    startingFunction: NewMapFunction,
    input: ParseTree,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    for {
      // Ensures that the input can be converted to the right type
      inputValue <- typeCheck(
        input,
        Some(RetrieveType.retrieveInputTypeFromFunction(startingFunction)),
        env
      )
    } yield {
      ApplyFunction(startingFunction, inputValue)
    }
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObject, String] = {
    typeCheck(expression, None, Environment.Base)
  }
}