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
            // TODO: this is so awkard! Fix
            RetrieveType.getParentType(expType) match {
              case TypeT => {
                val proposedObject = NewMapO.rangeT(i)
                SubtypeUtils.checkProposedObjectInSubtype(proposedObject, expType, env)
              }
              case _ => {
                val proposedObject = Index(i)
                SubtypeUtils.checkProposedObjectInSubtype(proposedObject, expType, env)
              }
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
      case ApplyParse(startingFunction: ParseTree, input: ParseTree) => {
        for {
          // TODO: create a typeCheckFunction to handle this specifically
          functionTypeChecked <- typeCheck(
            startingFunction,
            None,
            env
          )

          inputType = RetrieveType.retrieveInputTypeFromFunction(functionTypeChecked)

          inputValue <- typeCheck(
            input,
            Some(inputType),
            env
          )
        } yield ApplyFunction(functionTypeChecked, inputValue)
      }
      case FieldAccessParse(struct, field) => {
        for {
          // TODO - build a specialize typeCheck where you expect a struct?
          typeCheckedStruct <- typeCheck(struct, None, env)

          structParams <- typeCheckedStruct match {
            case StructInstance(value, StructT(params)) => Success(params)
            case CaseT(cases) => Success(cases)
            case _ => {
              Failure(s"Attempting to access field object $typeCheckedStruct which is not a struct instance")
            }
          }

          fieldsT = RetrieveType.retrieveInputTypeFromFunction(structParams)

          typeCheckedField <- typeCheck(field, Some(fieldsT), env)
          
          _ <- Outcome.failWhen(
            !RetrieveType.isTermClosedLiteral(typeCheckedField),
            s"When accessing the value of a struct, the field must not have any unbound variables or non-basic functions. This field is $typeCheckedField"
          )

          // Field must be fully evaluated
          evaluatedField <- Evaluator(typeCheckedField, env)

          // Make sure that this field actually exists
          resultingType <- Evaluator.quickApplyFunctionAttempt(structParams, evaluatedField, env)
        } yield AccessField(typeCheckedStruct, evaluatedField)
      }
      case CommandList(values: Vector[ParseTree]) => {
        expectedType.map(e => {
          val substType = MakeSubstitution.makeRelevantSubstitutionsOfType(e, env)
          RetrieveType.getParentType(substType)
        }) match {
          case Some(mapT@MapT(keyTypeT, valueT, completeness, featureSet)) => {
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
                  SubtypeUtils.doMapValuesCoverType(mapValues, keyTypeT)
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
          case Some(TypeT) => {
            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // TODO - this can be simplified by combining with the MapT section above

            for {
              mapValues <- typeCheckLiteralMap(
                values,
                IdentifierT,
                TypeT,
                env,
                patternMatchingAllowed = false
              )
            } yield {
              val fieldType = {
                SubtypeT(
                  MapInstance(
                    mapValues.map(x => x._1 -> Index(1)),
                    MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
                  )
                )
              }

              StructT(
                MapInstance(
                  mapValues,
                  MapT(fieldType, TypeT, RequireCompleteness, BasicMap)
                )
              )
            }
          }
          case None => {
            // Steps to implement this:
            // type check all the elements of the map
            // Find a common type. DONT use Any - they should be in the same construction
            // Build a ReqMap from this
            Failure("CommandLists must be explicitly typed")
          }
          case _ => {
            Failure(s"CommandLists not working yet with this expected type: $values exp: $expectedType")
          }
        }
      }
      case BindingCommandItem(key, value) => {
        typeCheck(CommandList(Vector(expression)), expectedType, env)
      }
      case LambdaParse(params, expression) if (expectedType == Some(TypeT)) => {
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
                    Vector(IdentifierInstance(id) -> inputT)
                  }
                }
                case _ => {
                  // TODO - implement
                  //Vector(IdentifierInstance(id) -> ??)
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
          val fieldType = {
            SubtypeT(
              MapInstance(
                newParams.map(x => x._1 -> Index(1)),
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
      nObjectConverted <- expectedType match {
        case Some(typeExpected) => SubtypeUtils.attemptToConvertToType(nObject, typeExpected, env)
        case None => Success(nObject)
      }
    } yield nObjectConverted
  }

  def convertParamsObjectToType(
    params: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapSubtype)], String] = {
    params match {
      case (id, nmObject) +: restOfParams => {
        for {
          restConverted <- convertParamsObjectToType(restOfParams, env)
          nmType <- Evaluator.convertObjectToType(nmObject, env)
        } yield {
          (id, nmType) +: restConverted
        }
      }
      case _ => Success(Vector.empty)
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
          val envCommand = FullEnvironmentCommand(IdentifierInstance(id), ParameterObj(id, expectedType))
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

  def typeCheckParameterList(
    parameterList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapSubtype)], String] = {
    parameterList match {
      case BindingCommandItem(identifier, typeOfIdentifier) +: otherIdentifiers => {
        checkForIdentifier(identifier, env) match {
          case NotAnIdentifier => Failure("Expected Identifier: " + identifier.toString)
          case FoundIdentifier(name) => {
            // Now we have the variable.. next step we need the type
            // TODO - we need a type check that's specific to a type
            for {
              typeOfIdentifierAsType <- typeSpecificTypeChecker(typeOfIdentifier, env)
              expandedEnv = env.newParam(name, typeOfIdentifierAsType)
              restOfParams <- typeCheckParameterList(otherIdentifiers, expandedEnv)
            } yield {
              (IdentifierInstance(name) -> typeOfIdentifierAsType) +: restOfParams
            }
          }
          case FoundIdentifierUnknownValue => {
            for {
              _ <- typeCheck(typeOfIdentifier, Some(TypeT), env)
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
    params: Vector[(NewMapObject, NewMapSubtype)]
  ): MapInstance = {
    val paramsAsObjects: Vector[(NewMapObject, NewMapObject)] = for {
      (name, nmt) <- params
    } yield {
      name -> nmt
    }

    val fieldType = {
      SubtypeT(
        MapInstance(
          paramsAsObjects.map(x => x._1 -> Index(1)),
          MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
        )
      )
    }

    MapInstance(paramsAsObjects, MapT(fieldType, TypeT, RequireCompleteness, BasicMap))
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
   * TODO(2022): This should no longer be the struct checking function because it manipulates the environment
   * - We use this feature, though, so we need to wait until it's separated out
   */
  def typeCheckStruct(
    parameterList: Vector[(NewMapObject, NewMapObject)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForIdentifier(valueIdentifier, env) match {
          case FoundIdentifier(name) => Some(IdentifierInstance(name))
          case _ => None
        }

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              typeOfIdentifierT <- Evaluator.convertObjectToType(typeOfIdentifier, env)
              tc <- typeCheck(valueObject, Some(typeOfIdentifierT), env)

              substObj = MakeSubstitution(tc, env)

              envCommand = FullEnvironmentCommand(paramId, substObj)
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
            Failure("Id not found for " + paramId)
          }
        }
      }      
      case (((IdentifierInstance(paramId), typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          typeOfIdentifierT <- Evaluator.convertObjectToType(typeOfIdentifier, env)
          tc <- typeCheck(valueObject, Some(typeOfIdentifierT), env)
          substObj = MakeSubstitution(tc, env)
          envCommand = Environment.eCommand(paramId, substObj)
          newEnv = env.newCommand(envCommand)
          result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
        } yield {
          (IdentifierInstance(paramId), substObj) +: result
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
  // Note that we want to result EVALUATED - otherwise we don't actually know if it's a type
  def typeSpecificTypeChecker(
    parseTree: ParseTree,
    env: Environment
  ): Outcome[NewMapSubtype, String] = {
    for {
      tc <- typeCheck(parseTree, Some(TypeT), env)
      evaluatedTc <- Evaluator(tc, env)
      nmt <- Evaluator.convertObjectToType(evaluatedTc, env)
    } yield nmt
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObject, String] = {
    typeCheck(expression, None, Environment.Base)
  }
}