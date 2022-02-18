package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be.
   *   The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is the environment of values upon which we are working
   * @param inPattern This tells us if this expression could be a piece of pattern matching, where there are different rules on unbound variables
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: NewMapObject,
    env: Environment,
    inPattern: Boolean = false
  ): Outcome[NewMapObject, String] = {
    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    val result = expression match {
      case NaturalNumberParse(i: Long) => {
        // TODO: this is so awkard! Fix
        // - perhaps we can combine the 2 concepts
        RetrieveType.getParentType(expectedType) match {
          case TypeT => {
            val proposedObject = NewMapO.rangeT(i)
            SubtypeUtils.checkProposedObjectInSubtype(proposedObject, expectedType, env)
          }
          case _ => {
            val proposedObject = Index(i)
            SubtypeUtils.checkProposedObjectInSubtype(proposedObject, expectedType, env)
          }
        }
      }
      case IdentifierParse(s: String, true) => Success(IdentifierInstance(s))
      case IdentifierParse(s: String, false) => {
        val expectingAnIdentifier = {
          SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env).toOption.getOrElse(false)
        }

        if (expectingAnIdentifier) {
          Success(IdentifierInstance(s))
        } else if (inPattern) {
          Success(IdentifierPattern(s, expectedType))
        } else env.lookup(s) match {
          case Some(nObject) => Success(nObject)
          case None => {
            Failure(s"Identifier $s is unknown $expectedType --- ${SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env)}")
          }
        }
      }
      case ApplyParse(startingFunction: ParseTree, input: ParseTree) => {
        for {
          // TODO: create a typeCheckFunction to handle this specifically
          functionTypeChecked <- typeCheck(startingFunction, AnyT, env)

          inputType = RetrieveType.retrieveInputTypeFromFunction(functionTypeChecked)

          inputValue <- typeCheck(input, inputType, env)
        } yield ApplyFunction(functionTypeChecked, inputValue)
      }
      case FieldAccessParse(struct, field) => {
        for {
          // TODO - build a specialize typeCheck where you expect a struct?
          typeCheckedStruct <- typeCheck(struct, AnyT, env)

          structParams <- typeCheckedStruct match {
            case StructInstance(value, StructT(params)) => Success(params)
            case CaseT(cases) => Success(cases)
            case _ => {
              Failure(s"Attempting to access field object $typeCheckedStruct which is not a struct instance")
            }
          }

          fieldsT = RetrieveType.retrieveInputTypeFromFunction(structParams)

          typeCheckedField <- typeCheck(field, fieldsT, env)
          
          _ <- Outcome.failWhen(
            !RetrieveType.isTermClosedLiteral(typeCheckedField),
            s"When accessing the value of a struct, the field must not have any variables or non-basic functions. This field is $typeCheckedField"
          )

          // Field must be fully evaluated
          evaluatedField <- Evaluator(typeCheckedField, env)

          // Make sure that this field actually exists
          resultingType <- Evaluator.quickApplyFunctionAttempt(structParams, evaluatedField, env)
        } yield AccessField(typeCheckedStruct, evaluatedField)
      }
      case CommandList(values: Vector[ParseTree]) => {
        // TODO - looks like we need this for now.. investigate why
        val substType = MakeSubstitution(expectedType, env)
        RetrieveType.getParentType(substType) match {
          case mapT@MapT(keyTypeT, valueT, completeness, featureSet) => {
            for {
              mapValues <- typeCheckLiteralMap(values, mapT, env)

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
          case StructT(params) => {
            for {
              parameterList <- structParamsIntoParameterList(params)
              result <- typeCheckStruct(
                parameterList,
                RetrieveType.retrieveInputTypeFromFunction(params),
                values,
                env,
                inPattern
              )
            } yield {
              StructInstance(result, StructT(params))
            }
          }
          case TypeT => {
            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // TODO - can this be simplified by combining with the MapT section above?

            val mapT = MapT(IdentifierT, TypeT, SubtypeInput, BasicMap)

            for {
              mapValues <- typeCheckLiteralMap(values, mapT, env)
            } yield {
              StructT(MapInstance(mapValues, mapT))
            }
          }
          case AnyT => {
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
        if (inPattern) {
          // Value is a type requirement and key is a pattern
          for {
            typeRequirement <- typeSpecificTypeChecker(value, env, inPattern)
            result <- typeCheck(key, typeRequirement, env, inPattern)
          } yield result
        } else {
          // TODO - do we need this?
          typeCheck(CommandList(Vector(expression)), expectedType, env)
        }
      }
      case LambdaParse(params, expression) if (expectedType == TypeT) => {
        for {
          inputType <- typeSpecificTypeChecker(params, env)
          newEnv <- inputType match {
            case StructT(params) => {
              for {
                parameterList <- structParamsIntoParameterList(params)
              } yield {
                env.newParams(parameterList)
              }
            }
            case _ => Success(env)
          }

          outputType <- typeSpecificTypeChecker(expression, newEnv)
        } yield {
          // TODO - how do we indicate completeness and featureSet in the LambdaParse Symbol?
          MapT(
            inputType,
            outputType,
            completeness = RequireCompleteness, 
            featureSet = FullFunction
          )
        }
      }
      /*case LambdaParse(params, expression) => {
        throw new Exception("This should not happen!")
      }*/
      // TODO: Remove this case, because lambda parse is now always going to refer to a type
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
                case MapT(inputT, outputType, completeness, featureSet) => {
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
          expectedExpressionType <- expectedType match {
            case MapT(inputType, outputT, completeness, featureSet) => {
              for {
                _ <- Outcome.failWhen(featureSet == BasicMap, ErrorMessageForBasicMap)
              } yield outputT
            }
            case _ => Success(AnyT)
          }

          tc <- typeCheck(expression, expectedExpressionType, env.newParams(newParams))
        } yield LambdaInstance(newParams, tc)
      }
    }

    for {
      nObject <- result

      // TODO: figure out what to really do here
      // TODO - perhaps if we were doing this all along with the parse analysis, we wouldn't need to do this.
      // Check that the object is part of the required subtype if given
      // Is it possible that this will always be true given the code above?
      // Once we can verify this, maybe we can remove this code

      /*_ = if (nObject == Index(0) || nObject == Index(1) || nObject == Index(2)) {
        println("\nAbout to call attempt to convert to type")
        println(s"Original parsed expression: $expression")
        println(s"Resulting Object: $nObject")
        println(s"Expected Type: $expectedType")
      }*/

      nObjectConverted <- SubtypeUtils.attemptToConvertToType(nObject, expectedType, env)
    } yield nObjectConverted
  }

  // This map could include pattern matching
  def typeCheckLiteralMap(
    values: Vector[ParseTree],
    mapT: MapT,
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    val keyType = mapT.inputType
    val valueType = mapT.outputType

    val patternMatchingAllowed = mapT.featureSet != BasicMap

    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, patternMatchingAllowed)
          objectFoundKey = resultKey.typeCheckResult

          objectFoundValue <- typeCheck(v, valueType, resultKey.newEnvironment)

          restOfMap <- typeCheckLiteralMap(restOfValues, mapT, env)
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

  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapObject,
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    for {
      tc <- typeCheck(expression, expectedType, env, inPattern = patternMatchingAllowed)

    } yield {
      val paramObjVector = findPatternsAndTurnIntoParameters(tc)
      val envCommands = paramObjVector.map(param => {
        FullEnvironmentCommand(IdentifierInstance(param.name), param)
      })

      val newEnv = env.newCommands(envCommands)

      TypeCheckWithPatternMatchingResult(tc, newEnv)
    }
  }

  def findPatternsAndTurnIntoParameters(nObject: NewMapObject): Vector[ParameterObj] = nObject match {
    case IdentifierPattern(name, nType) => Vector(ParameterObj(name, nType))
    case Index(_) | CountT | TypeT | AnyT | IdentifierT | IdentifierInstance(_) | RangeFunc(_) | IncrementFunc | LambdaInstance(_, _) |  ParameterObj(_, _) | IsCommandFunc => Vector.empty
    case MapInstance(_, _) => {
      // TODO - should we allow pattern-matched maps?
      Vector.empty
    }
    case StructT(_) | CaseT(_) | SubtypeT(_) => {
      // TODO - if we allow pattern-matched maps, then we should allow these as well
      Vector.empty
    }
    case ApplyFunction(_, _) => {
      // Apply function shouldn't be in a pattern
      Vector.empty
    }
    case MapT(inputType, outputType, _, _) => {
      findPatternsAndTurnIntoParameters(inputType) ++ findPatternsAndTurnIntoParameters(outputType)
    }
    case AccessField(struct, field) => {
      findPatternsAndTurnIntoParameters(struct)
    }
    case StructInstance(value, structT) => {
      value.map(_._2).map(findPatternsAndTurnIntoParameters).flatten
    }
    case CaseInstance(constructor, value, nType) => {
      findPatternsAndTurnIntoParameters(value)
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
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
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
              _ <- typeCheck(typeOfIdentifier, TypeT, env)
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

  // Assume that params is already type checked
  // We just want to concrete list of fields
  // TODO - we may be able to delete this!
  def structParamsIntoParameterList(
    params: NewMapObject
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    params match {
      case MapInstance(values, _) => Success(values)
      case _ => Failure(s"Cannot convert params into parameter list: $params")
    }
  }

  /*
   * We want to ensure that the struct was created correctly
   * TODO(2022): Maybe this should no longer be the struct checking function because it manipulates the environment?
   * - Rethink how all that works. We use this feature
   */
  def typeCheckStruct(
    parameterList: Vector[(NewMapObject, NewMapObject)],
    nTypeForStructFieldName: NewMapObject,
    valueList: Vector[ParseTree],
    env: Environment,
    inPattern: Boolean
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) if (!inPattern) => {
        for {
          valueId <- typeCheck(valueIdentifier, nTypeForStructFieldName, env, inPattern)

          _ <- Outcome.failWhen(paramId != valueId, s"Ids don't match: $paramId --- $valueId")

          tc <- typeCheck(valueObject, typeOfIdentifier, env, inPattern)
          substObj = MakeSubstitution(tc, env)

          newEnv = paramId match {
            case IdentifierInstance(s) => {
              env.newCommand(FullEnvironmentCommand(paramId, substObj))
            }
            case _ => env
          }

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, newEnv, inPattern)
        } yield {
          (paramId, substObj) +: result
        }
      }
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        //println(s"*** In here $valueObject -- $typeOfIdentifier -- $inPattern -- $typeOfIdentifier")
        for {
          tc <- typeCheck(valueObject, typeOfIdentifier, env, inPattern)
          
          substObj = MakeSubstitution(tc, env)

          newEnv = paramId match {
            case IdentifierInstance(s) => {
              env.newCommand(FullEnvironmentCommand(paramId, substObj))
            }
            case _ => env
          }

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, newEnv, inPattern)
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
  // Note that we want to result EVALUATED - otherwise we don't actually know if it's a type
  def typeSpecificTypeChecker(
    parseTree: ParseTree,
    env: Environment,
    inPattern: Boolean = false
  ): Outcome[NewMapObject, String] = {
    for {
      tc <- typeCheck(parseTree, TypeT, env, inPattern)
      evaluatedTc <- Evaluator(tc, env)
    } yield evaluatedTc
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObject, String] = {
    typeCheck(expression, AnyT, Environment.Base)
  }
}