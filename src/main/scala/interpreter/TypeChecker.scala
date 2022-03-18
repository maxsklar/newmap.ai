package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be.
   *   The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is the environment of values upon which we are working
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    val result = expression match {
      case NaturalNumberParse(i: Long) => {
        RetrieveType.getParentType(expectedType, env) match {
          case Index(j) => {
            if (j <= i) {
              Failure(s"Proposed index $i is too large for type $j")
            } else Success(IndexValue(i, j))
          }
          case _ => Success(Index(i))
        }
      }
      case IdentifierParse(s: String, true) => Success(IdentifierInstance(s))
      case IdentifierParse(s: String, false) => {
        val expectingAnIdentifier = {
          SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env).toOption.getOrElse(false)
        }

        if (expectingAnIdentifier) {
          Success(IdentifierInstance(s))
        } else env.lookup(s) match {
          case Some(ParameterObj(_, _)) => Success(ParamId(s))
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

          inputType = RetrieveType.retrieveInputTypeFromFunction(functionTypeChecked, env)

          inputValue <- typeCheck(input, inputType, env)
        } yield ApplyFunction(functionTypeChecked, inputValue)
      }
      case FieldAccessParse(struct, field) => {
        for {
          // TODO - build a specialize typeCheck where you expect a struct?
          typeCheckedStruct <- typeCheck(struct, AnyT, env)

          evaluatedStruct <- Evaluator(typeCheckedStruct, env)

          structParams <- evaluatedStruct match {
            case StructInstance(value, StructT(params)) => Success(params)
            case CaseT(cases) => Success(cases)
            case _ => {
              Failure(s"Attempting to access field object $evaluatedStruct which is not a struct instance")
            }
          }

          fieldsT = RetrieveType.retrieveInputTypeFromFunction(structParams, env)

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
        //val substType = MakeSubstitution(expectedType, env)
        //RetrieveType.getParentType(expectedType, env) match {
        expectedType match {
          case mapT@MapT(keyTypeT, valueT, completeness, featureSet) => {
            for {
              mapValues <- typeCheckMap(values, mapT, env)

              isCovered <- {
                if (completeness != RequireCompleteness) Success(true)
                else {
                  SubtypeUtils.doMapValuesCoverType(mapValues, keyTypeT, env)
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
                RetrieveType.retrieveInputTypeFromFunction(params, env),
                values,
                env
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
              mapValues <- typeCheckMap(values, mapT, env)
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
        typeCheck(CommandList(Vector(expression)), expectedType, env)
      }
      case LambdaParse(input, output) => {
        for {
          inputType <- typeSpecificTypeChecker(input, env)
          outputType <- typeSpecificTypeChecker(output, env)
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
  def typeCheckMap(
    values: Vector[ParseTree],
    mapT: MapT,
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapObject)], String] = {
    val keyType = mapT.inputType
    val valueType = mapT.outputType

    val patternMatchingAllowed = mapT.featureSet != BasicMap

    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, patternMatchingAllowed)
          foundKeyPattern = resultKey.typeCheckResult

          objectFoundValue <- typeCheck(v, valueType, resultKey.newEnvironment)

          restOfMap <- typeCheckMap(restOfValues, mapT, env)
        } yield {
          (foundKeyPattern -> objectFoundValue) +: restOfMap
        }
      }
      case s +: _ => {
        Failure(s"No binding found in map for item $s in $values")
      }
      case _ => Success(Vector.empty)
    }
  }

  case class TypeCheckWithPatternMatchingResult(
    typeCheckResult: NewMapPattern,
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapObject,
    env: Environment,
    patternMatchingAllowed: Boolean
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    val parentTypeIsIdentifier = SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env).toOption.getOrElse(false)

    (expression, expectedType) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed && !parentTypeIsIdentifier) => {
        Success(
          TypeCheckWithPatternMatchingResult(TypePattern(s, expectedType), env.newParam(s, expectedType))
        )
      }
      case (BindingCommandItem(IdentifierParse(s, false), value), _) if (patternMatchingAllowed) => {
        for {
          tc <- typeCheck(value, TypeT, env)
        } yield {
          TypeCheckWithPatternMatchingResult(TypePattern(s, tc), env.newParam(s, tc))
        }
      }
      // TODO: what if instead of BasicMap we have SimpleMap?
      case (CommandList(values), StructT(MapInstance(structValues, MapT(_, _, _, BasicMap)))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, env)
        } yield {
          TypeCheckWithPatternMatchingResult(StructPattern(tcmp.patterns), tcmp.newEnvironment)
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env)
        } yield {
          TypeCheckWithPatternMatchingResult(ObjectPattern(tc), env)
        }
      }
    }
  }

  case class TypeCheckWithMultiplePatternMatchingResult(
    patterns: Vector[NewMapPattern],
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithMultiplePatterns(
    expressions: Vector[(ParseTree, NewMapObject)],
    env: Environment
  ): Outcome[TypeCheckWithMultiplePatternMatchingResult, String] = {
    expressions match {
      case (expression, nType) +: restOfExpressions => {
        for {
          response <- typeCheckWithPatternMatching(expression, nType, env, true)
          pattern = response.typeCheckResult
          finalResponse <- typeCheckWithMultiplePatterns(restOfExpressions, response.newEnvironment)
        } yield {
          TypeCheckWithMultiplePatternMatchingResult(pattern +: finalResponse.patterns, finalResponse.newEnvironment)
        }
      }
      case _ => Success(TypeCheckWithMultiplePatternMatchingResult(Vector.empty, env))
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
          case Some(ParameterObj(_, _)) => FoundIdentifier(name)
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
  // TODO - can only be turned into a list if this is a BASIC MAP
  def structParamsIntoParameterList(
    params: NewMapObject
  ): Outcome[Vector[(NewMapPattern, NewMapObject)], String] = {
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
    parameterList: Vector[(NewMapPattern, NewMapObject)],
    nTypeForStructFieldName: NewMapObject,
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(NewMapPattern, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        for {
          valueId <- typeCheck(valueIdentifier, nTypeForStructFieldName, env)

          newParams <- Evaluator.attemptPatternMatch(paramId, valueId, env)

          tc <- typeCheck(valueObject, typeOfIdentifier, env)

          // TODO: Neccesary to make substitution here?
          substObj = MakeSubstitution(tc, newParams, env)

          newEnv = paramId match {
            case ObjectPattern(IdentifierInstance(s)) => {
              env.newCommand(FullEnvironmentCommand(s, substObj))
            }
            case _ => env
          }

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, newEnv)
        } yield {
          (paramId, substObj) +: result
        }
      }
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        //println(s"*** In here $valueObject -- $typeOfIdentifier -- $inPattern -- $typeOfIdentifier")
        for {
          tc <- typeCheck(valueObject, typeOfIdentifier, env)

          // TODO: Neccesary to make substitution here?
          //substObj = MakeSubstitution(tc, newParams, env)

          newEnv = paramId match {
            case ObjectPattern(IdentifierInstance(s)) => {
              env.newCommand(FullEnvironmentCommand(s, tc))
            }
            case _ => env
          }

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, newEnv)
        } yield {
          (paramId, tc/*substObj*/) +: result
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
  ): Outcome[NewMapObject, String] = {
    for {
      tc <- typeCheck(parseTree, TypeT, env)
      evaluatedTc <- Evaluator(tc, env)
    } yield evaluatedTc
  }

  def applyCommandTypeChecker(
    id: String,
    commandParseTree: ParseTree,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    for {
      versionedO <- Evaluator.lookupVersionedObject(id, env)
      inputT <- Evaluator.getCommandInputOfPureCommandType(versionedO.commandType, versionedO.currentState)
      result <- typeCheck(commandParseTree, inputT, env)
    } yield result
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObject, String] = {
    typeCheck(expression, AnyT, Environment.Base)
  }
}