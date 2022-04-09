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
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[NewMapExpression, String] = {
    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    val result: Outcome[NewMapExpression, String] = expression match {
      case NaturalNumberParse(i: Long) => {
        val parentExpectedType = RetrieveType.getParentType(expectedType, env)
        Evaluator.stripVersioning(parentExpectedType, env) match {
          case TaggedObject(UIndex(j), _) => {
            if (i < j) Success(ObjectExpression(TaggedObject(UIndex(i), parentExpectedType)))
            else Failure(s"Proposed index $i is too large for type $j")
          }
          case TaggedObject(umap@UMap(_), ExpandingSubsetT(superType)) => {
            for {
              result <- Evaluator.applyFunctionAttempt(
                TaggedObject(umap, MapT(superType, OrBooleanT, CommandOutput, BasicMap)),
                TaggedObject(UIndex(i), superType),
                env
              )

              isAllowed = result match {
                case TaggedObject(UIndex(1), _) => true
                case _ => false
              }

              _ <- Outcome.failWhen(!isAllowed, s"number $i not in subtype $expectedType")
            } yield {
              ObjectExpression(TaggedObject(UIndex(i), expectedType))
            }
          }
          case _ => {
            Success(ObjectExpression(TaggedObject(UIndex(i), CountT)))
          }
        }
      }
      case IdentifierParse(s: String, true) => Success(ObjectExpression(NewMapO.identifier(s)))
      case IdentifierParse(s: String, false) => {
        val expectingAnIdentifier = {
          SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env).toOption.getOrElse(false)
        }

        if (expectingAnIdentifier) {
          val obj = TaggedObject(UIdentifier(s), expectedType)

          Evaluator.stripVersioning(expectedType, env) match {
            case IdentifierT => Success(ObjectExpression(NewMapO.identifier(s)))
            case SubtypeT(isMember) => {
              for {
                result <- Evaluator.applyFunctionAttempt(isMember, obj, env)

                // TODO: the positive condition won't always be UIndex(1)
                isAllowed = result match {
                  case TaggedObject(UIndex(1), _) => true
                  case _ => false
                }

                _ <- Outcome.failWhen(!isAllowed, s"identifier $s not in subtype $expectedType")
              } yield ObjectExpression(obj)
            }
            case TaggedObject(umap, ExpandingSubsetT(parentType)) => {
              for {
                result <- Evaluator.applyFunctionAttempt(
                  TaggedObject(umap, MapT(parentType, OrBooleanT, CommandOutput, BasicMap)),
                  TaggedObject(UIdentifier(s), parentType),
                  env
                )

                // TODO: the positive condition won't always be UIndex(1)
                isAllowed = result match {
                  case TaggedObject(UIndex(1), _) => true
                  case _ => false
                }

                _ <- Outcome.failWhen(!isAllowed, s"identifier $s not in subtype $expectedType")
              } yield ObjectExpression(obj)
            }
            case _ => Failure(s"Can't convert identifier $s to type $expectedType")
          }
        } else env.lookup(s) match {
          case Some(EnvironmentValue(_, ParameterStatus)) => Success(ParamId(s))
          case Some(EnvironmentValue(nObject, BoundStatus)) => Success(ObjectExpression(nObject))
          case None => {
            Failure(s"Identifier $s is unknown, expecting type $expectedType --- ${SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env)}")
          }
        }
      }
      case ApplyParse(function, input) => {
        for {
          // TODO: Build a specialize type check for maps?
          functionTypeChecked <- typeCheck(function, AnyT, env, featureSet)

          inputType = RetrieveType.retrieveInputTypeFromFunction(functionTypeChecked, env)

          inputValue <- typeCheck(input, inputType, env, featureSet)
          functionFeatureSet = RetrieveType.retrieveFeatureSetFromFunction(functionTypeChecked, env)

          // Validate that this is allowed from the feature set
          _ <- Outcome.failWhen(
            !SubtypeUtils.isFeatureSetConvertible(functionFeatureSet, featureSet),
            s"Cannot allow function with feature set $functionFeatureSet in expression that should be featureSet $featureSet"
          )

          // If the function is a parameter, then there's no guarantee that it's not self-referential
          // TODO - flesh out these rules a little bit more
          _ <- Outcome.failWhen(
            !RetrieveType.isTermClosedLiteral(functionTypeChecked) && (featureSet != FullFunction),
            s"Function $functionTypeChecked is based on a parameter, which could create a self-referential definition, disallowed in featureSet $featureSet"
          )

          // If the function is actually a struct, and we're trying to get a field out of it,
          //  then we really have to know the params - a function that attached field names to field type
          //  and later check that it all lines up
          needToEvaluateParams = functionTypeChecked match {
            case ObjectExpression(TaggedObject(value, StructT(params))) => Some(params)
            case ObjectExpression(CaseT(cases)) => Some(cases)
            case _ => None
          }

          isConvertible <- needToEvaluateParams match {
            case None => Success(true)
            case Some(structParams) => {
              for {
                // Field must be fully evaluated
                evaluatedField <- Evaluator(inputValue, env)

                // Make sure that this field actually exists
                resultingType <- Evaluator.applyFunctionAttempt(structParams, evaluatedField, env)

                isConv <- SubtypeUtils.isTypeConvertible(resultingType, expectedType, env)
              } yield isConv
            }
          }

          _ <- Outcome.failWhen(!isConvertible, s"Function result error: ResultingType of $functionTypeChecked $inputValue not convertible to $expectedType")
        } yield {
          ApplyFunction(functionTypeChecked, inputValue)
        }
      }
      case CommandList(values: Vector[ParseTree]) => {
        expectedType match {
          case mapT@MapT(keyTypeT, _, completeness, _) => {
            for {
              mapValues <- typeCheckMap(values, mapT, env, featureSet)

              isCovered <- {
                if (completeness != RequireCompleteness) Success(true)
                else {
                  SubtypeUtils.doPatternsCoverType(mapValues.map(_._1), keyTypeT, env)
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + keyTypeT
              )
            } yield {
              BuildMapInstance(mapValues, mapT)
            }
          }
          case StructT(params@TaggedObject(UMap(parameterList), _)) => {
            for {
              result <- typeCheckStruct(
                parameterList,
                RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(params), env),
                values,
                env,
                featureSet
              )
            } yield {
              BuildMapInstance(result, StructT(params))
            }
          }
          case TypeT => {
            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // TODO - can this be simplified by combining with the MapT section above?
            val mapT = MapT(IdentifierT, TypeT, SubtypeInput, BasicMap)

            for {
              mapValues <- typeCheckMap(values, mapT, env, featureSet)
            } yield {
              BuildStructT(BuildMapInstance(mapValues, mapT))
            }
          }
          case AnyT => {
            // Steps to implement this:
            // type check all the elements of the map
            // Find a common type. DONT use Any - they should be in the same construction
            // Build a ReqMap from this
            Failure(s"CommandLists must be explicitly typed - $values")
          }
          case _ => {
            Failure(s"CommandLists not working yet with this expected type: $values exp: $expectedType")
          }
        }
      }
      case BindingCommandItem(key, value) => {
        typeCheck(CommandList(Vector(expression)), expectedType, env, featureSet)
      }
      case LambdaParse(input, output) => {
        for {
          inputType <- typeCheck(input, TypeT, env, featureSet)
          outputType <- typeCheck(output, TypeT, env, featureSet)
        } yield {
          // TODO - how do we indicate completeness and featureSet in the LambdaParse Symbol?
          BuildMapT(
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
    env: Environment,
    featureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    val keyType = mapT.inputType
    val valueType = mapT.outputType

    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, featureSet, mapT.featureSet)
          foundKeyPattern = resultKey.typeCheckResult

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueType,
            resultKey.newEnvironment,
            featureSet = mapT.featureSet
          )

          restOfMap <- typeCheckMap(restOfValues, mapT, env, featureSet)
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
    externalFeatureSet: MapFeatureSet, // Am I inside a map with restricted features already?
    internalFeatureSet: MapFeatureSet // Which feature set is this map allowed to use
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    val patternMatchingAllowed = internalFeatureSet != BasicMap
    val parentTypeIsIdentifier = SubtypeUtils.isTypeConvertible(expectedType, IdentifierT, env).toOption.getOrElse(false)

    (expression, expectedType) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed && !parentTypeIsIdentifier) => {
        Success(
          TypeCheckWithPatternMatchingResult(TypePattern(s, expectedType), env.newParam(s, expectedType))
        )
      }
      case (BindingCommandItem(IdentifierParse(s, false), value), _) if (patternMatchingAllowed) => {
        for {
          tc <- typeCheck(value, TypeT, env, externalFeatureSet)
          tcObj <- Evaluator(tc, env)
        } yield {
          TypeCheckWithPatternMatchingResult(TypePattern(s, tcObj), env.newParam(s, tcObj))
        }
      }
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (CommandList(values), StructT(TaggedObject(UMap(structValues), MapT(_, _, _, BasicMap)))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, externalFeatureSet, internalFeatureSet, env)
        } yield {
          TypeCheckWithPatternMatchingResult(StructPattern(tcmp.patterns), tcmp.newEnvironment)
        }
      }
      case (ApplyParse(constructorP, input), CaseT(cases)) if (patternMatchingAllowed) => {
        val caseConstructorType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(cases), env)
        for {
          constructorTC <- typeCheck(constructorP, caseConstructorType, env, BasicMap)
          constructor <- Evaluator(constructorTC, env)
          inputTypeExpected <- Evaluator.applyFunctionAttempt(cases, constructor, env)

          result <- typeCheckWithPatternMatching(input, inputTypeExpected, env, externalFeatureSet, internalFeatureSet)
        } yield {
          TypeCheckWithPatternMatchingResult(
            CasePattern(Evaluator.removeTypeTag(constructor).toOption.get, result.typeCheckResult),
            result.newEnvironment
          )
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env, externalFeatureSet)
          tcObj <- Evaluator(tc, env)
          untaggedObj <- Evaluator.removeTypeTag(tcObj)
        } yield {
          TypeCheckWithPatternMatchingResult(ObjectPattern(untaggedObj), env)
        }
      }
    }
  }

  case class TypeCheckWithMultiplePatternMatchingResult(
    patterns: Vector[NewMapPattern],
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithMultiplePatterns(
    expressions: Vector[(ParseTree, NewMapExpression)],
    externalFeatureSet: MapFeatureSet,
    internalFeatureSet: MapFeatureSet,
    env: Environment
  ): Outcome[TypeCheckWithMultiplePatternMatchingResult, String] = {
    expressions match {
      case (expression, nTypeExp) +: restOfExpressions => {
        for {
          nType <- Evaluator(nTypeExp, env)
          response <- typeCheckWithPatternMatching(expression, nType, env, externalFeatureSet, internalFeatureSet)
          pattern = response.typeCheckResult
          finalResponse <- typeCheckWithMultiplePatterns(restOfExpressions, externalFeatureSet, internalFeatureSet, response.newEnvironment)
        } yield {
          TypeCheckWithMultiplePatternMatchingResult(pattern +: finalResponse.patterns, finalResponse.newEnvironment)
        }
      }
      case _ => Success(TypeCheckWithMultiplePatternMatchingResult(Vector.empty, env))
    }
  }

  /*
   * We want to ensure that the struct was created correctly
   */
  def typeCheckStruct(
    parameterList: Vector[(NewMapPattern, NewMapExpression)],
    nTypeForStructFieldName: NewMapObject,
    valueList: Vector[ParseTree],
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        for {
          valueId <- typeCheck(valueIdentifier, nTypeForStructFieldName, env, featureSet)
          valueIdObj <- Evaluator(valueId, env)

          newParams <- Evaluator.attemptPatternMatch(paramId, valueIdObj, env)

          // Is this substitution neccesary??
          typeOfIdentifierObj <- Evaluator(MakeSubstitution(typeOfIdentifier, newParams, env), env)

          tc <- typeCheck(valueObject, typeOfIdentifierObj, env, featureSet)

          substExp = MakeSubstitution(tc, newParams, env)
          substObj <- Evaluator(substExp, env)

          newEnv = paramId match {
            case ObjectPattern(UIdentifier(s)) => {
              env.newCommand(FullEnvironmentCommand(s, substObj))
            }
            case _ => env
          }

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, newEnv, featureSet)
        } yield {
          (paramId, substExp) +: result
        }
      }
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          typeOfIdentifierObj <- Evaluator(typeOfIdentifier, env)
          tc <- typeCheck(valueObject, typeOfIdentifierObj, env, featureSet)

          valueObj <- Evaluator(tc, env)

          newEnv = paramId match {
            case ObjectPattern(UIdentifier(s)) => {
              env.newCommand(FullEnvironmentCommand(s, valueObj))
            }
            case _ => env
          }

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, newEnv, featureSet)
        } yield {
          (paramId, tc) +: result
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

  def apply(
    expression: ParseTree
  ): Outcome[NewMapExpression, String] = {
    typeCheck(expression, AnyT, Environment.Base, featureSet = FullFunction)
  }
}