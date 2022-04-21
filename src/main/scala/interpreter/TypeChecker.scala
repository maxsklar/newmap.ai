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
    expression match {
      case NaturalNumberParse(i: Long) => {
        val parentExpectedType = RetrieveType.getParentType(expectedType, env)

        Evaluator.stripVersioning(expectedType, env) match {
          case TaggedObject(UIndex(j), _) => {
            if (i < j) Success(ObjectExpression(TaggedObject(UIndex(i), parentExpectedType)))
            else {
              //throw new Exception(s"Proposed index $i is too large for type $j")
              Failure(s"Proposed index $i is too large for type $j")
            }
          }
          case TaggedObject(umap@UMap(_), ExpandingSubsetT(superType, _)) => {
            for {
              result <- Evaluator.applyFunctionAttempt(
                TaggedObject(umap, MapT(superType, OrBooleanT, MapConfig(CommandOutput, BasicMap))),
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
          case SubtypeT(isMember) => {
            val inputType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)
            for {
              inputTC <- typeCheck(expression, inputType, env, featureSet)

              // We should be allowed to evaluate this because it's just a number
              // BUT - this is an ugly call - it'd be nice if we didn't have to do it
              inputE <- Evaluator(inputTC, env)
              isMemberCheck <- Evaluator.applyFunctionAttempt(isMember, inputE, env)
              defaultValueOrResultType <- {
                Evaluator.getDefaultValueOfCommandType(RetrieveType.fromNewMapObject(isMemberCheck, env), env)
              }

              isMember = (isMemberCheck != defaultValueOrResultType)

              _ <- Outcome.failWhen(!isMember, s"Value $inputE not a member of subtype $expectedType")
            } yield inputTC
          }
          case _ => {
            Success(ObjectExpression(TaggedObject(UIndex(i), CountT)))
          }
        }
      }
      case IdentifierParse(s: String, true) => {
        val expectingAnIdentifier = typeIsExpectingAnIdentifier(expectedType, env)
        if (expectingAnIdentifier) {
          Success(ObjectExpression(TaggedObject(UIdentifier(s), expectedType)))
        } else {
          for {
            convertInstructions <- SubtypeUtils.isTypeConvertible(IdentifierT, expectedType, env)

            // TODO: execute convertInstructions
            convertedObject = TaggedObject(UIdentifier(s), IdentifierT)
          } yield convertedObject
        }
        // We need to check that the expectedType allows an identifier!!
        Success(ObjectExpression(NewMapO.identifier(s)))
      }
      case IdentifierParse(s: String, false) => {
        val expectingAnIdentifier = typeIsExpectingAnIdentifier(expectedType, env)

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
            case TaggedObject(umap, ExpandingSubsetT(parentType, _)) => {
              for {
                result <- Evaluator.applyFunctionAttempt(
                  TaggedObject(umap, MapT(parentType, OrBooleanT, MapConfig(CommandOutput, BasicMap))),
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
          case Some(EnvironmentValue(nType, ParameterStatus)) => {
            for {
              convertInstructions <- SubtypeUtils.isTypeConvertible(nType, expectedType, env)
              // TODO - execute convert instructions?
            } yield ParamId(s)
          }
          case Some(EnvironmentValue(nObject, BoundStatus)) => {
            val nType = RetrieveType.fromNewMapObject(nObject, env)
            for {
              convertInstructions <- SubtypeUtils.isObjectConvertibleToType(nObject, expectedType, env)
              // TODO - execute convert instructions on nObject
            } yield {
              ObjectExpression(nObject)
            }
          }
          case None => {
            Failure(s"Identifier $s is unknown, expecting type $expectedType --- ${typeIsExpectingAnIdentifier(expectedType, env)}")
          }
        }
      }
      case ApplyParse(function, input) => {
        for {
          // TODO: Build a specialize type check for maps?
          functionTypeChecked <- typeCheck(function, AnyT, env, featureSet)

          inputType = RetrieveType.retrieveInputTypeFromFunction(functionTypeChecked, env)

          inputValue <- typeCheck(input, inputType, env, featureSet)

          // Is member of Subtype check here?
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

          convertInstructions <- needToEvaluateParams match {
            case None => Success(Vector.empty)
            case Some(structParams) => {
              for {
                // Field must be fully evaluated
                evaluatedField <- Evaluator(inputValue, env)

                // Make sure that this field actually exists
                resultingType <- Evaluator.applyFunctionAttempt(structParams, evaluatedField, env)

                convertInstructions <- SubtypeUtils.isTypeConvertible(resultingType, expectedType, env)
                // TODO - do we also need to check the subtype?
              } yield convertInstructions
            }
          }
        } yield {
          // TODO - execute convertInstructions??
          ApplyFunction(functionTypeChecked, inputValue)
        }
      }
      case CommandList(values: Vector[ParseTree]) => {
        expectedType match {
          case mapT@MapT(keyTypeT, _, config) => {
            for {
              mapValues <- typeCheckMap(values, mapT, env, featureSet)

              isCovered <- {
                if (config.completeness != RequireCompleteness) Success(true)
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
            val mapT = MapT(IdentifierT, TypeT, MapConfig(SubtypeInput, BasicMap))

            for {
              mapValues <- typeCheckMap(values, mapT, env, featureSet)
            } yield {
              // TODO - MapT has the wrong input type here!
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
            MapConfig(RequireCompleteness, FullFunction)
          )
        }
      }
    }
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
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, featureSet, mapT.config.featureSet)
          foundKeyPattern = resultKey.typeCheckResult

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueType,
            resultKey.newEnvironment,
            featureSet = mapT.config.featureSet
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
    val parentTypeIsIdentifier = typeIsExpectingAnIdentifier(expectedType, env)

    (expression, expectedType) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed /*&& !parentTypeIsIdentifier*/) => {
        Success(
          TypeCheckWithPatternMatchingResult(WildcardPattern(s), env.newParam(s, expectedType))
        )
      }
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (CommandList(values), StructT(TaggedObject(UMap(structValues), MapT(_, _, MapConfig(_, BasicMap, _, _))))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
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

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, env, featureSet)
        } yield {
          (paramId, substExp) +: result
        }
      }
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          typeOfIdentifierObj <- Evaluator(typeOfIdentifier, env)
          tc <- typeCheck(valueObject, typeOfIdentifierObj, env, featureSet)
          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, env, featureSet)
        } yield {
          (paramId, tc) +: result
        }
      }
      case _ => {
        if (parameterList.isEmpty && valueList.isEmpty) {
          Success(Vector.empty)
        } else if (valueList.isEmpty && (parameterList.length == 1)) {
          parameterList.head._1 match {
            case WildcardPattern(_) => Success(Vector.empty)
            case _ => Failure("Additional parameters not specified " + parameterList.toString)
          }
        } else if (parameterList.nonEmpty) {
          Failure("Additional parameters not specified " + parameterList.toString)
        } else {
          Failure("To many parameters given: " + valueList.toString)
        }
      }
    }
  }

  def typeIsExpectingAnIdentifier(
    nType: NewMapObject,
    env: Environment
  ): Boolean = nType match {
    case IdentifierT => true
    case SubtypeT(isMember) => {
      val inputType = RetrieveType.retrieveInputTypeFromFunction(ObjectExpression(isMember), env)
      typeIsExpectingAnIdentifier(inputType, env)
    }
    case VersionedObjectLink(key, status) => {
      Evaluator.currentState(key.uuid, env).toOption.exists(state => {
        typeIsExpectingAnIdentifier(state, env)
      })
    }
    case TaggedObject(_, ExpandingSubsetT(parentType, _)) => {
      typeIsExpectingAnIdentifier(parentType, env)
    }
    case _ => false
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapExpression, String] = {
    typeCheck(expression, AnyT, Environment.Base, featureSet = FullFunction)
  }
}