package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

object TypeChecker {
  case class TypeCheckResponse(
    nExpression: UntaggedObject,
    refinedTypeClass: NewMapType
  )

  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType This represents the type that we expect the object to be
   *  It may be a specific type, or it may be a pattern of a type
   *  The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is the environment of values upon which we are working
   * @param featureSet This tells us which language features are allowed (usually FullFunction for all features, restricted in certain functions/maps)
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: NewMapType,
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[TypeCheckResponse, String] = {
    val expectedTypeOutcome = getFinalUnderlyingType(expectedType, env, env.typeSystem.currentState)

    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    expression match {
      case EmptyParse => {
        responseFromConversion(TaggedObject(UStruct(Vector.empty), NewMapO.emptyStruct), expectedType, env)
      }
      case NaturalNumberParse(i: Long) => {
        for {
          t <- expectedTypeOutcome
          refinedType <- TypeClassUtils.typeIsExpectingAnIndex(t, i, env)
        } yield {
          val expectingType = SubtypeUtils.isTypeConvertible(refinedType, TypeT, env).isSuccess

          if (expectingType) {
            val untaggedValue = env.typeSystem.typeToUntaggedObject(IndexT(UIndex(i)))
            TypeCheckResponse(untaggedValue, refinedType)
          } else {
            TypeCheckResponse(UIndex(i), refinedType)
          }
        }
      }
      case CharacterParse(s: String) => {
        if (s.length == 1) {
          val tObject = TaggedObject(UCharacter(s(0)), CharacterT)
          responseFromConversion(tObject, expectedType, env)
        } else {
          Failure(s"Character not recognized because it has length > 1: $s")
        }
      }
      case StringParse(s: String) => {
        val fixedS = s.replace("\\n", "\n").replace("\\t", "\t")

        val uObject = UCase(
          UIndex(fixedS.length - 2),
          UStruct(fixedS.toCharArray().toVector.drop(1).dropRight(1).map(c => UCharacter(c)))
        )

        val stringType = CustomT("String", UStruct(Vector.empty))

        responseFromConversion(TaggedObject(uObject, stringType), expectedType, env)
      }
      case IdentifierParse(s: String, true) => {
        // We need to check that the expectedType allows an identifier!!
        responseFromConversion(TaggedObject(UIdentifier(s), IdentifierT), expectedType, env)
      }
      case IdentifierParse(s: String, false) => {
        val useLiteralIdentifier = for {
          underlyingExpectedT <- expectedTypeOutcome.toOption

          // TODO: This exception is awkward - maybe find some way to account for this in "attemptConvertObjectToType"
          // - we are looking for types that ONLY include identifiers.
          _ <- underlyingExpectedT match {
            case WildcardPatternT(_) => None
            case _ => Some(())
          }

          // Herin Lies the problem!!!
          result <- SubtypeUtils.attemptConvertObjectToType(TaggedObject(UIdentifier(s), IdentifierT), underlyingExpectedT, env).toOption
        } yield ()

        if (useLiteralIdentifier.nonEmpty) {
          Success(TypeCheckResponse(UIdentifier(s), expectedType))
        } else {
          env.lookup(s) match {
            case Some(EnvironmentParameter(nType)) => {
              for {
                response <- SubtypeUtils.isTypeConvertible(nType, expectedType, env)
                // TODO - execute convert instructions?
                // response.convertInstructions
              } yield TypeCheckResponse(ParamId(s), nType)
            }
            case Some(EnvironmentBinding(nObject)) => {
              for { 
                tObject <- SubtypeUtils.attemptConvertObjectToType(nObject, expectedType, env)
              } yield {
                // TODO: why can't we use tObject.nType?
                // We can once we solve the parameter-in-key problem (probably with a singleton map)
                val nType = RetrieveType.fromNewMapObject(nObject, env)
                TypeCheckResponse(tObject.uObject, nType)
              }
            }
            case None if (env.typeSystem.currentMapping.get(s).nonEmpty) => {
              for {
                tcResult <- typeCheck(
                  ConstructCaseParse(expression, LiteralListParse(Vector.empty, MapType)),
                  HistoricalTypeT(env.typeSystem.currentState),
                  env,
                  featureSet
                )
              } yield {
                expectedTypeOutcome match {
                  case Success(HistoricalTypeT(uuid)) => {
                    TypeCheckResponse(
                      tcResult.nExpression,
                      HistoricalTypeT(uuid)
                    )
                  }
                  case _ => {
                    TypeCheckResponse(
                      UCase(
                        UIdentifier("WithState"),
                        UCase(
                          Uuuid(env.typeSystem.currentState),
                          tcResult.nExpression
                        )
                      ),
                      TypeT
                    )
                  }
                }
              }
            }
            case None => {
              expectedTypeOutcome match {
                case Success(CaseT(cases, IdentifierT, featureSet)) => {
                  for {
                    caseType <- Evaluator.applyFunctionAttempt(UMap(cases), UIdentifier(s), env)

                    caseT <- env.typeSystem.convertToNewMapType(caseType)

                    // TODO - of course, we can formulize this better!
                    caseTypeIsEmptyStruct = caseT match {
                      case StructT(params, _, _, _) => params.isEmpty
                      case _ => false
                    }

                    isSingularType = (caseTypeIsEmptyStruct || (caseT == IndexT(UIndex(1))))
                    _ <- Outcome.failWhen(!isSingularType, s"Type Value cannot be inferred: $caseType")
                  } yield {
                    if (caseT == IndexT(UIndex(1))) {
                      TypeCheckResponse(UCase(UIdentifier(s), UIndex(0)), expectedType)
                    } else {
                      TypeCheckResponse(UCase(UIdentifier(s), UMap(Vector.empty)), expectedType)
                    }
                  }
                }
                case Success(TypeT) => {
                  for {
                    parameterT <- env.typeSystem.getParameterType(env.typeSystem.currentState, s)

                    // TODO - of course, we can formulize this better!
                    parameterTypeIsEmptyStruct = parameterT match {
                      case StructT(params, _, _, _) => params.isEmpty
                      case _ => false
                    }

                    _ <- Outcome.failWhen(!parameterTypeIsEmptyStruct, s"No parameter specified for type $s")
                  } yield {
                    TypeCheckResponse(UCase(UIdentifier(s), UMap(Vector.empty)), expectedType)
                  }
                }
                case Success(WildcardPatternT(_)) => {
                  Failure(s"Identifier $s is unknown")
                }
                case Success(nType) => {
                  val trialObject = TaggedObject(UIdentifier(s), IdentifierT)
                  responseFromConversion(trialObject, nType, env)
                }
                case _ => {
                  Failure(s"Identifier $s is unknown, expecting type class $expectedType")
                }
              }
            }
          }
        }
      }
      case ApplyParse(function, input) => {
        for {
          // TODO! We need to use the type transform in here
          result <- typeCheckUnknownFunction(function, input, env)

          // Is member of Subtype check here?
          functionFeatureSet <- retrieveFeatureSetFromFunctionTypePattern(result.typeOfFunction, env)

          // If the function is a simplemap, and if it has no internal parameters, it can be executed now, and considered a basic
          functionFeatureSetFixed = if (functionFeatureSet.getLevel <= SimpleFunction.getLevel) {
            Evaluator.applyFunctionAttempt(result.functionExpression, result.inputExpression, env) match {
              case Success(result) => BasicMap // TODO - save the actual result we got!
              case Failure(_) => functionFeatureSet
            }
          } else functionFeatureSet

          // Validate that this is allowed from the feature set
          _ <- Outcome.failWhen(
            !SubtypeUtils.isFeatureSetConvertible(functionFeatureSetFixed, featureSet),
            s"Cannot allow function with feature set $functionFeatureSetFixed in expression that should be featureSet $featureSet"
          )

          // If the function is a parameter, then there's no guarantee that it's not self-referential
          // TODO - flesh out these rules a little bit more
          _ <- Outcome.failWhen(
            !RetrieveType.isTermClosedLiteral(result.functionExpression) && (featureSet != FullFunction),
            s"Function ${result.functionExpression} is based on a parameter, which could create a self-referential definition, disallowed in featureSet $featureSet"
          )

          response <- SubtypeUtils.isTypeConvertible(result.resultingType, expectedType, env)
          // TODO - execute response.convertInstructions
        } yield {
          val applyParseResult = ApplyFunction(result.functionExpression, result.inputExpression, StandardMatcher)
          TypeCheckResponse(applyParseResult, result.resultingType)
        }
      }
      case LiteralListParse(values: Vector[ParseTree], llType: LiteralListType) => {
        expectedTypeOutcome match {
          // TODO: Remove this in favor of other struct T
          case Success(StructT(parameterList, parentFieldType, _, _)) => {
            for {
              result <- typeCheckStruct(
                parameterList,
                parentFieldType, // Is this right - or do we need to pass in the subset??
                values,
                env,
                featureSet
              )
            } yield {
              TypeCheckResponse(UMap(result), expectedType)
            }
          }
          case Success(MapT(UMap(typeTransform), MapConfig(MapPattern, internalFeatureSet, _, _, _))) => {
            for {
              _ <- Outcome.failWhen(values.length > 1, "Map Pattern can only have 1 key-value pair: " + values)
              value <- Outcome(values.headOption, "Empty Map Pattern")
              mapValue <- typeCheckMapPattern(value, typeTransform, internalFeatureSet, env, featureSet)
            } yield TypeCheckResponse(mapValue, expectedType)
          }
          case Success(MapT(UMap(typeTransform), config)) => {
            val isArrayInput = llType == ArrayType
            val correctedValues = if (isArrayInput) {
              values.zipWithIndex.map(x => KeyValueBinding(NaturalNumberParse(x._2), x._1))
            } else {
              values
            }

            for {
              mapValues <- typeCheckGenericMap(correctedValues, typeTransform, config.featureSet, env, featureSet)

              _ <- Outcome.failWhen(typeTransform.length > 1, s"Could not handle type transform: $typeTransform")

              isCovered <- {
                if (config.completeness != RequireCompleteness) Success(true)
                else if (typeTransform.length == 0) Success(true)
                else {
                  for {
                    headTypePattern <- env.typeSystem.convertToNewMapType(typeTransform.head._1)
                    result <- SubtypeUtils.doPatternsCoverType(mapValues.map(_._1), headTypePattern, env)
                  } yield result
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + typeTransform.head._1
              )
            } yield {
              TypeCheckResponse(UMap(mapValues), expectedType)
            }
          }
          case Success(TypeT) | Success(HistoricalTypeT(_)) => {
            val typeT = expectedTypeOutcome.toOption.get

            val typeTransform = Vector(
              env.typeSystem.typeToUntaggedObject(IdentifierT) -> patternToExpression(env.typeSystem.typeToUntaggedObject(typeT))
            )

            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // OR we are just being given a list of types
            // TODO - can this be simplified by combining with the MapT section above?
            {
              for {
                mapValues <- typeCheckGenericMap(values, typeTransform, BasicMap, env, featureSet)
              } yield {
                TypeCheckResponse(
                  env.typeSystem.typeToUntaggedObject(StructT(mapValues, IdentifierT, RequireCompleteness, BasicMap)),
                  expectedType
                )
              }
            }.rescue(f => {
              for {
                expressions <- typeCheckSequence(values, typeT, env)
              } yield {
                val indexType = IndexT(UIndex(expressions.length))
                TypeCheckResponse(
                  env.typeSystem.typeToUntaggedObject(StructT(
                    expressions.zipWithIndex.map(x => UIndex(x._2) -> x._1),
                    indexType,
                    RequireCompleteness,
                    BasicMap
                  )),
                  expectedType
                )
              }
            })
          }
          case Success(WildcardPatternT(_)) => {
            Failure(s"Lists must be explicitly typed - $values")
          }
          case Success(UndefinedT) => {
            throw new Exception(s"our bug: $values")
          }
          case _ => {
            Failure(s"Lists not working yet with this expected type: $values exp: $expectedType -- $expectedTypeOutcome")
          }
        }
      }
      case KeyValueBinding(key, value) => {
        typeCheck(LiteralListParse(Vector(expression), MapType), expectedType, env, featureSet)
      }
      case LambdaParse(input, output) => {
        // Make sure that this fits expectedType!
        // Also - why do these need to be evaluated? seems like they do!
        for {
          inputType <- typeCheck(input, TypeT, env, featureSet)
          outputType <- typeCheck(output, TypeT, env, featureSet)

          evalInputType <- Evaluator(inputType.nExpression, env)
        } yield {
          val typeTransform = UMap(Vector(evalInputType -> outputType.nExpression))
          // TODO - how do we indicate completeness and featureSet in the LambdaParse Symbol?
          val mapConfig = MapConfig(RequireCompleteness, FullFunction)

          TypeCheckResponse(
            env.typeSystem.typeToUntaggedObject(MapT(typeTransform, mapConfig)),
            TypeT
          )
        }
      }
      case ConstructCaseParse(first, second) => {
        expectedTypeOutcome match {
          case Success(CaseT(simpleMap, parentFieldType, _)) => {
            for {
              firstExp <- typeCheck(first, parentFieldType, env, featureSet)

              // TODO - we must ensure that the evaluator is not evaluating anything too complex here
              // must be a "simple map" type situation
              // can this be built into the evaluator?
              firstObj <- Evaluator(firstExp.nExpression, env)
              secondType <- Evaluator.applyFunctionAttempt(UMap(simpleMap), firstObj, env)

              secondT <- Evaluator.asType(secondType, env)
              secondExp <- typeCheck(second, secondT, env, featureSet)
            } yield {
              TypeCheckResponse(UCase(firstObj, secondExp.nExpression), expectedType)
            }
          }
          case Success(HistoricalTypeT(uuid)) => {
            for {
              result <- typeCheckCaseAsType(first, second, uuid, featureSet, env)
            } yield TypeCheckResponse(result, expectedType)
          }
          case Success(TypeT) | Success(WildcardPatternT(_)) => {
            for {
              result <- typeCheckCaseAsType(first, second, env.typeSystem.currentState, featureSet, env)
            } yield TypeCheckResponse(result, TypeT)
          }
          case nTypeOutcome => {
            Failure(s"Case type must be specified for $expression -- instead got $nTypeOutcome")
          }
        }
      }
      case LiteralCode(statements, expression) => {
        CodeBlockTypeChecker(statements, expression, expectedType, env, featureSet)
      }
    }
  }

  def responseFromConversion(
    nObject: NewMapObject,
    expectedType: NewMapType,
    env: Environment
  ): Outcome[TypeCheckResponse, String] = {
    SubtypeUtils.attemptConvertObjectToType(nObject, expectedType, env).map(tObject => {
      TypeCheckResponse(tObject.uObject, tObject.nType)
    })
  }

  def typeCheckSequence(
    values: Vector[ParseTree],
    valueType: NewMapType,
    env: Environment,
  ): Outcome[Vector[UntaggedObject], String] = {
    values match {
      case KeyValueBinding(k, v) +: _ => {
        Failure(s"Sequences don't work with binding command item $k : $v")
      }
      case value +: restOfValues => {
        for {
          objectFoundValue <- typeCheck(value, valueType, env, BasicMap)
          restOfMap <- typeCheckSequence(restOfValues, valueType, env)
        } yield {
          objectFoundValue.nExpression +: restOfMap
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def typeCheckGenericMap(
    values: Vector[ParseTree],
    typeTransform: Vector[(UntaggedObject, UntaggedObject)],
    internalFeatureSet: MapFeatureSet,
    env: Environment,
    externalFeatureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = {
    val inputTypeClass = if (typeTransform.length == 1) {
      typeTransform.head._1
    } else {
      throw new Exception(s"Can't work with type transform if it doesn't have exactly 1 pattern (because it's unimplemented): $typeTransform")
    }
    
    values match {
      case KeyValueBinding(k, v) +: restOfValues => {
        for {
          inputTypeClassT <- env.typeSystem.convertToNewMapType(inputTypeClass)
          resultKey <- typeCheckWithPatternMatching(k, inputTypeClassT, env, externalFeatureSet, internalFeatureSet)

          // Keys must be evaluated on the spot
          foundKeyPattern <- Evaluator(resultKey.typeCheckResult, env)

          valueTypePatternUntagged <- Evaluator.applyFunctionAttempt(
            UMap(typeTransform),
            env.typeSystem.typeToUntaggedObject(resultKey.expectedTypeRefinement),
            env,
            TypeMatcher
          )

          valueTypePattern <- env.typeSystem.convertToNewMapType(valueTypePatternUntagged)

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueTypePattern,
            resultKey.newEnvironment,
            featureSet = internalFeatureSet
          )

          restOfMap <- typeCheckGenericMap(restOfValues, typeTransform, internalFeatureSet, env, externalFeatureSet)
        } yield {
          (foundKeyPattern -> objectFoundValue.nExpression) +: restOfMap
        }
      }
      case s +: _ => {
        Failure(s"No binding found in map for item $s in $values -- $typeTransform")
      }
      case _ => Success(Vector.empty)
    }
  }

  def typeCheckMapPattern(
    value: ParseTree,
    typeTransform: Vector[(UntaggedObject, UntaggedObject)],
    internalFeatureSet: MapFeatureSet,
    env: Environment,
    externalFeatureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[UntaggedObject, String] = {
    val inputTypeClass = if (typeTransform.length == 1) {
      typeTransform.head._1
    } else {
      throw new Exception(s"Can't work with type transform if it doesn't have exactly 1 pattern (because it's unimplemented): $typeTransform")
    }

    value match {
      case KeyValueBinding(k, v) => {
        // TODO - this is a copy of type check generic map for now
        for {
          inputTypeClassT <- env.typeSystem.convertToNewMapType(inputTypeClass)
          resultKey <- typeCheckWithPatternMatching(k, inputTypeClassT, env, externalFeatureSet, internalFeatureSet)

          // Keys must be evaluated on the spot
          foundKeyPattern <- Evaluator(resultKey.typeCheckResult, env)

          valueTypePatternUntagged <- Evaluator.applyFunctionAttempt(
            UMap(typeTransform),
            env.typeSystem.typeToUntaggedObject(resultKey.expectedTypeRefinement),
            env,
            TypeMatcher
          )

          valueTypePattern <- env.typeSystem.convertToNewMapType(valueTypePatternUntagged)

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueTypePattern,
            resultKey.newEnvironment,
            featureSet = internalFeatureSet
          )
        } yield {
          UMapPattern(foundKeyPattern, objectFoundValue.nExpression)
        }
      }
      case _ => {
        Failure(s"No binding found in map pattern for value $value -- $typeTransform")
      }
    }
  }

  def getUnderlyingType(name: String, params: UntaggedObject, env: Environment, typeSystemId: UUID): Outcome[UntaggedObject, String] = {
    val typeSystem = env.typeSystem
    for {
      mappingToUse <- Outcome(typeSystem.historicalMapping.get(typeSystemId), s"Couldn't pull up type system $typeSystemId")
      typeId <- Outcome(typeSystem.currentMapping.get(name), s"$name must be defined")
      underlyingTypeInfo <- Outcome(typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find underlying type for $name")

      parameterPattern = underlyingTypeInfo._1
      genericUnderlyingType = underlyingTypeInfo._2

      substitutions <- Evaluator.attemptPatternMatch(parameterPattern, params, StandardMatcher, env)

      underlyingType = MakeSubstitution(genericUnderlyingType, substitutions)
    } yield underlyingType
  }

  def getFinalUnderlyingType(nType: NewMapType, env: Environment, typeSystemId: UUID): Outcome[NewMapType, String] = {
    for {
      result <- nType match {
        case CustomT(name, params) => {
          for {
            partialResult <- getUnderlyingType(name, params, env, typeSystemId)
            partialResultT <- env.typeSystem.convertToNewMapType(partialResult)
            finalizeResult <- getFinalUnderlyingType(partialResultT, env, typeSystemId)
          } yield finalizeResult
        }
        case WithStateT(uuid, t) => getFinalUnderlyingType(t, env, uuid)
        case _ => Success(nType)
      }
    } yield result
  }

  def typeCheckCaseAsType(
    first: ParseTree,
    second: ParseTree,
    typeSystemId: UUID,
    featureSet: MapFeatureSet,
    env: Environment
  ): Outcome[UntaggedObject, String] = {

    for {
      firstAsString <- first match {
        case IdentifierParse(s, false) => Success(s)
        case _ => Failure(s"Constructor for type must be an identifier, it was $first")
      }
    
      typeOfParameter <- env.typeSystem.getParameterType(env.typeSystem.currentState, firstAsString)

      paramValue <- typeCheck(second, typeOfParameter, env, featureSet)
    } yield UCase(UIdentifier(firstAsString), paramValue.nExpression)
  }

  def patternToExpression(nPattern: UntaggedObject): UntaggedObject = {
    nPattern match {
      case UWildcardPattern(name) => ParamId(name)
      case UStruct(params) => {
        val structOrdering = params.zipWithIndex.map(param => {
          UIndex(param._2) -> patternToExpression(param._1)
        })

        UMap(structOrdering)
      }
      case UCase(constructor, input) => UCase(constructor, patternToExpression(input))
      case uObject => uObject
    }
  }

  case class TypeCheckWithPatternMatchingResult(
    typeCheckResult: UntaggedObject,
    expectedTypeRefinement: NewMapType,
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapType,
    env: Environment,

    // TODO - I want to get rid of this!
    externalFeatureSet: MapFeatureSet, // Am I inside a map with restricted features already?
    internalFeatureSet: MapFeatureSet // Which feature set is this map allowed to use
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    val patternMatchingAllowed = internalFeatureSet.getLevel >= PatternMap.getLevel
    
    // Use this??
    //val parentTypeIsIdentifier = TypeClassUtils.typeIsExpectingAnIdentifier(expectedType, s, env)

    val expectedTypeOutcome = getFinalUnderlyingType(expectedType, env, env.typeSystem.currentState)

    (expression, expectedTypeOutcome) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed) => {
        Success(
          TypeCheckWithPatternMatchingResult(UWildcardPattern(s), expectedType, env.newParam(s, expectedType))
        )
      }
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (LiteralListParse(values, _), Success(StructT(structValues, parentFieldType, _, _))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, externalFeatureSet, internalFeatureSet, env)
        } yield {
          TypeCheckWithPatternMatchingResult(UStruct(tcmp.patterns), expectedType, tcmp.newEnvironment)
        }
      }
      case (ConstructCaseParse(constructorP, input), Success(CaseT(cases, parentFieldType, _))) if (patternMatchingAllowed) => {
        for {
          // TODO - update environment here
          constructorTC <- typeCheckWithPatternMatching(constructorP, parentFieldType, env, externalFeatureSet, if (parentFieldType == IdentifierT) BasicMap else internalFeatureSet)
          constructor <- Evaluator(constructorTC.typeCheckResult, env)

          inputTypeExpected <- Evaluator.applyFunctionAttempt(UMap(cases), constructor, constructorTC.newEnvironment)
          inputTExpected <- Evaluator.asType(inputTypeExpected, constructorTC.newEnvironment)

          result <- typeCheckWithPatternMatching(input, inputTExpected, constructorTC.newEnvironment, externalFeatureSet, internalFeatureSet)
        } yield {
          TypeCheckWithPatternMatchingResult(
            UCase(constructor, result.typeCheckResult),
            expectedType,
            result.newEnvironment
          )
        }
      }
      case (ConstructCaseParse(constructorP, input), Success(TypeT)) if (patternMatchingAllowed) => {
        for {
          // For now, the constructor will explicitly point to a generic case type
          // This is a problem with Option!!!
          // This is not an unknown type!!!
          name <- constructorP match {
            case IdentifierParse(typeConstructor, _) => Success(typeConstructor)
            case _ => Failure("type constructor must be an identifier")
          }

          typeId <- Outcome(env.typeSystem.currentMapping.get(name), s"$name is not defined in the type system")

          parameterType <- Outcome(env.typeSystem.typeToParameterType.get(typeId), s"Couldn't find parameter type for $name")
          parameterT <- env.typeSystem.convertToNewMapType(parameterType)

          result <- typeCheckWithPatternMatching(
            input,
            parameterT,
            env,
            externalFeatureSet,
            internalFeatureSet
          )
        } yield {
          TypeCheckWithPatternMatchingResult(
            UCase(UIdentifier(name), result.typeCheckResult),
            expectedType,
            result.newEnvironment
          )
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env, externalFeatureSet)
          //tcPattern <- expressionToPattern(tc.nExpression)
          tcPattern = tc.nExpression
        } yield {
          TypeCheckWithPatternMatchingResult(tcPattern, expectedType, env)
        }
      }
    }
  }

  case class TypeCheckWithMultiplePatternMatchingResult(
    patterns: Vector[UntaggedObject],
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithMultiplePatterns(
    expressions: Vector[(ParseTree, UntaggedObject)],
    externalFeatureSet: MapFeatureSet,
    internalFeatureSet: MapFeatureSet,
    env: Environment
  ): Outcome[TypeCheckWithMultiplePatternMatchingResult, String] = {
    expressions match {
      case (expression, nTypeExp) +: restOfExpressions => {
        for {
          nTypeObj <- Evaluator(nTypeExp, env)
          nType <- Evaluator.asType(nTypeObj, env)
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
    parameterList: Vector[(UntaggedObject, UntaggedObject)],
    nTypeForStructFieldName: NewMapType,
    valueList: Vector[ParseTree],
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (KeyValueBinding(valueIdentifier, valueObject) +: restOfValueList)) => {
        for {
          valueId <- typeCheck(valueIdentifier, nTypeForStructFieldName, env, featureSet)
          valueIdObj <- Evaluator(valueId.nExpression, env)

          newParams <- Evaluator.attemptPatternMatch(paramId, valueIdObj, StandardMatcher, env)

          // Is this substitution neccesary??
          typeOfIdentifierObj <- Evaluator(MakeSubstitution(typeOfIdentifier, newParams), env)
          typeOfIdentifierT <- Evaluator.asType(typeOfIdentifierObj, env)

          tc <- typeCheck(valueObject, typeOfIdentifierT, env, featureSet)

          substExp = MakeSubstitution(tc.nExpression, newParams)

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, env, featureSet)
        } yield {
          (paramId, substExp) +: result
        }
      }
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          typeOfIdentifierObj <- Evaluator(typeOfIdentifier, env)
          typeOfIdentifierT <- Evaluator.asType(typeOfIdentifierObj, env)
          tc <- typeCheck(valueObject, typeOfIdentifierT, env, featureSet)
          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, env, featureSet)
        } yield {
          (paramId, tc.nExpression) +: result
        }
      }
      case _ => {
        if (parameterList.isEmpty && valueList.isEmpty) {
          Success(Vector.empty)
        } else if (valueList.isEmpty && (parameterList.length == 1)) {
          parameterList.head._1 match {
            case UWildcardPattern(_) => Success(Vector.empty)
            case _ => {
              Failure("A) Additional parameters not specified " + parameterList.toString)
            }
          }
        } else if (parameterList.nonEmpty) {
          Failure("B) Additional parameters not specified " + parameterList.toString)
        } else {
          Failure("To many parameters given: " + valueList.toString)
        }
      }
    }
  }

  def apply(
    expression: ParseTree
  ): Outcome[TypeCheckResponse, String] = {
    val env = (new EnvironmentInterpreter()).env
    typeCheckUnknownType(expression, env)
  }

  def typeCheckUnknownType(
    expression: ParseTree,
    env: Environment
  ): Outcome[TypeCheckResponse, String] = {
    typeCheck(expression, WildcardPatternT("_"), env, FullFunction)
  }

  case class TypeCheckUnknownFunctionResult(
    functionExpression: UntaggedObject,
    typeOfFunction: NewMapType,
    inputExpression: UntaggedObject,
    resultingType: NewMapType
  )

  def typeCheckUnknownFunction(
    function: ParseTree,
    input: ParseTree,
    env: Environment
  ): Outcome[TypeCheckUnknownFunctionResult, String] = {
    for {
      functionTypeChecked <- typeCheckUnknownType(function, env)

      typeOfFunction = functionTypeChecked.refinedTypeClass

      inputTypeOption = inputTypeCheckFromFunctionType(typeOfFunction, env)

      inputTagged <- inputTypeOption match {
        case Some(inputT) => for {
          inputTypeChecked <- typeCheck(input, inputT, env, FullFunction)
        } yield (inputTypeChecked.nExpression -> inputTypeChecked.refinedTypeClass)
        case None => {
          for {
            typeCheckUnknownTypeResult <- typeCheckUnknownType(input, env)
          } yield (typeCheckUnknownTypeResult.nExpression -> typeCheckUnknownTypeResult.refinedTypeClass)
        }
      }

      (inputTC, inputType) = inputTagged

      untaggedInputType = env.typeSystem.typeToUntaggedObject(inputType)

      resultingType <- typeOfFunction match {
        case StructT(params, _, _, _) => outputTypeFromStructParams(params, inputTC, env)
        case TypeClassT(typeTransform, implementation) => {
          outputTypeFromTypeClassParams(typeTransform, implementation, inputTC, inputType, env)
        }
        case MapT(typeTransform, config) => {
          for {
            typeAsObj <- Evaluator.applyFunctionAttempt(typeTransform, untaggedInputType, env, TypeMatcher)
            nType <- Evaluator.asType(typeAsObj, env)
            // TODO: This should be uncommented when we can build "convertible to" into the type transform
            //_ <- Outcome.failWhen(nType == UndefinedT, s"Couldn't apply type $inputType to type transform $typeTransform")
          } yield nType
        }
        case TypeT => {
          for {
            typeCheckedT <- env.typeSystem.convertToNewMapType(functionTypeChecked.nExpression)

            underlingT <- getFinalUnderlyingType(typeCheckedT, env, env.typeSystem.currentState)

            result <- underlingT match {
              case TypeClassT(typeTransform, implementation) => {
                outputTypeFromTypeClassParams(typeTransform, implementation, inputTC, inputType, env)
              }
              case _ => Failure(s"Cannot get resulting type from function of Type ${functionTypeChecked.nExpression} -- $function -- $input")
            }
          } yield result
        }
        case _ => Failure(s"Cannot get resulting type from function type $typeOfFunction -- $function -- $input")
      }

      resultingFunctionExpression <- typeOfFunction match {
        case TypeT => {
          for {
            typeCheckedT <- env.typeSystem.convertToNewMapType(functionTypeChecked.nExpression)
            underlingT <- getFinalUnderlyingType(typeCheckedT, env, env.typeSystem.currentState)

            impl <- underlingT match {
              case TypeClassT(_, implementation) => {
                Success(implementation)
              }
              case _ => Failure(s"Cannot get resulting type from function of Type ${underlingT}")
            }

            strippedExpression = Evaluator.stripVersioningU(functionTypeChecked.nExpression, env)

            patternMatchAttempted <- Evaluator.attemptPatternMatchInOrder(impl, untaggedInputType, env, TypeMatcher)
          } yield {
            patternMatchAttempted
          }
        }
        case _ => Success(functionTypeChecked.nExpression)
      }
    } yield {
      TypeCheckUnknownFunctionResult(resultingFunctionExpression, typeOfFunction, inputTC, resultingType)
    }    
  }

  // Returns a set of patterns representing newmap types
  // TODO - it should not return an option of NewMapType, rather some patterns!!
  def inputTypeCheckFromFunctionType(nFunctionTypeClass: NewMapType, env: Environment): Option[NewMapType] = {
    val underlyingTOutcome =  getFinalUnderlyingType(nFunctionTypeClass, env, env.typeSystem.currentState)

    underlyingTOutcome match {
      case Success(StructT(params, parentFieldType, _, _)) => {
        Some(parentFieldType)
      }
      case Success(TypeClassT(typeTransform, implementation)) => {
        //eventually send typesInTypeClass to the type checker
        None
      }
      case Success(MapT(UMap(typeTransform), config)) => {
        // TODO - this is what has to change!!!
        if (typeTransform.length == 1) {
          Evaluator.asType(typeTransform.head._1, env).toOption
        } else {
          None
        }
        
        // Really we want a list of the type patterns in typeTransform, but that's going to require a larger change to the type checker
      }
      case other => None
    }
  }

  def outputTypeFromTypeClassParams(
    params: UMapPattern,
    implementation: Vector[(UntaggedObject, UntaggedObject)],
    input: UntaggedObject,
    inputType: NewMapType,
    env: Environment
  ): Outcome[NewMapType, String] = {
    val uMap = UMap(implementation)
    val untaggedInputType = env.typeSystem.typeToUntaggedObject(inputType)
    
    for {
      // Ensure that this type is a member of the type class
      _ <- Evaluator.applyFunctionAttempt(uMap, untaggedInputType, env)

      resultingFunctionType <- Evaluator.applyFunctionAttempt(UMap(Vector(params.key -> params.value)), untaggedInputType, env)
      resultingFunctionT <- env.typeSystem.convertToNewMapType(resultingFunctionType)

      outputT <- resultingFunctionT match {
        case MapT(typeTransform, config) => {
          for {
            typeAsObj <- Evaluator.applyFunctionAttempt(typeTransform, untaggedInputType, env, TypeMatcher)
            nType <- Evaluator.asType(typeAsObj, env)
            // TODO: This should be uncommented when we can build "convertible to" into the type transform
            //_ <- Outcome.failWhen(nType == UndefinedT, s"Couldn't apply type $inputType to type transform $typeTransform")
          } yield nType
        }
        case _ => {
          throw new Exception(s"Couldn't handle function type within typeclass: $resultingFunctionType")
        }
      }
    } yield outputT
  }

  def outputTypeFromStructParams(
    params: Vector[(UntaggedObject, UntaggedObject)],
    input: UntaggedObject,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      inputObj <- Evaluator(input, env)
      resultingType <- Evaluator.applyFunctionAttempt(UMap(params), inputObj, env)

      // This will (correctly) fail when resultingType == UInit (ie, it's not in params)
      resultingT <- Evaluator.asType(resultingType, env)
    } yield resultingT
  }

  def tagAndNormalizeObject(uObject: UntaggedObject, nTypeClass: NewMapType, env: Environment): Outcome[NewMapObject, String] = {
    uObject match {
      case UInit => {
        for {
          initValue <- CommandMaps.getDefaultValueOfCommandType(nTypeClass, env)
        } yield TaggedObject(initValue, nTypeClass)
      }
      case _ => {
        if (nTypeClass == CountT) {
          for {
            i <- normalizeCount(uObject)
          } yield TaggedObject(UIndex(i), nTypeClass)
        } else {
          Success(TaggedObject(uObject, nTypeClass))
        }
      }
    }
  }

  def normalizeCount(uObject: UntaggedObject): Outcome[Long, String] = {
    uObject match {
      case UInit => Success(0)
      case UCase(UIdentifier("Inc"), internal) => {
        normalizeCount(internal).map(i => i + 1)
      }
      case UIndex(i) => Success(i)
      case _ => Failure(s"Couldn't normalize count: $uObject")
    }
  }

  def retrieveFeatureSetFromFunctionTypePattern(nTypeClass: NewMapType, env: Environment): Outcome[MapFeatureSet, String] = {
    nTypeClass match {
      case StructT(_, _, _, featureSet) => Success(featureSet)
      case TypeClassT(_, _) => Success(PatternMap)
      case MapT(_, config) => Success(config.featureSet)
      case TypeT => Success(PatternMap) // This is for type classes
      case _ => Failure(s"Cannot retrieve meaningful feature set from object with type $nTypeClass")
    }
  }
}