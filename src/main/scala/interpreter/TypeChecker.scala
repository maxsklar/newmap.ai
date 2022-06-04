package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

object TypeChecker {
  case class TypeCheckResponse(
    nExpression: NewMapExpression,
    refinedTypeClass: UntaggedObject
  )

  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType This represents the typeClass that we expect the object to be
   *  If it's a single ObjectPattern - then that represents the common case where we are expecting a specific type
   *  The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is the environment of values upon which we are working
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: UntaggedObject,
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[TypeCheckResponse, String] = {
    val expectedTypeOutcome = getFinalUnderlyingType(expectedType, env)

    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    expression match {
      case NaturalNumberParse(i: Long) => {
        expectedTypeOutcome match {
          case Success(WildcardPatternT(_)) => {
            // BUT - the type will be abridged!!!
            Success(TypeCheckResponse(ObjectExpression(UIndex(i)), UType(CountT)))
          }
          case Success(t) => {
            val expectingType = SubtypeUtils.isTypeConvertible(t, TypeT, env).isSuccess

            for {
              _ <- TypeClassUtils.typeIsExpectingAnIndex(t, i, env)
            } yield {
              if (expectingType) {
                // TODO - replace this!
                TypeCheckResponse(ObjectExpression(UType(IndexT(i))), expectedType)
              } else {
                TypeCheckResponse(ObjectExpression(UIndex(i)), expectedType)
              }
            }
          }
          case _ => {
            // This is required if we recieve an "Any" type - which in the future won't happen
            // because this is going to be a type class and not an actual type
            Failure(s"Unexpected type with number: $expectedType")
            //Success(ObjectExpression(UIndex(i)))
          }
        }
      }
      case IdentifierParse(s: String, true) => {
        // We need to check that the expectedType allows an identifier!!
        expectedTypeOutcome match {
          case Success(WildcardPatternT(_)) => {
            Success(TypeCheckResponse(ObjectExpression(UIdentifier(s)), UType(IdentifierT)))
          }
          case Success(t) => {
            for {
              convertInstructions <- SubtypeUtils.isObjectConvertibleToType(TaggedObject(UIdentifier(s), IdentifierT), t, env)
              
              // TODO: Execute convert instructions?
            } yield {
              val uType = env.typeSystem.typeToUntaggedObject(t)
              TypeCheckResponse(ObjectExpression(UIdentifier(s)), uType)
            }
          }
          case _ => {
            Failure(s"Unexpected type with identifier: $expectedType")
          }
        }

      }
      case IdentifierParse(s: String, false) => {
        val useLiteralIdentifier = for {
          expectedT <- expectedType match {
            case UType(t) => Some(t)
            case _ => None
          }

          convertInstructions <- SubtypeUtils.isObjectConvertibleToType(TaggedObject(UIdentifier(s), IdentifierT), expectedT, env).toOption
        } yield ()

        if (useLiteralIdentifier.nonEmpty) {
          Success(TypeCheckResponse(ObjectExpression(UIdentifier(s)), expectedType))
        } else {
          env.lookup(s) match {
            case Some(EnvironmentParameter(nType)) => {
              for {
                convertInstructions <- TypeClassUtils.isPatternConvertibleToPattern(nType, expectedType, env)
                // TODO - execute convert instructions?
              } yield TypeCheckResponse(ParamId(s), nType)
            }
            case Some(EnvironmentBinding(nObject)) => {
              val nType = RetrieveType.fromNewMapObject(nObject, env)
              for { 
                convertInstructions <- TypeClassUtils.isObjectConvertibleToPattern(nObject, expectedType, env)

                // TODO - execute convert instructions on nObject
                uObject <- Evaluator.removeTypeTag(nObject)
              } yield {
                TypeCheckResponse(ObjectExpression(uObject), UType(nType))
              }
            }
            case None if (env.typeSystem.currentMapping.get(s).nonEmpty) => {
              typeCheck(
                ConstructCaseParse(expression, CommandList(Vector.empty)),
                UCase(UIdentifier("HistoricalType"), Uuuid(env.typeSystem.currentState)),
                env,
                featureSet
              )
            }
            case None => {
              expectedTypeOutcome match {
                case Success(CaseT(cases, IdentifierT, featureSet)) => {
                  for {
                    caseType <- Evaluator.applyFunctionAttempt(UMap(cases), UIdentifier(s), env)

                    // TODO - of course, we can formulize this better!
                    caseTypeIsEmptyStruct = caseType match {
                      case UType(StructT(params, _, _, _)) => params.isEmpty
                      case _ => false
                    }

                    isSingularType = (caseTypeIsEmptyStruct || (caseType == UType(IndexT(1))))
                    _ <- Outcome.failWhen(!isSingularType, s"Type Value cannot be inferred: $caseType")
                  } yield {
                    if (caseType == UType(IndexT(1))) {
                      TypeCheckResponse(ObjectExpression(UCase(UIdentifier(s), UIndex(0))), expectedType)
                    } else {
                      TypeCheckResponse(ObjectExpression(UCase(UIdentifier(s), UMap(Vector.empty))), expectedType)
                    }
                  }
                }
                /*case UType(ConstructedType(genericType, params)) => {
                  // TODO - copied from above - this should be collapsed in the next refactor
                  val genericTypeObj = Evaluator.stripVersioning(genericType, env)

                  genericTypeObj match {
                    case TaggedObject(UParametrizedCaseT(parameters, CaseT(cases, IdentifierT, featureSet)), _) => {
                      for {
                        caseType <- Evaluator.applyFunctionAttempt(UMap(cases), UIdentifier(s), env)

                        // TODO - of course, we can formulize this better!
                        caseTypeIsEmptyStruct = caseType match {
                          case UType(StructT(params, _, _, _)) => params.isEmpty
                          case _ => false
                        }

                        isSingularType = (caseTypeIsEmptyStruct || (caseType == UType(IndexT(1))))
                        _ <- Outcome.failWhen(!isSingularType, s"Type Value cannot be inferred: $caseType")
                      } yield {
                        if (caseType == UType(IndexT(1))) {
                          TypeCheckResponse(ObjectExpression(UCase(UIdentifier(s), UIndex(0))), expectedType)
                        } else {
                          TypeCheckResponse(ObjectExpression(UCase(UIdentifier(s), UMap(Vector.empty))), expectedType)
                        }
                      }
                    }
                    case _ => {
                      Failure(s"Unexpected generic type: $genericTypeObj")
                    }
                  }
                }*/
                case Success(WildcardPatternT(t)) => {
                  // TODO - this is in here temporarily to get some generic stuff to work
                  // eventually, take it out!
                  //throw new Exception(s"Identifier $s is unknown, expecting type class $expectedType")
                  Success(TypeCheckResponse(ParamId(s), expectedType))
                }
                case Success(nType) => {
                  val trialObject = TaggedObject(UIdentifier(s), IdentifierT)
                  for {
                    convertInstructions <- SubtypeUtils.isObjectConvertibleToType(trialObject, nType, env)
                    
                    // TODO: execute convertInstructions?
                  } yield {
                    TypeCheckResponse(ObjectExpression(UIdentifier(s)), expectedType)
                  }
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

          // Validate that this is allowed from the feature set
          _ <- Outcome.failWhen(
            !SubtypeUtils.isFeatureSetConvertible(functionFeatureSet, featureSet),
            s"Cannot allow function with feature set $functionFeatureSet in expression that should be featureSet $featureSet"
          )

          // If the function is a parameter, then there's no guarantee that it's not self-referential
          // TODO - flesh out these rules a little bit more
          _ <- Outcome.failWhen(
            !RetrieveType.isTermClosedLiteral(result.functionExpression) && (featureSet != FullFunction),
            s"Function ${result.functionExpression} is based on a parameter, which could create a self-referential definition, disallowed in featureSet $featureSet"
          )

          // TODO - execute convertInstructions
          convertInstructions <- TypeClassUtils.isPatternConvertibleToPattern(result.resultingType, expectedType, env)
        } yield {
          TypeCheckResponse(ApplyFunction(result.functionExpression, result.inputExpression), result.resultingType)
        }
      }
      case CommandList(values: Vector[ParseTree]) => {
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
              TypeCheckResponse(BuildMapInstance(result), expectedType)
            }
          }
          case Success(MapT(typeTransform, config)) => {
            for {
              mapValues <- typeCheckGenericMap(values, typeTransform, config.featureSet, env, featureSet)

              _ <- Outcome.failWhen(typeTransform.length > 1, s"Could not handle type transform: $typeTransform")

              isCovered <- {
                if (config.completeness != RequireCompleteness) Success(true)
                else if (typeTransform.length == 0) Success(true)
                else {
                  SubtypeUtils.doPatternsCoverType(mapValues.map(_._1), typeTransform.head._1, env)
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + typeTransform.head._1
              )
            } yield {
              TypeCheckResponse(BuildMapInstance(mapValues), expectedType)
            }
          }
          case Success(TypeT) => {
            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // OR we are just being given a list of types
            // TODO - can this be simplified by combining with the MapT section above?
            {
              for {
                mapValues <- typeCheckMap(values, UType(IdentifierT), UType(TypeT), BasicMap, env, featureSet)
              } yield {
                TypeCheckResponse(
                  BuildStructT(BuildMapInstance(mapValues), IdentifierT, RequireCompleteness, BasicMap),
                  expectedType
                )
              }
            }.rescue(f => {
              for {
                expressions <- typeCheckSequence(values, TypeT, env)
              } yield {
                val indexType = IndexT(expressions.length)
                TypeCheckResponse(BuildStructT(
                  BuildMapInstance(
                    expressions.zipWithIndex.map(x => UIndex(x._2) -> x._1),
                  ),
                  indexType,
                  RequireCompleteness,
                  BasicMap
                ), expectedType)
              }
            })
          }
          case Success(WildcardPatternT(_)) => {
            Failure(s"CommandLists must be explicitly typed - $values")
          }
          /*case Success(UMapTPattern(inputP, outputP, config)) => {
            for {
              mapValues <- typeCheckMap(values, inputP, outputP, config.featureSet, env, featureSet)

              isCovered <- {
                if (config.completeness != RequireCompleteness) Success(true)
                else {
                  SubtypeUtils.doPatternsCoverType(mapValues.map(_._1), inputP, env)
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + inputP
              )
            } yield {
              // TODO - if the outputP is a command, then append the default value on the end of this (even if a basic map!)
              TypeCheckResponse(BuildMapInstance(mapValues), expectedType)
            }
          }
          case Success(UStruct(typePatterns)) => {
            for {
              result <- typeCheckStruct(
                typePatterns.zipWithIndex.map(x => UIndex(x._2) -> patternToExpression(x._1)),
                IndexT(typePatterns.length), // Is this right - or do we need to pass in the subset??
                values,
                env,
                featureSet
              )
            } yield {
              TypeCheckResponse(BuildMapInstance(result), expectedType)
            }
          }*/
          case _ => {
            Failure(s"CommandLists not working yet with this expected type: $values exp: $expectedType")
          }
        }
      }
      case BindingCommandItem(key, value) => {
        typeCheck(CommandList(Vector(expression)), expectedType, env, featureSet)
      }
      case LambdaParse(input, output) => {
        // Make sure that this fits expectedType!
        for {
          inputType <- typeCheck(input, UType(TypeT), env, featureSet)
          outputType <- typeCheck(output, UType(TypeT), env, featureSet)
        } yield {
          // TODO - how do we indicate completeness and featureSet in the LambdaParse Symbol?
          TypeCheckResponse(BuildSimpleMapT(
            inputType.nExpression,
            outputType.nExpression,
            MapConfig(RequireCompleteness, FullFunction)
          ), UType(TypeT))
        }
      }
      case ConstructCaseParse(first, second) => {
        expectedTypeOutcome match {
          case Success(CaseT(simpleMap, parentFieldType, _)) => {
            for {
              firstExp <- typeCheck(first, UType(parentFieldType), env, featureSet)

              // TODO - we must ensure that the evaluator is not evaluating anything too complex here
              // must be a "simple map" type situation
              // can this be built into the evaluator?
              firstObj <- Evaluator(firstExp.nExpression, env)

              secondType <- Evaluator.applyFunctionAttempt(UMap(simpleMap), firstObj, env)
              secondT <- Evaluator.asType(secondType, env)
              secondExp <- typeCheck(second, UType(secondT), env, featureSet)
            } yield {
              TypeCheckResponse(BuildCase(firstObj, secondExp.nExpression), expectedType)
            }
          }
          /*case Success(UType(ConstructedType(genericType, params))) => {
            for {
              uGenericType <- Evaluator.removeTypeTag(genericType)
              result <- typeCheckCaseAgainstConstructedType(first, second, uGenericType, params, featureSet, env)
            } yield {
              TypeCheckResponse(result, expectedType)
            }
          }*/
          case Success(HistoricalTypeT(uuid)) => {
            for {
              result <- typeCheckCaseAsType(first, second, uuid, featureSet, env)
            } yield TypeCheckResponse(result, expectedType)
          }
          case Success(TypeT) => {
            for {
              result <- typeCheckCaseAsType(first, second, env.typeSystem.currentState, featureSet, env)
            } yield TypeCheckResponse(result, expectedType)
          }
          /*case Success(CustomT(genericType, params)) => {
            for {
              result <- typeCheckCaseAgainstConstructedType(first, second, genericType, params, featureSet, env)
            } yield {
              TypeCheckResponse(result, expectedType)
            }
          }*/
          case Success(WildcardPatternT(_)) => {
            // This means that the first object must be some kind of constructor
            // For now, that's got to be UParametrizedCaseT
            for {
              firstExp <- typeCheck(first, UWildcardPattern("_"), env, featureSet)

              firstObj <- Evaluator(firstExp.nExpression, env)

              firstObjStripped = Evaluator.stripVersioningU(firstObj, env)

              secondT <- firstObjStripped match {
                case UParametrizedCaseT(parameters, caseT) => {
                  val params = parameters.zipWithIndex.map(x => UIndex(x._2) -> ObjectExpression(UType(x._1._2)))
                  Success(StructT(params, IndexT(parameters.length)))
                }
                case _ => Failure(s"Not a constructor: $firstExp")
              }

              secondExp <- typeCheck(second, UType(secondT), env, featureSet)
            } yield {
              TypeCheckResponse(BuildCase(firstObj, secondExp.nExpression), UType(TypeT))
            }
          }
          case nTypeOutcome => {
            Failure(s"Case type must be specified for $expression -- instead got $nTypeOutcome")
          }
        }
      }
    }
  }

  // This map could include pattern matching
  def typeCheckMap(
    values: Vector[ParseTree],
    keyTypeP: UntaggedObject,
    valueTypeP: UntaggedObject,
    internalFeatureSet: MapFeatureSet,
    env: Environment,
    externalFeatureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[Vector[(UntaggedObject, NewMapExpression)], String] = {
    val typeTransform = Vector(keyTypeP -> patternToExpression(valueTypeP))
    typeCheckGenericMap(values, typeTransform, internalFeatureSet, env, externalFeatureSet)
  }

  def typeCheckSequence(
    values: Vector[ParseTree],
    valueType: NewMapType,
    env: Environment,
  ): Outcome[Vector[NewMapExpression], String] = {
    values match {
      case BindingCommandItem(k, v) +: _ => {
        Failure(s"Sequences don't work with binding command item $k : $v")
      }
      case value +: restOfValues => {
        for {
          objectFoundValue <- typeCheck(value, UType(valueType), env, BasicMap)
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
    typeTransform: Vector[(UntaggedObject, NewMapExpression)],
    internalFeatureSet: MapFeatureSet,
    env: Environment,
    externalFeatureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[Vector[(UntaggedObject, NewMapExpression)], String] = {
    val inputTypeClass = if (typeTransform.length == 1) {
      typeTransform.head._1
    } else {
      throw new Exception(s"Can't work with type transform if it doesn't have exactly 1 pattern (because it's unimplemented): $typeTransform")
    }

    val outputExpression = typeTransform.head._2

    //println(s"In typeCheckGenericMap: $values -- $typeTransform")
    
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, inputTypeClass, env, externalFeatureSet, internalFeatureSet)

          foundKeyPattern = resultKey.typeCheckResult

          //_ = println(s"k: $k\n -- resultKey: ${resultKey}\n --outputExpression: $outputExpression")

          valueTypePattern <- getValueTypePattern(resultKey.expectedTypeRefinement, outputExpression, env)

          //_ = println(s"valueTypePattern: $valueTypePattern")

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueTypePattern,
            resultKey.newEnvironment,
            featureSet = internalFeatureSet
          )

          //_ = println(s"objectFoundValue: $objectFoundValue")

          restOfMap <- typeCheckGenericMap(restOfValues, typeTransform, internalFeatureSet, env, externalFeatureSet)
        } yield {
          (resultKey.typeCheckResult -> objectFoundValue.nExpression) +: restOfMap
        }
      }
      case s +: _ => {
        Failure(s"No binding found in map for item $s in $values")
      }
      case _ => Success(Vector.empty)
    }
  }

  /*def typeCheckCaseAgainstConstructedType(
    first: ParseTree,
    second: ParseTree,
    genericType: String,
    genericTypeParams: UntaggedObject,
    featureSet: MapFeatureSet,
    env: Environment
  ): Outcome[NewMapExpression, String] = {
    for {
      underlyingT <- getFinalUnderlyingType(genericType, genericTypeParams, env)

      fullCaseType <- underlyingT match {
        /*case upt@UParametrizedCaseT(_, _) => {
          Success(upt)
        }*/
        case caseT@CaseT(cases, parentType, _) => Success(caseT)
        //case UCase(caseName, caseParams) => 
        case _ => Failure(s"Unexpected generic type: $genericType")
      }

      firstExp <- typeCheck(first, UType(fullCaseType.fieldParentType), env, featureSet)

      // TODO - we must ensure that the evaluator is not evaluating anything too complex here
      // (see above?)
      firstObj <- Evaluator(firstExp.nExpression, env)

      secondTypeExpression <- Evaluator.attemptPatternMatchInOrder(fullCaseType.cases, firstObj, env)

      /*parameterNames = fullCaseType.parameters.map(_._1)

      parameterPattern = UStruct(fullCaseType.parameters.map(p => UWildcardPattern(p._1)))

      secondTypeParameters <- Evaluator.attemptPatternMatch(parameterPattern, genericTypeParams, env)
      //Map[String, UntaggedObject]
      secondTypeParamsAsExpressions = secondTypeParameters.toVector.map(x => (x._1 -> patternToExpression(x._2))).toMap

      //parameters: Map[String, UntaggedObject],
      // Plug in the params to get secondTypeObject
      //secondType <- Evaluator(MakeSubstitution(secondTypeExpression, secondTypeParamsAsExpressions), env)
      //secondT <- Evaluator.asType(secondType, env)
      secondTypeExpressionSubst = MakeSubstitution(secondTypeExpression, secondTypeParamsAsExpressions)*/
      //secondTypePattern <- expressionToPattern(secondTypeExpressionSubst)
      secondTypePattern <- expressionToPattern(secondTypeExpression)

      secondExp <- typeCheck(second, secondTypePattern/*UType(secondT)*/, env, featureSet)
    } yield {
      BuildCase(firstObj, secondExp.nExpression)
    }
  }*/

  def getUnderlyingType(name: String, params: UntaggedObject, env: Environment): Outcome[NewMapType, String] = {
    val typeSystem = env.typeSystem
    for {
      typeId <- Outcome(typeSystem.currentMapping.get(name), s"$name must be defined")
      underlyingTypeInfo <- Outcome(typeSystem.typeToUnderlyingType.get(typeId), s"Couldn't find underlying type for $name")

      parameterPattern = underlyingTypeInfo._1
      genericUnderlyingType = underlyingTypeInfo._2

      substitutions <- Evaluator.attemptPatternMatch(parameterPattern, params, env)

      underlyingType = MakeSubstitution.substObject(genericUnderlyingType, substitutions)
      underlyingT <- typeSystem.convertToNewMapType(underlyingType)
    } yield underlyingT
  }

  def getFinalUnderlyingType(typeClass: UntaggedObject, env: Environment): Outcome[NewMapType, String] = {
    for {
      nType <- env.typeSystem.convertToNewMapType(typeClass)

      result <- nType match {
        case CustomT(name, params) => {
          for {
            partialResult <- getUnderlyingType(name, params, env)
            finalizeResult <- getFinalUnderlyingType(env.typeSystem.typeToUntaggedObject(partialResult), env)
          } yield finalizeResult
        }
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
  ): Outcome[NewMapExpression, String] = {
    for {
      firstAsString <- first match {
        case IdentifierParse(s, false) => Success(s)
        case _ => Failure(s"Constructor for type must be an identifier, it was $first")
      }

      typeOfParameter <- env.typeSystem.getParameterType(env.typeSystem.currentState, firstAsString)
      uTypeOfParameter = env.typeSystem.typeToUntaggedObject(typeOfParameter)

      paramValue <- typeCheck(second, uTypeOfParameter, env, featureSet)
    } yield BuildCase(UIdentifier(firstAsString), paramValue.nExpression)
  }

  def patternToExpression(nPattern: UntaggedObject): NewMapExpression = {
    nPattern match {
      case UWildcardPattern(name) => ParamId(name)
      case UStruct(params) => {
        val structOrdering = params.zipWithIndex.map(param => {
          UIndex(param._2) -> patternToExpression(param._1)
        })

        BuildMapInstance(structOrdering)
      }
      case UCase(constructor, input) => BuildCase(constructor, patternToExpression(input))
      case UMapTPattern(input, output, config) => {
        BuildSimpleMapT(patternToExpression(input), patternToExpression(output), config)
      }
      case uObject => ObjectExpression(uObject)
    }
  }

  def expressionToPattern(nExpression: NewMapExpression): Outcome[UntaggedObject, String] = {
    nExpression match {
      case ObjectExpression(uObject) => Success(uObject)
      case ParamId(name) => Success(UWildcardPattern(name))
      case BuildMapInstance(structOrdering) => {
        for {
          patternList <- structExpressionToPattern(structOrdering)
        } yield UStruct(patternList)
      }
      case BuildCase(constructor, inputExpression) => {
        for {
          inputPattern <- expressionToPattern(inputExpression)
        } yield UCase(constructor, inputPattern)
      }
      case BuildStructT(values, parentFieldType, completeness, featureSet) => {
        expressionToPattern(values)
      }
      case _ => Failure(s"Couldn't convert expression to pattern $nExpression")
    }
  }

  def structExpressionToPattern(structOrdering: Vector[(UntaggedObject, NewMapExpression)]) : Outcome[Vector[UntaggedObject], String] = {
    structOrdering match {
      case first +: rest => {
        for {
          firstPattern <- expressionToPattern(first._2)
          restOfPatterns <- structExpressionToPattern(rest)
        } yield firstPattern +: restOfPatterns
      }
      case _ => Success(Vector.empty)
    }
  }

  // Eventually replace this with an "applyFunctionAttempt" variant which can apply patterns to patterns
  def getValueTypePattern(
    foundKeyPattern: UntaggedObject,
    outputExpression: NewMapExpression,
    env: Environment
  ): Outcome[UntaggedObject, String] = {
    //println(s"In getValueTypePattern: $foundKeyPattern -- $outputExpression")

    foundKeyPattern match {
      case UType(t) => {
        for {
          result <- Evaluator(outputExpression, env)
          _ <- Evaluator.asType(result, env)
        } yield result
      }
      case UCase(uConstructor, inputPattern) => {
        getValueTypePattern(inputPattern, outputExpression, env)
      }
      case UWildcardPattern(s) => {
        typeExpressionToPattern(outputExpression, env)
      }
      case _ => {
        // TODO - needs a better implementation!!
        Success(UWildcardPattern("_"))
      }
    }
  }

  def typeExpressionToPattern(nExpression: NewMapExpression, env: Environment): Outcome[UntaggedObject, String] = {
    nExpression match {
      case ObjectExpression(obj) => Success(obj)
      case ParamId(s) => Success(UWildcardPattern(s))
      case ApplyFunction(ObjectExpression(UMap(values)), inputExpression) => {

        for {
          inputPattern <- expressionToPattern(inputExpression)
          exp <- Evaluator.attemptPatternMatchInOrder(values, inputPattern, env)
          result <- typeExpressionToPattern(exp, env)
        } yield result
        

        //Failure(s"Need to match values with inputs: $values -- $inputs")

        // if inputs is a struct, turn it into a struct pattern!!


        // Look for a struct pattern, with coutning ObjectPattern inputs!!

        //Vector((StructPattern(Vector(WildcardPattern(key), WildcardPattern(value))),BuildMapT(ParamId(key),ParamId(value),MapConfig(ai.newmap.model.RequireCompleteness$@6aecb083,ai.newmap.model.SimpleFunction$@3c8c4bd8,Vector())))) --
        //Vector((ObjectPattern(0),ParamId(T)), (ObjectPattern(1),ParamId(T)))
      }
      case BuildSimpleMapT(inputExp, outputExp, config) => {
        for {
          inputP <- typeExpressionToPattern(inputExp, env)
          outputP <- typeExpressionToPattern(outputExp, env)
        } yield UMapTPattern(inputP, outputP, config)
      }
      case _ => {
        Failure(s"type expression to pattern unimplemented: $nExpression")

        //type expression to pattern unimplemented: 

      }
    }
  }

  case class TypeCheckWithPatternMatchingResult(
    typeCheckResult: UntaggedObject,
    expectedTypeRefinement: UntaggedObject,
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: UntaggedObject,
    env: Environment,
    externalFeatureSet: MapFeatureSet, // Am I inside a map with restricted features already?
    internalFeatureSet: MapFeatureSet // Which feature set is this map allowed to use
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    val patternMatchingAllowed = internalFeatureSet != BasicMap
    //val parentTypeIsIdentifier = typeIsExpectingAnIdentifier(expectedType, env)

    val expectedTypeOutcome = getFinalUnderlyingType(expectedType, env)

    (expression, expectedTypeOutcome) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed /*&& !parentTypeIsIdentifier*/) => {
        Success(
          TypeCheckWithPatternMatchingResult(UWildcardPattern(s), expectedType, env.newParamTypeClass(s, expectedType))
        )
      }
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (CommandList(values), Success(StructT(structValues, parentFieldType, _, _))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, externalFeatureSet, internalFeatureSet, env)
        } yield {
          TypeCheckWithPatternMatchingResult(UStruct(tcmp.patterns), expectedType, tcmp.newEnvironment)
        }
      }
      case (ConstructCaseParse(constructorP, input), Success(CaseT(cases, parentFieldType, _))) if (patternMatchingAllowed) => {
        for {
          constructorTC <- typeCheck(constructorP, UType(parentFieldType), env, BasicMap)
          constructor <- Evaluator(constructorTC.nExpression, env)
          inputTypeExpected <- Evaluator.applyFunctionAttempt(UMap(cases), constructor, env)
          inputTExpected <- Evaluator.asType(inputTypeExpected, env)

          result <- typeCheckWithPatternMatching(input, UType(inputTExpected), env, externalFeatureSet, internalFeatureSet)
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
          constructorTC <- typeCheckUnknownType(constructorP, env)

          constructor <- Evaluator(constructorTC.nExpression, env)

          parametrizedCaseT <- Evaluator.stripVersioningU(constructor, env) match {
            case pcase@UParametrizedCaseT(_, _) => Success(pcase) 
            case _ => Failure(s"unexpected type constructo $constructor")
          }

          parameters = parametrizedCaseT.parameters

          params = parameters.zipWithIndex.map(x => UIndex(x._2) -> ObjectExpression(UType(x._1._2)))
          inputTypePatternExpected = StructT(params, IndexT(parameters.length))

          result <- typeCheckWithPatternMatching(
            input,
            UType(inputTypePatternExpected),
            env,
            externalFeatureSet,
            internalFeatureSet
          )
        } yield {
          TypeCheckWithPatternMatchingResult(
            UCase(constructor, result.typeCheckResult),
            expectedType,
            result.newEnvironment
          )
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env, externalFeatureSet)
          tcPattern <- expressionToPattern(tc.nExpression)
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
    expressions: Vector[(ParseTree, NewMapExpression)],
    externalFeatureSet: MapFeatureSet,
    internalFeatureSet: MapFeatureSet,
    env: Environment
  ): Outcome[TypeCheckWithMultiplePatternMatchingResult, String] = {
    expressions match {
      case (expression, nTypeExp) +: restOfExpressions => {
        for {
          nTypeObj <- Evaluator(nTypeExp, env)
          nType <- Evaluator.asType(nTypeObj, env)
          response <- typeCheckWithPatternMatching(expression, UType(nType), env, externalFeatureSet, internalFeatureSet)
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
    parameterList: Vector[(UntaggedObject, NewMapExpression)],
    nTypeForStructFieldName: NewMapType,
    valueList: Vector[ParseTree],
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[Vector[(UntaggedObject, NewMapExpression)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        for {
          valueId <- typeCheck(valueIdentifier, UType(nTypeForStructFieldName), env, featureSet)
          valueIdObj <- Evaluator(valueId.nExpression, env)

          newParams <- Evaluator.attemptPatternMatch(paramId, valueIdObj, env)

          // Is this substitution neccesary??
          typeOfIdentifierObj <- Evaluator(MakeSubstitution(typeOfIdentifier, newParams), env)
          typeOfIdentifierT <- Evaluator.asType(typeOfIdentifierObj, env)

          tc <- typeCheck(valueObject, UType(typeOfIdentifierT), env, featureSet)

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
          tc <- typeCheck(valueObject, UType(typeOfIdentifierT), env, featureSet)
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
    typeCheck(expression, UWildcardPattern("_"), env, FullFunction)
  }

  case class TypeCheckUnknownFunctionResult(
    functionExpression: NewMapExpression,
    typeOfFunction: UntaggedObject,
    inputExpression: NewMapExpression,
    resultingType: UntaggedObject
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
          inputTypeChecked <- typeCheck(input, UType(inputT), env, FullFunction)
        } yield (inputTypeChecked.nExpression -> inputTypeChecked.refinedTypeClass)
        case None => {
          for {
            typeCheckUnknownTypeResult <- typeCheckUnknownType(input, env)
          } yield (typeCheckUnknownTypeResult.nExpression -> typeCheckUnknownTypeResult.refinedTypeClass)
        }
      }

      (inputTC, inputType) = inputTagged

      resultingType <- typeOfFunction match {
        //case UType(MapT(_, outputType, _)) => Success(outputType)
        case UType(StructT(params, _, _, _)) => outputTypeFromStructParams(params, inputTC, env)
        case UType(TypeClassT(typeTransform, typesInTypeClass)) => {
          outputTypeFromTypeClassParams(typeTransform, typesInTypeClass, inputTC, env)
        }
        case UType(MapT(typeTransform, config)) => {
          for {
            typeAsObj <- Evaluator.applyFunctionAttempt(UMap(typeTransform), inputType, env)
            nType <- Evaluator.asType(typeAsObj, env)
          } yield {
            nType
          }
        }
        case _ => Failure(s"Cannot get resulting type from function type ${functionTypeChecked.refinedTypeClass}")
      }
    } yield {
      TypeCheckUnknownFunctionResult(functionTypeChecked.nExpression, typeOfFunction, inputTC, UType(resultingType))
    }    
  }

  // Returns a set of patterns representing newmap types
  // TODO - it should not return an option of NewMapType, rather some patterns!!
  def inputTypeCheckFromFunctionType(nFunctionTypeClass: UntaggedObject, env: Environment): Option[NewMapType] = {
    stripCustomTag(nFunctionTypeClass, env.typeSystem) match {
      //case UType(MapT(inputType, _, _)) => Some(inputType)
      case Success(UType(StructT(params, parentFieldType, _, _))) => {
        Some(parentFieldType)
      }
      case Success(UType(TypeClassT(typeTransform, typesInTypeClass))) => {
        //eventually send typesInTypeClass to the type checker
        Some(TypeT)
        /*SubtypeT(
          UMap(typesInTypeClass.map(x => (x -> ObjectExpression(UIndex(1))))),
          TypeT,
          SimpleFunction
        )*/
      }
      case Success(UType(MapT(typeTransform, config))) => {
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
    params: Vector[(UntaggedObject, NewMapExpression)],
    typesInTypeClass: Vector[UntaggedObject],
    input: NewMapExpression,
    env: Environment
  ): Outcome[NewMapType, String] = {
    val uMap = UMap(typesInTypeClass.map(pattern => pattern -> ObjectExpression(UIndex(1))))
    
    for {
      inputObj <- Evaluator(input, env)

      // Ensure that this type is a member of the type class
      _ <- Evaluator.applyFunctionAttempt(uMap, inputObj, env)

      resultingType <- Evaluator.applyFunctionAttempt(UMap(params), inputObj, env)

      // This will (correctly) fail when resultingType == UInit (ie, it's not in params)
      resultingT <- Evaluator.asType(resultingType, env)
    } yield resultingT
  }

  def outputTypeFromStructParams(
    params: Vector[(UntaggedObject, NewMapExpression)],
    input: NewMapExpression,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      inputObj <- Evaluator(input, env)
      resultingType <- Evaluator.applyFunctionAttempt(UMap(params), inputObj, env)

      // This will (correctly) fail when resultingType == UInit (ie, it's not in params)
      resultingT <- Evaluator.asType(resultingType, env)
    } yield resultingT
  }

  def tagAndNormalizeObject(uObject: UntaggedObject, nTypeClass: UntaggedObject, env: Environment): Outcome[NewMapObject, String] = {
    env.typeSystem.convertToNewMapType(nTypeClass) match {
      case Success(nType) => {
        uObject match {
          case UInit => {
            for {
              initValue <- CommandMaps.getDefaultValueOfCommandType(nTypeClass, env)
            } yield TaggedObject(initValue, nType)
          }
          case _ => Success(TaggedObject(uObject, nType))
        }
      }
      case _ => {
        Failure(s"Cannot tag object with unknown final type $nTypeClass")
      }
    }
  }

  def stripCustomTag(nTypePattern: UntaggedObject, typeSystem: NewMapTypeSystem): Outcome[UntaggedObject, String] = {
    nTypePattern match {
      case UType(CustomT(name, params)) => {
        for {
          currentTypeId <- Outcome(typeSystem.currentMapping.get(name), s"$name must be defined")
          currentUnderlyingType <- Outcome(typeSystem.typeToUnderlyingType.get(currentTypeId), s"Couldn't find underlying type for $name")
        } yield currentUnderlyingType._2
      }
      case _ => Success(nTypePattern)
    }
  }

  def retrieveFeatureSetFromFunctionTypePattern(nTypeClass: UntaggedObject, env: Environment): Outcome[MapFeatureSet, String] = {
    nTypeClass match {
      case UType(StructT(_, _, _, featureSet)) => Success(featureSet)
      case UType(TypeClassT(_, _)) => Success(SimpleFunction)
      //case UType(CustomT(_, t)) => retrieveFeatureSetFromFunctionTypePattern(UType(t), env)
      case UType(MapT(_, config)) => Success(config.featureSet)
      case _ => Failure(s"Cannot retrieve meaningful feature set from object with type $nTypeClass")
    }
  }
}