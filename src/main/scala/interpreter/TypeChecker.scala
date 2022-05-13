package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  case class TypeCheckResponse(
    nExpression: NewMapExpression,
    refinedTypeClass: NewMapPattern
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
    expectedType: NewMapPattern,
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[TypeCheckResponse, String] = {
    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    expression match {
      case NaturalNumberParse(i: Long) => {
        expectedType match {
          case ObjectPattern(UType(t)) => {
            for {
              _ <- TypeClassUtils.typeIsExpectingAnIndex(t, i, env)
            } yield {
              TypeCheckResponse(ObjectExpression(UIndex(i)), expectedType)
            }
          }
          case WildcardPattern(_) => {
            // BUT - the type will be abridged!!!
            Success(TypeCheckResponse(ObjectExpression(UIndex(i)), ObjectPattern(UType(CountT))))
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
        expectedType match {
          case ObjectPattern(UType(t)) => {
            val expectingAnIdentifier = TypeClassUtils.typeIsExpectingAnIdentifier(t, s, env).toOption.nonEmpty
            if (expectingAnIdentifier) {
              Success(TypeCheckResponse(ObjectExpression(UIdentifier(s)), expectedType))
            } else {
              Failure(s"Type $expectedType not expecting identifier $s")
            }
          }
          case WildcardPattern(_) => {
            Success(TypeCheckResponse(ObjectExpression(UIdentifier(s)), ObjectPattern(UType(IdentifierT))))
          }
          case _ => {
            Failure(s"Unexpected type with identifier: $expectedType")
          }
        }

      }
      case IdentifierParse(s: String, false) => {
        val useLiteralIdentifier = for {
          expectedT <- expectedType match {
            case ObjectPattern(UType(t)) => Some(t)
            case _ => None
          }

          _ <- TypeClassUtils.typeIsExpectingAnIdentifier(expectedT, s, env).toOption
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
                TypeCheckResponse(ObjectExpression(uObject), ObjectPattern(UType(nType)))
              }
            }
            case None => {
              Failure(s"Identifier $s is unknown, expecting type class $expectedType")
            }
          }
        }
      }
      case ApplyParse(function, input) => {
        for {
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
        stripCustomTag(expectedType) match {
          case ObjectPattern(UType(MapT(keyTypeT, valueT, config))) => {
            for {
              mapValues <- typeCheckMap(values, keyTypeT, valueT, config.featureSet, env, featureSet)

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
              // TODO - if the valueT is a command, then append the default value on the end of this (even if a basic map!)
              TypeCheckResponse(BuildMapInstance(mapValues), expectedType)
            }
          }
          case ObjectPattern(UType(StructT(parameterList, parentFieldType, _, _, _))) => {
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
          case ObjectPattern(UType(GenericMapT(typeTransform, config))) => {
            for {
              mapValues <- typeCheckGenericMap(values, typeTransform, config.featureSet, env, featureSet)
            } yield {
              TypeCheckResponse(BuildMapInstance(mapValues), expectedType)
            }
          }
          case ObjectPattern(UType(TypeT)) => {
            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // OR we are just being given a list of types
            // TODO - can this be simplified by combining with the MapT section above?
            {
              for {
                mapValues <- typeCheckMap(values, IdentifierT, TypeT, BasicMap, env, featureSet)
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
                    expressions.zipWithIndex.map(x => ObjectPattern(UIndex(x._2)) -> x._1),
                  ),
                  indexType,
                  RequireCompleteness,
                  BasicMap
                ), expectedType)
              }
            })
          }
          case WildcardPattern(_) => {
          //case ObjectPattern(UType(AnyT)) => {
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
        // Make sure that this fits expectedType!
        for {
          inputType <- typeCheck(input, ObjectPattern(UType(TypeT)), env, featureSet)
          outputType <- typeCheck(output, ObjectPattern(UType(TypeT)), env, featureSet)
        } yield {
          // TODO - how do we indicate completeness and featureSet in the LambdaParse Symbol?
          TypeCheckResponse(BuildMapT(
            inputType.nExpression,
            outputType.nExpression,
            MapConfig(RequireCompleteness, FullFunction)
          ), ObjectPattern(UType(TypeT)))
        }
      }
      case ConstructCaseParse(first, second) => {
        stripCustomTag(expectedType) match {
          case ObjectPattern(UType(CaseT(simpleMap, parentFieldType, _, _))) => {
            for {
              firstExp <- typeCheck(first, ObjectPattern(UType(parentFieldType)), env, featureSet)

              // TODO - we must ensure that the evaluator is not evaluating anything too complex here
              // must be a "simple map" type situation
              // can this be built into the evaluator?
              firstObj <- Evaluator(firstExp.nExpression, env)

              secondType <- Evaluator.applyFunctionAttempt(UMap(simpleMap), firstObj, env)
              secondT <- Evaluator.asType(secondType, env)
              secondExp <- typeCheck(second, ObjectPattern(UType(secondT)), env, featureSet)
            } yield {
              TypeCheckResponse(BuildCase(firstObj, secondExp.nExpression), expectedType)
            }
          }
          case _ => {
            Failure(s"Case type must be specified for $expression")
          }
        }
      }
    }
  }

  // This map could include pattern matching
  def typeCheckMap(
    values: Vector[ParseTree],
    keyType: NewMapType,
    valueType: NewMapType,
    internalFeatureSet: MapFeatureSet,
    env: Environment,
    externalFeatureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    val typeTransform = Vector(ObjectPattern(UType(keyType)) -> ObjectExpression(UType(valueType)))
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
          objectFoundValue <- typeCheck(value, ObjectPattern(UType(valueType)), env, BasicMap)
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
    typeTransform: Vector[(NewMapPattern, NewMapExpression)],
    internalFeatureSet: MapFeatureSet,
    env: Environment,
    externalFeatureSet: MapFeatureSet // This is the external feature set, the map feature set can be found in mapT
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    val inputTypeClass = if (typeTransform.length == 1) {
      typeTransform.head._1
    } else {
      throw new Exception(s"Can't work with type transform if it doesn't have exactly 1 pattern (because it's unimplemented): $typeTransform")
    }

    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, inputTypeClass, env, externalFeatureSet, internalFeatureSet)
          
          foundKeyPattern = resultKey.typeCheckResult

          valueTypePattern <- getValueTypePattern(resultKey.expectedTypeRefinement, typeTransform, env)

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
          (resultKey.typeCheckResult -> objectFoundValue.nExpression) +: restOfMap
        }
      }
      case s +: _ => {
        Failure(s"No binding found in map for item $s in $values")
      }
      case _ => Success(Vector.empty)
    }
  }

  def getValueTypePattern(
    foundKeyPattern: NewMapPattern,
    typeTransform: Vector[(NewMapPattern, NewMapExpression)],
    env: Environment
  ): Outcome[NewMapPattern, String] = {
    foundKeyPattern match {
      case ObjectPattern(UType(t)) => {
        for {
          result <- Evaluator.applyFunctionAttempt(UMap(typeTransform), UType(t), env)
          _ <- Evaluator.asType(result, env)
        } yield ObjectPattern(result)
      }
      case _ => {
        // TODO - needs a better implementation!!
        Success(WildcardPattern("_"))
      }
    }
  }

  case class TypeCheckWithPatternMatchingResult(
    typeCheckResult: NewMapPattern,
    expectedTypeRefinement: NewMapPattern,
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapPattern,
    env: Environment,
    externalFeatureSet: MapFeatureSet, // Am I inside a map with restricted features already?
    internalFeatureSet: MapFeatureSet // Which feature set is this map allowed to use
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    val patternMatchingAllowed = internalFeatureSet != BasicMap
    //val parentTypeIsIdentifier = typeIsExpectingAnIdentifier(expectedType, env)

    (expression, stripCustomTag(expectedType)) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed /*&& !parentTypeIsIdentifier*/) => {
        Success(
          TypeCheckWithPatternMatchingResult(WildcardPattern(s), expectedType, env.newParamTypeClass(s, expectedType))
        )
      }
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (CommandList(values), ObjectPattern(UType(StructT(structValues, parentFieldType, _, _, _)))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, externalFeatureSet, internalFeatureSet, env)
        } yield {
          TypeCheckWithPatternMatchingResult(StructPattern(tcmp.patterns), expectedType, tcmp.newEnvironment)
        }
      }
      case (ConstructCaseParse(constructorP, input), ObjectPattern(UType(CaseT(cases, parentFieldType, _, _)))) if (patternMatchingAllowed) => {
        // TODO - shouldn't this be a subtype??
        val caseConstructorType = parentFieldType

        for {
          constructorTC <- typeCheck(constructorP, ObjectPattern(UType(parentFieldType)), env, BasicMap)
          constructor <- Evaluator(constructorTC.nExpression, env)
          inputTypeExpected <- Evaluator.applyFunctionAttempt(UMap(cases), constructor, env)
          inputTExpected <- Evaluator.asType(inputTypeExpected, env)

          result <- typeCheckWithPatternMatching(input, ObjectPattern(UType(inputTExpected)), env, externalFeatureSet, internalFeatureSet)
        } yield {
          TypeCheckWithPatternMatchingResult(
            CasePattern(constructor, result.typeCheckResult),
            expectedType,
            result.newEnvironment
          )
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env, externalFeatureSet)
          tcObj <- Evaluator(tc.nExpression, env)
        } yield {
          TypeCheckWithPatternMatchingResult(ObjectPattern(tcObj), expectedType, env)
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
          nTypeObj <- Evaluator(nTypeExp, env)
          nType <- Evaluator.asType(nTypeObj, env)
          response <- typeCheckWithPatternMatching(expression, ObjectPattern(UType(nType)), env, externalFeatureSet, internalFeatureSet)
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
    nTypeForStructFieldName: NewMapType,
    valueList: Vector[ParseTree],
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[Vector[(NewMapPattern, NewMapExpression)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        for {
          valueId <- typeCheck(valueIdentifier, ObjectPattern(UType(nTypeForStructFieldName)), env, featureSet)
          valueIdObj <- Evaluator(valueId.nExpression, env)

          newParams <- Evaluator.attemptPatternMatch(paramId, valueIdObj, env)

          // Is this substitution neccesary??
          typeOfIdentifierObj <- Evaluator(MakeSubstitution(typeOfIdentifier, newParams, env), env)
          typeOfIdentifierT <- Evaluator.asType(typeOfIdentifierObj, env)

          tc <- typeCheck(valueObject, ObjectPattern(UType(typeOfIdentifierT)), env, featureSet)

          substExp = MakeSubstitution(tc.nExpression, newParams, env)

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
          tc <- typeCheck(valueObject, ObjectPattern(UType(typeOfIdentifierT)), env, featureSet)
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
    typeCheck(expression, WildcardPattern("_"), env, FullFunction)
  }

  case class TypeCheckUnknownFunctionResult(
    functionExpression: NewMapExpression,
    typeOfFunction: NewMapPattern,
    inputExpression: NewMapExpression,
    resultingType: NewMapPattern
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
          inputTypeChecked <- typeCheck(input, ObjectPattern(UType(inputT)), env, FullFunction)
        } yield (inputTypeChecked.nExpression -> inputT)
        case None => {
          for {
            typeCheckUnknownTypeResult <- typeCheckUnknownType(input, env)
            nType <- typeCheckUnknownTypeResult.refinedTypeClass match {
              case ObjectPattern(UType(t)) => Success(t)
              case tc => Failure(s"Couldn't handle type pattern $tc")
            }
          } yield (typeCheckUnknownTypeResult.nExpression -> nType)
        }
      }

      (inputTC, inputT) = inputTagged

      resultingType <- typeOfFunction match {
        case ObjectPattern(UType(MapT(_, outputType, _))) => Success(outputType)
        case ObjectPattern(UType(StructT(params, _, _, _, _))) => outputTypeFromStructParams(params, inputTC, env)
        case ObjectPattern(UType(TypeClassT(typeTransform, typesInTypeClass))) => {
          outputTypeFromTypeClassParams(typeTransform, typesInTypeClass, inputTC, env)
        }
        case ObjectPattern(UType(GenericMapT(typeTransform, config))) => {
          for {
            typeAsObj <- Evaluator.applyFunctionAttempt(UMap(typeTransform), UType(inputT), env)
            nType <- Evaluator.asType(typeAsObj, env)
          } yield {
            nType
          }
        }
        case _ => Failure(s"Cannot get resulting type from function type ${functionTypeChecked.refinedTypeClass}")
      }
    } yield {
      TypeCheckUnknownFunctionResult(functionTypeChecked.nExpression, typeOfFunction, inputTC, ObjectPattern(UType(resultingType)))
    }    
  }

  // Returns a set of patterns representing newmap types
  def inputTypeCheckFromFunctionType(nFunctionTypeClass: NewMapPattern, env: Environment): Option[NewMapType] = {
    stripCustomTag(nFunctionTypeClass) match {
      case ObjectPattern(UType(MapT(inputType, _, _))) => Some(inputType)
      case ObjectPattern(UType(StructT(params, parentFieldType, _, _, _))) => {
        Some(parentFieldType)
      }
      case ObjectPattern(UType(TypeClassT(typeTransform, typesInTypeClass))) => {
        //eventually send typesInTypeClass to the type checker
        Some(TypeT)
        /*SubtypeT(
          UMap(typesInTypeClass.map(x => (x -> ObjectExpression(UIndex(1))))),
          TypeT,
          SimpleFunction
        )*/
      }
      case ObjectPattern(UType(GenericMapT(typeTransform, config))) => {
        // TODO - this is what has to change!!!
        None
        // Really we want a list of the type patterns in typeTransform, but that's going to require a larger change to the type checker
      }
      case other => None
    }
  }

  def outputTypeFromTypeClassParams(
    params: Vector[(NewMapPattern, NewMapExpression)],
    typesInTypeClass: Vector[NewMapPattern],
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
    params: Vector[(NewMapPattern, NewMapExpression)],
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

  def tagAndNormalizeObject(uObject: UntaggedObject, nTypeClass: NewMapPattern, env: Environment): Outcome[NewMapObject, String] = {
    nTypeClass match {
      case ObjectPattern(UType(nType)) => {
        uObject match {
          case UInit => {
            for {
              initValue <- CommandMaps.getDefaultValueOfCommandType(UType(nType), env)
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

  def stripCustomTag(nTypePattern: NewMapPattern): NewMapPattern = {
    nTypePattern match {
      case ObjectPattern(UType(CustomT(_, t))) => ObjectPattern(UType(t))
      case _ => nTypePattern
    }
  }

  def retrieveFeatureSetFromFunctionTypePattern(nTypeClass: NewMapPattern, env: Environment): Outcome[MapFeatureSet, String] = {
    nTypeClass match {
      case ObjectPattern(UType(MapT(_, _, config))) => Success(config.featureSet)
      case ObjectPattern(UType(StructT(_, _, _, featureSet, _))) => Success(featureSet)
      case ObjectPattern(UType(TypeClassT(_, _))) => Success(SimpleFunction)
      case ObjectPattern(UType(CustomT(_, t))) => retrieveFeatureSetFromFunctionTypePattern(ObjectPattern(UType(t)), env)
      case ObjectPattern(UType(GenericMapT(_, config))) => Success(config.featureSet)
      case _ => Failure(s"Cannot retrieve meaningful feature set from object with type $nTypeClass")
    }
  }
}