package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

object TypeChecker {
  case class TypeCheckResponse(
    nExpression: UntaggedObject,
    refinedTypeClass: NewMapType,
    tcParameters: Map[String, NewMapType]
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
    featureSet: MapFeatureSet,
    tcParameters: Map[String, NewMapType],
    patternMatchingAllowed: Boolean = false
  ): Outcome[TypeCheckResponse, String] = {
    val expectedTypeOutcome = getFinalUnderlyingType(expectedType, env)

    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    expression match {
      case EmptyParse => {
        responseFromConversion(NewMapObject(UArray(), NewMapO.emptyStruct), expectedType, env, tcParameters)
      }
      case NaturalNumberParse(i: Long) => {
        for {
          t <- expectedTypeOutcome
          refinedType <- TypeClassUtils.typeIsExpectingAnIndex(t, i, env)
        } yield {
          val expectingType = TypeConverter.isTypeConvertible(refinedType, TypeT, env).isSuccess

          if (expectingType) {
            val untaggedValue = IndexT(UIndex(i)).asUntagged
            TypeCheckResponse(untaggedValue, refinedType, tcParameters)
          } else {
            TypeCheckResponse(UIndex(i), refinedType, tcParameters)
          }
        }
      }
      case CharacterParse(s: String) => {
        if (s.length == 1) {
          val tObject = NewMapObject(UCharacter(s(0)), CharacterT)
          responseFromConversion(tObject, expectedType, env, tcParameters)
        } else {
          Failure(s"Character not recognized because it has length > 1: $s")
        }
      }
      case FloatParse(d: Double) => {
        val tObject = NewMapObject(UDouble(d), DoubleT)
        responseFromConversion(tObject, expectedType, env, tcParameters)
      }
      case StringParse(s: String) => {
        val fixedS = s.replace("\\n", "\n").replace("\\t", "\t")

        val arr: Array[UntaggedObject] = fixedS.toCharArray().drop(1).dropRight(1).map(c => UCharacter(c))

        val uObject = UCase(UIndex(fixedS.length - 2), UArray(arr))

        val stringType = CustomT("String", UArray(), env.typeSystem.currentVersion)

        responseFromConversion(NewMapObject(uObject, stringType), expectedType, env, tcParameters)
      }
      case IdentifierParse(s: String, true) => {
        // We need to check that the expectedType allows an identifier!!
        responseFromConversion(NewMapObject(UIdentifier(s), IdentifierT), expectedType, env, tcParameters)
      }
      case IdentifierParse(s: String, false) => {
        val useLiteralIdentifier = for {
          underlyingExpectedT <- expectedTypeOutcome.toOption

          // TODO: This exception is awkward - maybe find some way to account for this in "attemptConvertObjectToType"
          // - we are looking for types that ONLY include identifiers.
          _ <- underlyingExpectedT match {
            case WildcardT(_) => None
            case _ => Some(())
          }

          // Herin Lies the problem!!!
          _ <- TypeConverter.attemptConvertObjectToType(NewMapObject(UIdentifier(s), IdentifierT), underlyingExpectedT, env).toOption
        } yield ()

        if (useLiteralIdentifier.nonEmpty && !patternMatchingAllowed) {
          Success(TypeCheckResponse(UIdentifier(s), expectedType, tcParameters))
        } else if (tcParameters.get(s).nonEmpty) {
          // TODO - not good functional style
          val nType = tcParameters.get(s).get

          for {
            response <- TypeConverter.isTypeConvertible(nType, expectedType, env)
            // TODO - execute convert instructions?
            // response.convertInstructions
          } yield TypeCheckResponse(ParamId(s), nType, tcParameters)
        } else {
          env.lookupValue(s) match {
            case Some(nObject) => {
              responseFromConversion(nObject, expectedType, env, tcParameters)
            }
            case None if (env.typeSystem.typeToParameterType.get(s).nonEmpty) => {
              typeCheckIdentifierFromTypeSystem(s, expectedTypeOutcome, env, featureSet, tcParameters)
            }
            case None if (patternMatchingAllowed) => {
              Success(TypeCheckResponse(UWildcard(s), expectedType, tcParameters + (s -> expectedType)))
            }
            case None => {
              expectedTypeOutcome match {
                case Success(CaseT(cases, IdentifierT, _)) => {
                  for {
                    caseType <- Evaluator.applyFunction(cases, UIdentifier(s), env)

                    caseT <- caseType.asType

                    // TODO - of course, we can formulize this better!
                    caseTypeIsEmptyStruct = caseT match {
                      case StructT(UMap(params), _, _, _) => params.isEmpty
                      case _ => false
                    }

                    isSingularType = (caseTypeIsEmptyStruct || (caseT == IndexT(UIndex(1))))
                    _ <- Outcome.failWhen(!isSingularType, s"Type Value cannot be inferred: $caseType")
                  } yield {
                    if (caseT == IndexT(UIndex(1))) {
                      TypeCheckResponse(UCase(UIdentifier(s), UIndex(0)), expectedType, tcParameters)
                    } else {
                      TypeCheckResponse(UCase(UIdentifier(s), UMap(Vector.empty)), expectedType, tcParameters)
                    }
                  }
                }
                case Success(TypeT) => {
                  for {
                    parameterT <- env.typeSystem.getParameterType(env.typeSystem.currentVersion, s)

                    // TODO - of course, we can formulize this better!
                    parameterTypeIsEmptyStruct = parameterT match {
                      case StructT(UMap(params), _, _, _) => params.isEmpty
                      case _ => false
                    }

                    _ <- Outcome.failWhen(!parameterTypeIsEmptyStruct, s"No parameter specified for type $s")
                  } yield {
                    TypeCheckResponse(UCase(UIdentifier(s), UMap(Vector.empty)), expectedType, tcParameters)
                  }
                }
                case Success(WildcardT(w)) => {
                  Failure(s"Identifier $s is unknown for wildcard type $w")
                }
                case Success(nType) => {
                  val trialObject = NewMapObject(UIdentifier(s), IdentifierT)
                  responseFromConversion(trialObject, nType, env, tcParameters)
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
          result <- typeCheckUnknownFunction(function, input, env, tcParameters)

          // Is member of Subtype check here?
          underlyingTypeOfFunction <- getFinalUnderlyingType(result.typeOfFunction, env)

          functionFeatureSet <- retrieveFeatureSetFromFunctionTypePattern(underlyingTypeOfFunction)

          // If the function is a simplemap, and if it has no internal parameters, it can be executed now, and considered a basic
          functionFeatureSetFixed = if (functionFeatureSet.getLevel <= SimpleFunction.getLevel) {
            Evaluator.applyFunction(result.functionExpression, result.inputExpression, env) match {
              case Success(_) => BasicMap // TODO - save the actual result we got!
              case Failure(_) => functionFeatureSet
            }
          } else functionFeatureSet

          // Validate that this is allowed from the feature set
          _ <- Outcome.failWhen(
            !TypeConverter.isFeatureSetConvertible(functionFeatureSetFixed, featureSet),
            s"Cannot allow function with feature set $functionFeatureSetFixed in expression that should be featureSet $featureSet"
          )

          // If the function is a parameter, then there's no guarantee that it's not self-referential
          // If functionFeatureSet is basicMap, then this can't cause a self-referential issue
          // TODO - flesh out these rules a little bit more
          _ <- Outcome.failWhen(
            !RetrieveType.isTermClosedLiteral(result.functionExpression) && (functionFeatureSet != BasicMap) && (featureSet != FullFunction),
            s"Function ${result.functionExpression} is based on a parameter, which could create a self-referential definition, disallowed in featureSet $featureSet -- $functionFeatureSetFixed"
          )
          response <- TypeConverter.isTypeConvertible(result.resultingType, expectedType, env)
          // TODO - execute response.convertInstructions
        } yield {
          val applyParseResult = ApplyFunction(result.functionExpression, result.inputExpression, StandardMatcher)
          TypeCheckResponse(applyParseResult, result.resultingType, tcParameters)
        }
      }
      case AccessFieldAsMapParse(value, field) => {
        for {
          tcValue <- typeCheckUnknownType(value, env, tcParameters)

          uTypeClass = tcValue.refinedTypeClass.asUntagged

          fieldsToTypeMap <- Evaluator.applyFunction(
            env.typeToFieldMapping,
            uTypeClass,
            env,
            TypeMatcher
          )

          typeOfPotentialFields = SubtypeT(fieldsToTypeMap, IdentifierT)

          theFieldTC <- typeCheck(field, typeOfPotentialFields, env, SimpleFunction, tcParameters)

          // Do not pass tcParameters to the evaluator, because the field must be evaluated
          //  in its entirety in order to be used in the expression
          evaluatedField <- Evaluator(theFieldTC.nExpression, env)

          returnValuePair <- Evaluator.applyFunction(
            fieldsToTypeMap,
            evaluatedField,
            env
          )

          returnValue <- Evaluator.applyFunction(
            returnValuePair,
            UIndex(0),
            env
          )

          returnType <- returnValue match {
            case UCase(t, _) => Success(t)
            case _ => Failure("Unknown return value: " + returnValue)
          }

          returnT <- returnType.asType

          response <- TypeConverter.isTypeConvertible(returnT, expectedType, env)
        } yield {
          val accessFieldResult = AccessField(tcValue.nExpression, uTypeClass, evaluatedField)
          TypeCheckResponse(accessFieldResult, returnT, tcParameters)
        }
      }
      case LiteralListParse(values: Vector[ParseTree], llType: LiteralListType) => {
        expectedTypeOutcome match {
          // TODO: Remove this in favor of other struct T
          case Success(StructT(parameterList, parentFieldType, _, _)) => {
            for {
              parameterBindings <- parameterList.getMapBindings()

              result <- typeCheckStruct(
                parameterBindings,
                parentFieldType, // Is this right - or do we need to pass in the subset??
                values,
                env,
                featureSet,
                tcParameters
              )
            } yield {
              TypeCheckResponse(UMap(result), expectedType, tcParameters)
            }
          }
          case Success(MapT(typeTransform, config)) => {
            val isArrayInput = llType == ArrayType
            val correctedValues = if (isArrayInput) {
              values.zipWithIndex.map(x => KeyValueBinding(NaturalNumberParse(x._2), x._1))
            } else {
              values
            }

            for {
              mapValues <- typeCheckGenericMap(correctedValues, typeTransform, config.featureSet != BasicMap, env, config.featureSet, tcParameters)

              isCovered <- {
                if (config.completeness != RequireCompleteness) Success(true)
                else {
                  val headTypePattern = typeTransform.keyType
                  PatternCoverageCalculator.doPatternsCoverType(mapValues.map(_._1), headTypePattern, env)
                }
              }

              _ <- Outcome.failWhen(
                !isCovered,
                "Incomplete mapping of " + typeTransform.keyType.displayString(env)
              )
            } yield {
              val uObject = if (isArrayInput) UArray(mapValues.map(_._2).toArray) else UMap(mapValues)
              TypeCheckResponse(uObject, expectedType, tcParameters)
            }
          }
          case Success(TypeT) => {
            val typeT = expectedTypeOutcome.toOption.get

            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // OR we are just being given a list of types
            // TODO - can this be simplified by combining with the MapT section above?
            {
              val typeTransform = TypeTransform(IdentifierT, typeT)
              for {
                mapValues <- typeCheckGenericMap(values, typeTransform, false, env, featureSet, tcParameters)
              } yield {
                TypeCheckResponse(
                  StructT(UMap(mapValues), IdentifierT, RequireCompleteness, BasicMap).asUntagged,
                  expectedType,
                  tcParameters
                )
              }
            }.rescue(_ => {
              for {
                expressions <- typeCheckSequence(values, typeT, env, tcParameters)
              } yield {
                val indexType = IndexT(UIndex(expressions.length))
                TypeCheckResponse(
                  StructT(
                    UArray(expressions.toArray),
                    indexType,
                    RequireCompleteness,
                    BasicMap
                  ).asUntagged,
                  expectedType,
                  tcParameters
                )
              }
            })
          }
          case Success(WildcardT(_)) => {
            Failure(s"Lists must be explicitly typed - $values")
          }
          case Success(UndefinedT) => {
            throw new Exception(s"our bug: $values")
          }
          case Success(TypeTransformT(allowGenerics)) => {
            for {
              _ <- Outcome.failWhen(values.length > 1, "Type transform cannot have multiple values")
              value <- Outcome(values.headOption, "Type transform must contain a key and a value")
              mapValue <- typeCheckGenericMap(
                Vector(value),
                TypeTransform(TypeT, TypeT),
                //if (allowGenerics) PatternMap else BasicMap,
                allowGenerics,
                env,
                featureSet,
                tcParameters
              )
            } yield {
              TypeCheckResponse(USingularMap(mapValue.head._1, mapValue.head._2), expectedType, tcParameters)
            }
          }
          case _ => {
            Failure(s"Lists not working yet with this expected type: $values exp: $expectedType -- $expectedTypeOutcome")
          }
        }
      }
      case KeyValueBinding(_, _) => {
        typeCheck(LiteralListParse(Vector(expression), MapType), expectedType, env, featureSet, tcParameters)
      }
      case LambdaParse(input, output) => {
        // Make sure that this fits expectedType!
        // Also - why do these need to be evaluated? seems like they do!
        for {
          inputType <- typeCheck(input, TypeT, env, featureSet, tcParameters)
          outputType <- typeCheck(output, TypeT, env, featureSet, tcParameters)

          evalInputType <- Evaluator(inputType.nExpression, env)

          inputT <- evalInputType.asType
          outputT <- outputType.nExpression.asType
        } yield {
          val typeTransform = TypeTransform(inputT, outputT)

          // TODO - how do we make the mapConfig customizable in the LambdaParse Symbol?
          // - Is there anything we can do with more equals signs, or -> or maybe ~> (I don't know)
          val mapConfig = MapConfig(RequireCompleteness, FullFunction)

          TypeCheckResponse(
            MapT(typeTransform, mapConfig).asUntagged,
            TypeT,
            tcParameters
          )
        }
      }
      case ConstructCaseParse(first, second) => {
        expectedTypeOutcome match {
          case Success(CaseT(simpleMap, parentFieldType, _)) => {
            for {
              firstExp <- typeCheck(first, parentFieldType, env, featureSet, tcParameters)

              // TODO - we must ensure that the evaluator is not evaluating anything too complex here
              // must be a "simple map" type situation
              // can this be built into the evaluator?
              firstObj <- Evaluator(firstExp.nExpression, env)

              secondType <- Evaluator.applyFunction(simpleMap, firstObj, env)

              secondT <- secondType.asType
              secondExp <- typeCheck(second, secondT, env, featureSet, tcParameters)
            } yield {
              TypeCheckResponse(UCase(firstObj, secondExp.nExpression), expectedType, tcParameters)
            }
          }
          case Success(ArrayT(itemT)) => {
            for {
              firstExp <- typeCheck(first, CountT, env, featureSet, tcParameters)

              // TODO - we must ensure that the evaluator is not evaluating anything too complex here
              // must be a "simple map" type situation
              // can this be built into the evaluator?
              firstObj <- Evaluator(firstExp.nExpression, env)

              secondT = MapT(
                TypeTransform(IndexT(firstObj), itemT),
                MapConfig(RequireCompleteness, BasicMap)
              )

              secondExp <- typeCheck(second, secondT, env, featureSet, tcParameters)
            } yield {
              TypeCheckResponse(UCase(firstObj, secondExp.nExpression), expectedType, tcParameters)
            }
          }
          case Success(TypeT) | Success(WildcardT(_)) => {
            for {
              result <- typeCheckCaseAsType(first, second, env.typeSystem.currentVersion, featureSet, env, tcParameters)
            } yield TypeCheckResponse(result, TypeT, tcParameters)
          }
          case nTypeOutcome => {
            Failure(s"Case type must be specified for $expression -- instead got $nTypeOutcome")
          }
        }
      }
      case LiteralCode(statements, expression) => {
        CodeBlockTypeChecker(statements, expression, expectedType, env, featureSet, tcParameters)
      }
    }
  }

  def typeCheckIdentifierFromTypeSystem(
    id: String,
    expectedTypeOutcome: Outcome[NewMapType, String],
    env: Environment,
    featureSet: MapFeatureSet,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeCheckResponse, String] = {
    val expression = IdentifierParse(id, false)
    for {
      tcResult <- typeCheck(
        ConstructCaseParse(expression, LiteralListParse(Vector.empty, MapType)),
        TypeT,
        env,
        featureSet,
        tcParameters
      )
    } yield {
      expectedTypeOutcome match {
        case _ => TypeCheckResponse(tcResult.nExpression, TypeT, tcParameters)
      }
    }
  }

  def responseFromConversion(
    nObject: NewMapObject,
    expectedType: NewMapType,
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeCheckResponse, String] = {
    TypeConverter.attemptConvertObjectToType(nObject, expectedType, env).map(tObject => {
      TypeCheckResponse(tObject.uObject, tObject.nType, tcParameters)
    })
  }

  def typeCheckSequence(
    values: Vector[ParseTree],
    valueType: NewMapType,
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[Vector[UntaggedObject], String] = {
    values match {
      case KeyValueBinding(k, v) +: _ => {
        Failure(s"Sequences don't work with binding command item $k : $v")
      }
      case value +: restOfValues => {
        for {
          objectFoundValue <- typeCheck(value, valueType, env, BasicMap, tcParameters)
          restOfMap <- typeCheckSequence(restOfValues, valueType, env, tcParameters)
        } yield {
          objectFoundValue.nExpression +: restOfMap
        }
      }
      case _ => Success(Vector.empty)
    }
  }

  def typeCheckGenericMap(
    values: Vector[ParseTree],
    typeTransform: TypeTransform,
    patternMatchingAllowed: Boolean,
    env: Environment,
    featureSet: MapFeatureSet, // This is the external feature set, the map feature set can be found in mapT
    tcParameters: Map[String, NewMapType]
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = {
    values match {
      case KeyValueBinding(k, v) +: restOfValues => {
        //println("Checking KEY: " + k + " -- " + typeTransform.keyType)
        for {
          resultKey <- typeCheckWithPatternMatching(k, typeTransform.keyType, env, featureSet, patternMatchingAllowed, tcParameters)

          // Keys must be evaluated on the spot
          foundKeyPattern <- Evaluator(resultKey.nExpression, env)

          //_ = println(s"keyPattern: $foundKeyPattern -- ${resultKey.newParams}")

          untaggedTypeTransformKey = typeTransform.keyType.asUntagged
          untaggedTypeTransformValue = typeTransform.valueType.asUntagged

          paramsToSubsitute <- Evaluator.patternMatch(
            untaggedTypeTransformKey,
            resultKey.refinedTypeClass.asUntagged,
            TypeMatcher,
            env
          )

          valueTypePatternUntagged = MakeSubstitution(untaggedTypeTransformValue, paramsToSubsitute)
          valueTypePattern <- valueTypePatternUntagged.asType

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueTypePattern,
            env,
            featureSet = featureSet,
            resultKey.tcParameters
          )

          restOfMap <- typeCheckGenericMap(restOfValues, typeTransform, patternMatchingAllowed, env, featureSet, tcParameters)
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

  def getUnderlyingType(name: String, params: UntaggedObject, env: Environment, typeSystemId: Long): Outcome[NewMapType, String] = {
    for {
      underlyingTypeInfo <- env.typeSystem.historicalUnderlyingType(name, typeSystemId)

      parameterPattern = underlyingTypeInfo._1
      genericUnderlyingType = underlyingTypeInfo._2

      substitutions <- Evaluator.patternMatch(parameterPattern, params, StandardMatcher, env)

      underlyingType = MakeSubstitution(genericUnderlyingType.asUntagged, substitutions)

      underlyingT <- underlyingType.asType
    } yield underlyingT
  }

  def getCurrentUnderlyingType(name: String, params: UntaggedObject, env: Environment): Outcome[NewMapType, String] = {
    for {
      underlyingTypeInfo <- env.typeSystem.currentUnderlyingType(name)

      parameterPattern = underlyingTypeInfo._1
      genericUnderlyingType = underlyingTypeInfo._2

      substitutions <- Evaluator.patternMatch(parameterPattern, params, StandardMatcher, env)

      underlyingType = MakeSubstitution(genericUnderlyingType.asUntagged, substitutions)

      underlyingT <- underlyingType.asType
    } yield underlyingT
  }

  def getFinalUnderlyingType(nType: NewMapType, env: Environment): Outcome[NewMapType, String] = {
    nType match {
      case CustomT(name, params, typeSystemId) => {
        for {
          partialResultT <- getUnderlyingType(name, params, env, typeSystemId)
          finalizeResult <- getFinalUnderlyingType(partialResultT, env)
        } yield finalizeResult
      }
      case ArrayT(itemT) => {
        getCurrentUnderlyingType("Array", itemT.asUntagged, env)
      }
      case _ => Success(nType)
    }
  }

  def typeCheckCaseAsType(
    first: ParseTree,
    second: ParseTree,
    typeSystemId: Long,
    featureSet: MapFeatureSet,
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[UntaggedObject, String] = {

    for {
      firstAsString <- first match {
        case IdentifierParse(s, false) => Success(s)
        case _ => Failure(s"Constructor for type must be an identifier, it was $first")
      }
    
      typeOfParameter <- env.typeSystem.getParameterType(env.typeSystem.currentVersion, firstAsString)

      paramValue <- typeCheck(second, typeOfParameter, env, featureSet, tcParameters)
    } yield {
      // TODO - this is an awkward hack given how we slip in the historical UUID.
      // - Let's figure out if there's a better way to organize this.
      val includeUuid = NewMapTypeSystem.isCustomType(firstAsString)
      val uParams = if (includeUuid) {
        UArray(paramValue.nExpression, UIndex(env.typeSystem.currentVersion))
      } else paramValue.nExpression
      
      UCase(UIdentifier(firstAsString), uParams)
    }
  }

  // TODO - I suspect that this can be folded into the main typeCheck function
  //  where the input specifies whether we are looking for patterns or not
  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapType,
    env: Environment,
    featureSet: MapFeatureSet, // Am I inside a map with restricted features already?
    patternMatchingAllowed: Boolean,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeCheckResponse, String] = {
    if (!patternMatchingAllowed) {
      val retVal = for {
        result <- typeCheck(expression, expectedType, env, featureSet, tcParameters)
      } yield {
        TypeCheckResponse(result.nExpression, result.refinedTypeClass, tcParameters)
      }

      return retVal
    }

    //val patternMatchingAllowed = internalFeatureSet.getLevel >= PatternMap.getLevel
    
    // Use this??
    // val parentTypeIsIdentifier = TypeClassUtils.typeIsExpectingAnIdentifier(expectedType, s, env)

    val expectedTypeOutcome = getFinalUnderlyingType(expectedType, env)

    (expression, expectedTypeOutcome) match {
      /*case (IdentifierParse(s, false), _) if (patternMatchingAllowed) => {
        env.lookupValue(s) match {
          // It's unclear if this is the right way to go!
          case Some(value) => Success(TypeCheckResponse(value.uObject, value.nType, tcParameters))
          case None if (env.typeSystem.typeToParameterType.get(s).nonEmpty) => {
            for {
              tcResponse <- typeCheckIdentifierFromTypeSystem(s, expectedTypeOutcome, env, featureSet, tcParameters)
            } yield {
              TypeCheckResponse(tcResponse.nExpression, tcResponse.refinedTypeClass, tcParameters)
            }
          }
          case None => Success(TypeCheckResponse(UWildcard(s), expectedType, tcParameters + (s -> expectedType)))
        }
      }*/
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (LiteralListParse(values, _), Success(StructT(UMap(structValues), _, _, _))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, featureSet, patternMatchingAllowed, env, tcParameters)
        } yield {
          TypeCheckResponse(UArray(tcmp.patterns.toArray), expectedType, tcmp.newParams)
        }
      }
      case (LiteralListParse(values, _), Success(StructT(UArray(structValues), _, _, _))) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues).zipped.toVector, featureSet, patternMatchingAllowed, env, tcParameters)
        } yield {
          TypeCheckResponse(UArray(tcmp.patterns.toArray), expectedType, tcmp.newParams)
        }
      }
      case (ConstructCaseParse(constructorP, input), Success(CaseT(cases, parentFieldType, _))) if (patternMatchingAllowed) => {
        for {
          // TODO - update environment here
          constructorTC <- typeCheckWithPatternMatching(constructorP, parentFieldType, env, featureSet, (parentFieldType != IdentifierT) && patternMatchingAllowed, tcParameters)
          constructor <- Evaluator(constructorTC.nExpression, env)

          inputTypeExpected <- Evaluator.applyFunction(cases, constructor, env)
          inputTExpected <- inputTypeExpected.asType

          result <- typeCheckWithPatternMatching(input, inputTExpected, env, featureSet, patternMatchingAllowed, constructorTC.tcParameters)
        } yield {
          TypeCheckResponse(
            UCase(constructor, result.nExpression),
            expectedType,
            result.tcParameters
          )
        }
      }
      case (ConstructCaseParse(constructorP, input), Success(ArrayT(itemT))) if (patternMatchingAllowed) => {
        for {
          // TODO - update environment here
          constructorTC <- typeCheckWithPatternMatching(constructorP, CountT, env, featureSet, patternMatchingAllowed, tcParameters)
          constructor <- Evaluator(constructorTC.nExpression, env)

          inputTExpected = MapT(
            TypeTransform(IndexT(constructor), itemT),
            MapConfig(RequireCompleteness, BasicMap)
          )

          result <- typeCheckWithPatternMatching(input, inputTExpected, env, featureSet, patternMatchingAllowed, constructorTC.tcParameters)
        } yield {
          TypeCheckResponse(
            UCase(constructor, result.nExpression),
            expectedType,
            result.tcParameters
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

          parameterT <- env.typeSystem.getParameterType(env.typeSystem.currentVersion, name)

          result <- typeCheckWithPatternMatching(
            input,
            parameterT,
            env,
            featureSet,
            patternMatchingAllowed,
            tcParameters
          )
        } yield {
          // TODO - this is an awkward hack given how we slip in the historical UUID.
          // - Let's figure out if there's a better way to organize this.
          val includeUuid = NewMapTypeSystem.isCustomType(name)

          val uParams = if (includeUuid) {
            UArray(result.nExpression, UIndex(env.typeSystem.currentVersion))
          } else result.nExpression
          
          TypeCheckResponse(
            UCase(UIdentifier(name), uParams),
            expectedType,
            result.tcParameters
          )
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env, featureSet, tcParameters, patternMatchingAllowed)
        } yield {
          TypeCheckResponse(tc.nExpression, tc.refinedTypeClass, tc.tcParameters)
        }
      }
    }
  }

  case class TypeCheckWithMultiplePatternMatchingResult(
    patterns: Vector[UntaggedObject],
    newParams: Map[String, NewMapType]
  )

  def typeCheckWithMultiplePatterns(
    expressions: Vector[(ParseTree, UntaggedObject)],
    featureSet: MapFeatureSet,
    patternMatchingAllowed: Boolean,
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeCheckWithMultiplePatternMatchingResult, String] = {
    expressions match {
      case (expression, nTypeExp) +: restOfExpressions => {
        for {
          nTypeObj <- Evaluator(nTypeExp, env)
          nType <- nTypeObj.asType
          response <- typeCheckWithPatternMatching(expression, nType, env, featureSet, patternMatchingAllowed, tcParameters)
          pattern = response.nExpression
          finalResponse <- typeCheckWithMultiplePatterns(restOfExpressions, featureSet, patternMatchingAllowed, env, response.tcParameters)
        } yield {
          TypeCheckWithMultiplePatternMatchingResult(pattern +: finalResponse.patterns, finalResponse.newParams)
        }
      }
      case _ => Success(TypeCheckWithMultiplePatternMatchingResult(Vector.empty, tcParameters))
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
    featureSet: MapFeatureSet,
    tcParameters: Map[String, NewMapType]
  ): Outcome[Vector[(UntaggedObject, UntaggedObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (KeyValueBinding(valueIdentifier, valueObject) +: restOfValueList)) => {
        for {
          valueId <- typeCheck(valueIdentifier, nTypeForStructFieldName, env, featureSet, tcParameters)
          valueIdObj <- Evaluator(valueId.nExpression, env)

          newParams <- Evaluator.patternMatch(paramId, valueIdObj, StandardMatcher, env)

          // Is this substitution neccesary??
          typeOfIdentifierObj <- Evaluator(MakeSubstitution(typeOfIdentifier, newParams), env)
          typeOfIdentifierT <- typeOfIdentifierObj.asType

          tc <- typeCheck(valueObject, typeOfIdentifierT, env, featureSet, tcParameters)

          substExp = MakeSubstitution(tc.nExpression, newParams)

          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, env, featureSet, tcParameters)
        } yield {
          (valueIdObj, substExp) +: result
        }
      }
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          typeOfIdentifierObj <- Evaluator(typeOfIdentifier, env)
          typeOfIdentifierT <- typeOfIdentifierObj.asType
          tc <- typeCheck(valueObject, typeOfIdentifierT, env, featureSet, tcParameters)
          result <- typeCheckStruct(restOfParamList, nTypeForStructFieldName, restOfValueList, env, featureSet, tcParameters)
        } yield {
          (paramId, tc.nExpression) +: result
        }
      }
      case _ => {
        if (parameterList.isEmpty && valueList.isEmpty) {
          Success(Vector.empty)
        } else if (valueList.isEmpty && (parameterList.length == 1)) {
          parameterList.head._1 match {
            case UWildcard(_) => Success(Vector.empty)
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
    typeCheckUnknownType(expression, env, Map.empty)
  }

  def typeCheckUnknownType(
    expression: ParseTree,
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeCheckResponse, String] = {
    typeCheck(expression, WildcardT("_"), env, FullFunction, tcParameters)
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
    env: Environment,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeCheckUnknownFunctionResult, String] = {
    for {
      functionTypeChecked <- typeCheckUnknownType(function, env, tcParameters)

      typeOfFunction = functionTypeChecked.refinedTypeClass
      underlyingTypeOfFunction <- getFinalUnderlyingType(typeOfFunction, env)

      functionUntaggedObject = Evaluator.stripVersioningU(functionTypeChecked.nExpression, env)

      inputTagged <- underlyingTypeOfFunction.inputTypeOpt(Some(functionUntaggedObject)) match {
        case Some(inputT) => {
          for {
            inputTypeChecked <- typeCheck(input, inputT, env, FullFunction, tcParameters)
          } yield (inputTypeChecked.nExpression -> inputTypeChecked.refinedTypeClass)
        }
        case None => {
          for {
            typeCheckUnknownTypeResult <- typeCheckUnknownType(input, env, tcParameters)
          } yield (typeCheckUnknownTypeResult.nExpression -> typeCheckUnknownTypeResult.refinedTypeClass)
        }
      }

      (inputTC, inputType) = inputTagged

      untaggedInputType = inputType.asUntagged

      resultingType <- underlyingTypeOfFunction match {
        case StructT(UMap(params), _, _, _) => outputTypeFromStructParams(params, inputTC, env)
        case TypeClassT(typeTransform, implementation) => {
          outputTypeFromTypeClassParams(typeTransform, implementation, inputTC, inputType, env)
        }
        case SequenceT(parent, _) => {
          val strippedExpression = Evaluator.stripVersioningU(functionTypeChecked.nExpression, env)
          
          strippedExpression match {
            case UCase(i, _) => Success(IndexT(i))
            case _ => Failure("Incorrect sequence: " + functionTypeChecked.nExpression)
          }
        }
        case MapT(typeTransform, _) => {
          for {
            newParams <- Evaluator.patternMatch(
              typeTransform.keyType.asUntagged,
              untaggedInputType,
              TypeMatcher,
              env
            )

            typeAsObj = MakeSubstitution(
              typeTransform.valueType.asUntagged,
              newParams
            )

            nType <- typeAsObj.asType
            _ <- Outcome.failWhen(nType == UndefinedT, s"Couldn't apply type $inputType to type transform $typeTransform")
          } yield nType
        }
        case TypeT => {
          for {
            typeCheckedT <- functionTypeChecked.nExpression.asType

            underlingT <- getFinalUnderlyingType(typeCheckedT, env)

            result <- underlingT match {
              case TypeClassT(typeTransform, implementation) => {
                outputTypeFromTypeClassParams(typeTransform, implementation, inputTC, inputType, env)
              }
              case _ => Failure(s"Cannot get resulting type from function of Type ${functionTypeChecked.nExpression} -- $function -- $input")
            }
          } yield result
        }
        case _ => Failure(s"Cannot get resulting type from function type ${typeOfFunction.displayString(env)} -- $function -- $input")
      }

      resultingFunctionExpression <- typeOfFunction match {
        case TypeT => {
          for {
            typeCheckedT <- functionTypeChecked.nExpression.asType
            underlingT <- getFinalUnderlyingType(typeCheckedT, env)

            impl <- underlingT match {
              case TypeClassT(_, implementation) => {
                Success(implementation)
              }
              case _ => Failure(s"Cannot get resulting type from function of Type ${underlingT}")
            }

            strippedExpression = Evaluator.stripVersioningU(functionTypeChecked.nExpression, env)

            patternMatchAttempted <- Evaluator.applyFunction(impl, untaggedInputType, env, TypeMatcher)
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

  def outputTypeFromTypeClassParams(
    params: UntaggedObject,
    implementation: UntaggedObject,
    input: UntaggedObject,
    inputType: NewMapType,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      resultingFunctionType <- Evaluator.applyFunction(params, inputType.asUntagged, env)
      resultingFunctionT <- resultingFunctionType.asType
    } yield {
      resultingFunctionT
    }
  }

  def outputTypeFromStructParams(
    params: Vector[(UntaggedObject, UntaggedObject)],
    input: UntaggedObject,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      inputObj <- Evaluator(input, env)
      resultingType <- Evaluator.applyFunction(UMap(params), inputObj, env)

      // This will (correctly) fail when resultingType == UInit (ie, it's not in params)
      resultingT <- resultingType.asType
    } yield resultingT
  }

  def tagAndNormalizeObject(uObject: UntaggedObject, nTypeClass: NewMapType, env: Environment): Outcome[NewMapObject, String] = {
    uObject match {
      case UInit => {
        for {
          initValue <- UpdateCommandCalculator.getDefaultValueOfCommandType(nTypeClass, env)
        } yield NewMapObject(initValue, nTypeClass)
      }
      case _ => {
        if (nTypeClass == CountT) {
          normalizeCount(uObject) match {
            case Success(i) => Success(NewMapObject(UIndex(i), nTypeClass))
            case _ => Success(NewMapObject(uObject, nTypeClass))
          }
        } else {
          Success(NewMapObject(uObject, nTypeClass))
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

  def retrieveFeatureSetFromFunctionTypePattern(nTypeClass: NewMapType): Outcome[MapFeatureSet, String] = {
    nTypeClass match {
      case StructT(_, _, _, featureSet) => Success(featureSet)
      case TypeClassT(_, _) => Success(PatternMap)
      case MapT(_, config) => Success(config.featureSet)
      case TypeT => Success(PatternMap) // This is for type classes
      case SequenceT(_, featureSet) => Success(featureSet)
      case _ => Failure(s"Cannot retrieve meaningful feature set from object with type $nTypeClass")
    }
  }
}