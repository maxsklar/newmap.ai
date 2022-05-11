package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param typeClass This represents the typeClass that we expect the object to be
   *  If it's a single ObjectPattern - then that represents the common case where we are expecting a specific type
   *  The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is the environment of values upon which we are working
   */
  def typeCheck(
    expression: ParseTree,
    //typeClass: Vector[NewMapPattern],
    expectedType: NewMapType,
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[NewMapExpression, String] = {
    // Be sure to return something whose type is convertible to expectedType
    // OR if expectedType is a Subset, it's a member of the superset and also matches the subset condition
    // TODO - write a bunch of tests for that!
    expression match {
      case NaturalNumberParse(i: Long) => {
        expectedType/*Evaluator.stripCustomTag(expectedType)*/ match {
          case CountT => Success(ObjectExpression(UIndex(i)))
            case OrBooleanT => Success(ObjectExpression(UIndex(i)))
          case IndexT(j) => {
            if (i < j) Success(ObjectExpression(UIndex(i)))
            else {
              //throw new Exception(s"Proposed index $i is too large for type $j")
              Failure(s"Proposed index $i is too large for type $j")
            }
          }
          case SubtypeT(isMember, parentType, featureSet) => {
            for {
              inputTC <- typeCheck(expression, parentType, env, featureSet)

              // We should be allowed to evaluate this because it's just a number
              // BUT - this is an ugly call - it'd be nice if we didn't have to do it
              inputE <- Evaluator(inputTC, env)

              // TODO - this should be unneccesary for when we change isMemberOfSubtype
              isMemberTagged = TaggedObject(isMember, MapT(parentType, OrBooleanT, MapConfig(CommandOutput, SimpleFunction)))

              membershipCheck <- SubtypeUtils.isMemberOfSubtype(inputE, isMemberTagged, env)
              _ <- Outcome.failWhen(!membershipCheck, s"Value $inputE not a member of subtype $expectedType")
            } yield inputTC
          }
          case TypeT => Success(ObjectExpression(UType(IndexT(i))))
          case _ => {
            // This is required if we recieve an "AnyT" type - which in the future won't happen
            // because this is going to be a type class and not an actual type

            //Failure(s"Unexpected type with number: $expectedType")
            Success(ObjectExpression(UIndex(i)))
          }
        }
      }
      case IdentifierParse(s: String, true) => {
        // We need to check that the expectedType allows an identifier!!
        val expectingAnIdentifier = typeIsExpectingAnIdentifier(expectedType, env)
        if (expectingAnIdentifier) {
          // TODO: What if the type is a subtype - then this might not be allowed!!
          Success(ObjectExpression(UIdentifier(s)))
        } else {
          for {
            convertInstructions <- SubtypeUtils.isTypeConvertible(IdentifierT, expectedType, env)

            // TODO: execute convertInstructions
            convertedObject = UIdentifier(s)
          } yield ObjectExpression(convertedObject)
        }
      }
      case IdentifierParse(s: String, false) => {
        val expectingAnIdentifier = typeIsExpectingAnIdentifier(expectedType, env)

        if (expectingAnIdentifier) {
          Evaluator.stripCustomTag(expectedType) match {
            case IdentifierT => Success(ObjectExpression(UIdentifier(s)))
            case SubtypeT(isMember, parentType, featureSet) => {
              val isMemberTagged = TaggedObject(isMember, MapT(parentType, OrBooleanT, MapConfig(CommandOutput, SimpleFunction)))
              for {
                isAllowed <- SubtypeUtils.isMemberOfSubtype(UIdentifier(s), isMemberTagged, env)
                _ <- Outcome.failWhen(!isAllowed, s"identifier $s not in subtype $expectedType")
              } yield ObjectExpression(UIdentifier(s))
            }
            case _ => Failure(s"Can't convert identifier $s to type $expectedType")
          }
        } else env.lookup(s) match {
          case Some(EnvironmentParameter(nType)) => {
            for {
              convertInstructions <- SubtypeUtils.isTypeConvertible(nType, expectedType, env)
              // TODO - execute convert instructions?
            } yield ParamId(s)
          }
          case Some(EnvironmentBinding(nObject)) => {
            val nType = RetrieveType.fromNewMapObject(nObject, env)
            for { 
              convertInstructions <- SubtypeUtils.isObjectConvertibleToType(nObject, expectedType, env)
              
              // TODO - execute convert instructions on nObject
              uObject <- Evaluator.removeTypeTag(nObject)
            } yield {
              ObjectExpression(uObject)
            }
          }
          case None => {
            Failure(s"Identifier $s is unknown, expecting type $expectedType --- ${typeIsExpectingAnIdentifier(expectedType, env)}")
          }
        }
      }
      case ApplyParse(function, input) => {
        for {
          result <- typeCheckUnknownFunction(function, input, env)

          // Is member of Subtype check here?
          functionFeatureSet <- RetrieveType.retrieveFeatureSetFromFunctionType(result.typeOfFunction, env)

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
          convertInstructions <- SubtypeUtils.isTypeConvertible(result.resultingType, expectedType, env)
        } yield {
          ApplyFunction(result.functionExpression, result.inputExpression)
        }
      }
      case CommandList(values: Vector[ParseTree]) => {
        Evaluator.stripCustomTag(expectedType) match {
          case mapT@MapT(keyTypeT, valueT, config) => {
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
              BuildMapInstance(mapValues)
            }
          }
          case StructT(parameterList, parentFieldType, _, _, _) => {
            for {
              result <- typeCheckStruct(
                parameterList,
                parentFieldType, // Is this right - or do we need to pass in the subset??
                values,
                env,
                featureSet
              )
            } yield {
              BuildMapInstance(result)
            }
          }
          case GenericMapT(typeTransform, config) => {
            for {
              mapValues <- typeCheckGenericMap(values, typeTransform, config.featureSet, env, featureSet)
            } yield {
              BuildMapInstance(mapValues)
            }
          }
          case TypeT => {
            // Here we assume that we are looking at a struct type, and that we are being given a Map from an identifier to a Type
            // OR we are just being given a list of types
            // TODO - can this be simplified by combining with the MapT section above?
            {
              for {
                mapValues <- typeCheckMap(values, IdentifierT, TypeT, BasicMap, env, featureSet)
              } yield {
                BuildStructT(BuildMapInstance(mapValues), IdentifierT, RequireCompleteness, BasicMap)
              }
            }.rescue(f => {
              for {
                expressions <- typeCheckSequence(values, TypeT, env)
              } yield {
                val indexType = IndexT(expressions.length)
                BuildStructT(
                  BuildMapInstance(
                    expressions.zipWithIndex.map(x => ObjectPattern(UIndex(x._2)) -> x._1),
                  ),
                  indexType,
                  RequireCompleteness,
                  BasicMap
                )
              }
            })
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
      case ConstructCaseParse(first, second) => {
        Evaluator.stripCustomTag(expectedType) match {
          case CaseT(simpleMap, parentFieldType, _, _) => {
            for {
              firstExp <- typeCheck(first, parentFieldType, env, featureSet)

              // TODO - we must ensure that the evaluator is not evaluating anything too complex here
              // must be a "simple map" type situation
              // can this be built into the evaluator?
              firstObj <- Evaluator(firstExp, env)

              secondType <- Evaluator.applyFunctionAttempt(UMap(simpleMap), firstObj, env)
              secondT <- Evaluator.asType(secondType, env)
              secondExp <- typeCheck(second, secondT, env, featureSet)
            } yield {
              BuildCase(firstObj, secondExp)
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
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, externalFeatureSet, internalFeatureSet)
          
          foundKeyPattern = resultKey.typeCheckResult

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueType,
            resultKey.newEnvironment,
            featureSet = internalFeatureSet
          )

          restOfMap <- typeCheckMap(restOfValues, keyType, valueType, internalFeatureSet, env, externalFeatureSet)
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
          objectFoundValue <- typeCheck(value, valueType, env, BasicMap)
          restOfMap <- typeCheckSequence(restOfValues, valueType, env)
        } yield {
          objectFoundValue +: restOfMap
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
    throw new Exception("Unimplemented: typeCheckGenericMap")
    /*values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          resultKey <- typeCheckWithPatternMatching(k, keyType, env, externalFeatureSet, internalFeatureSet)
          
          foundKeyPattern = resultKey.typeCheckResult

          // Now we want to type check the object, but we have to tell it what kind of map we're in
          //  in order to ensure that the right features are being used
          objectFoundValue <- typeCheck(
            v,
            valueType,
            resultKey.newEnvironment,
            featureSet = internalFeatureSet
          )

          restOfMap <- typeCheckGenericMap(restOfValues, typeTransform, internalFeatureSet, env, externalFeatureSet)
        } yield {
          (foundKeyPattern -> objectFoundValue) +: restOfMap
        }
      }
      case s +: _ => {
        Failure(s"No binding found in map for item $s in $values")
      }
      case _ => Success(Vector.empty)
    }*/
  }

  case class TypeCheckWithPatternMatchingResult(
    typeCheckResult: NewMapPattern,
    newEnvironment: Environment // TODO: can this be grabbed from the pattern above??
  )

  def typeCheckWithPatternMatching(
    expression: ParseTree,
    expectedType: NewMapType,
    env: Environment,
    externalFeatureSet: MapFeatureSet, // Am I inside a map with restricted features already?
    internalFeatureSet: MapFeatureSet // Which feature set is this map allowed to use
  ): Outcome[TypeCheckWithPatternMatchingResult, String] = {
    val patternMatchingAllowed = internalFeatureSet != BasicMap
    val parentTypeIsIdentifier = typeIsExpectingAnIdentifier(expectedType, env)

    (expression, Evaluator.stripCustomTag(expectedType)) match {
      case (IdentifierParse(s, false), _) if (patternMatchingAllowed /*&& !parentTypeIsIdentifier*/) => {
        Success(
          TypeCheckWithPatternMatchingResult(WildcardPattern(s), env.newParam(s, expectedType))
        )
      }
      // TODO: what if instead of BasicMap we have SimpleMap on the struct? It gets a little more complex
      case (CommandList(values), StructT(structValues, parentFieldType, _, _, _)) if (patternMatchingAllowed && (values.length == structValues.length)) => {
        for {
          tcmp <- typeCheckWithMultiplePatterns((values,structValues.map(_._2)).zipped.toVector, externalFeatureSet, internalFeatureSet, env)
        } yield {
          TypeCheckWithPatternMatchingResult(StructPattern(tcmp.patterns), tcmp.newEnvironment)
        }
      }
      case (ConstructCaseParse(constructorP, input), CaseT(cases, parentFieldType, _, _)) if (patternMatchingAllowed) => {
        // TODO - shouldn't this be a subtype??
        val caseConstructorType = parentFieldType

        for {
          constructorTC <- typeCheck(constructorP, parentFieldType, env, BasicMap)
          constructor <- Evaluator(constructorTC, env)
          inputTypeExpected <- Evaluator.applyFunctionAttempt(UMap(cases), constructor, env)
          inputTExpected <- Evaluator.asType(inputTypeExpected, env)

          result <- typeCheckWithPatternMatching(input, inputTExpected, env, externalFeatureSet, internalFeatureSet)
        } yield {
          TypeCheckWithPatternMatchingResult(
            CasePattern(constructor, result.typeCheckResult),
            result.newEnvironment
          )
        }
      }
      case _ => {
        for {
          tc <- typeCheck(expression, expectedType, env, externalFeatureSet)
          tcObj <- Evaluator(tc, env)
        } yield {
          TypeCheckWithPatternMatchingResult(ObjectPattern(tcObj), env)
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
    nTypeForStructFieldName: NewMapType,
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
          typeOfIdentifierT <- Evaluator.asType(typeOfIdentifierObj, env)

          tc <- typeCheck(valueObject, typeOfIdentifierT, env, featureSet)

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
          typeOfIdentifierT <- Evaluator.asType(typeOfIdentifierObj, env)
          tc <- typeCheck(valueObject, typeOfIdentifierT, env, featureSet)
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
    nType: NewMapType,
    env: Environment
  ): Boolean = nType match {
    case IdentifierT => true
    case CustomT(_, t) => typeIsExpectingAnIdentifier(t, env)
    case SubtypeT(_, parentType, _) => typeIsExpectingAnIdentifier(parentType, env)
    case _ => false
  }

  def apply(
    expression: ParseTree
  ): Outcome[(NewMapExpression, NewMapType), String] = {
    val env = (new EnvironmentInterpreter()).env

    typeCheckUnknownType(expression, env)
  }

  def typeCheckUnknownType(
    expression: ParseTree,
    env: Environment
  ): Outcome[(NewMapExpression, NewMapType), String] = {
    expression match {
      case NaturalNumberParse(i: Long) => Success(ObjectExpression(UIndex(i)) -> CountT)
      case IdentifierParse(s: String, true) => Success(ObjectExpression(UIdentifier(s)) -> IdentifierT)
      case IdentifierParse(s: String, false) => {
        env.lookup(s) match {
          case Some(EnvironmentBinding(nObject)) => {
            val nType = RetrieveType.fromNewMapObject(nObject, env)
            for {
              uObject <- Evaluator.removeTypeTag(nObject)
            } yield {
              ObjectExpression(uObject) -> nType
            }
          }
          case Some(EnvironmentParameter(nType)) => {
            Success(ParamId(s) -> nType)
          }
          case None => Failure(s"Unknown Identifier $s")
        }
      }
      case ApplyParse(function, input) => {

        for {
          result <- typeCheckUnknownFunction(function, input, env)
        } yield {
          ApplyFunction(result.functionExpression, result.inputExpression) -> result.resultingType
        }        
      }
      case LambdaParse(input, output) => {
        for {
          exp <- typeCheck(expression, TypeT, env, FullFunction)
        } yield (exp -> TypeT)
      }
      case _ => Failure(s"Couldn't do an unknown type check: $expression")
      /*case CommandList(_) => {}
      case BindingCommandItem(key, value) => {}
      case ConstructCaseParse(first, second) = {}*/
    }
  }

  case class TypeCheckUnknownFunctionResult(
    functionExpression: NewMapExpression,
    typeOfFunction: NewMapType,
    inputExpression: NewMapExpression,
    resultingType: NewMapType
  )

  def typeCheckUnknownFunction(
    function: ParseTree,
    input: ParseTree,
    env: Environment
  ): Outcome[TypeCheckUnknownFunctionResult, String] = {
    for {
      functionTypeChecked <- typeCheckUnknownType(function, env)

      typeOfFunction = functionTypeChecked._2

      inputTypeOption = inputTypeCheckFromFunctionType(typeOfFunction, env)

      inputTagged <- inputTypeOption match {
        case Some(inputT) => for {
          inputTypeChecked <- typeCheck(input, inputT, env, FullFunction)
        } yield {
          (inputTypeChecked -> inputT)
        }
        case None => typeCheckUnknownType(input, env)
      }

      (inputTC, inputT) = inputTagged

      resultingType <- typeOfFunction match {
        case MapT(_, outputType, _) => Success(outputType)
        case StructT(params, _, _, _, _) => outputTypeFromStructParams(params, inputTC, env)
        case TypeClassT(typeTransform, typesInTypeClass) => {
          outputTypeFromTypeClassParams(typeTransform, typesInTypeClass, inputTC, env)
        }
        case GenericMapT(typeTransform, config) => {
          for {
            typeAsObj <- Evaluator.applyFunctionAttempt(UMap(typeTransform), UType(inputT), env)
            nType <- Evaluator.asType(typeAsObj, env)
          } yield {
            nType
          }
        }
        case _ => Failure(s"Cannot get resulting type from function type ${functionTypeChecked._2}")
      }
    } yield {
      TypeCheckUnknownFunctionResult(functionTypeChecked._1, typeOfFunction, inputTC, resultingType)
    }    
  }

  // Returns a set of patterns representing newmap types
  def inputTypeCheckFromFunctionType(nFunctionType: NewMapType, env: Environment): Option[NewMapType] = {
    Evaluator.stripCustomTag(nFunctionType) match {
      case MapT(inputType, _, _) => Some(inputType)
      case StructT(params, parentFieldType, _, _, _) => {
        Some(parentFieldType)
      }
      case TypeClassT(typeTransform, typesInTypeClass) => {
        //eventually send typesInTypeClass to the type checker
        Some(TypeT)
        /*SubtypeT(
          UMap(typesInTypeClass.map(x => (x -> ObjectExpression(UIndex(1))))),
          TypeT,
          SimpleFunction
        )*/
      }
      case GenericMapT(typeTransform, config) => {
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

  def tagAndNormalizeObject(uObject: UntaggedObject, nType: NewMapType, env: Environment): Outcome[NewMapObject, String] = {
    uObject match {
      case UInit => {
        for {
          initValue <- CommandMaps.getDefaultValueOfCommandType(UType(nType), env)
        } yield TaggedObject(initValue, nType)
      }
      case _ => Success(TaggedObject(uObject, nType))
    }
  }
}