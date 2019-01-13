package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be . The object does not have to have this type exactly, but the type of the object must have an automatic conversion to this type.
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: NewMapTypeInfo,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    val result = expression match {
      case NaturalNumberParse(i: Long) => Success(NewMapObjectWithType.untyped(Index(i)))
      case IdentifierParse(s: String, true) => Success(NewMapObjectWithType.untyped(IdentifierInstance(s)))
      case IdentifierParse(s: String, false) => {
        env.lookup(s) match {
          case Some(objectWithType) => Success(objectWithType)
          case None => Success(NewMapObjectWithType.untyped(IdentifierInstance(s)))
        }
      }
      case ApplyParse(startingFunction: ParseTree, applications: Vector[ParseTree]) => {
        for {
          functionTypeChecked <- {
            typeCheck(
              startingFunction,
              NewMapTypeInfo.init,
              env
            )
          }

          result <- processMultipleFunctionApplications(functionTypeChecked, applications, env)
        } yield result
      }
      case CommandList(values: Vector[ParseTree]) => {
        expectedType match {
          case ExplicitlyTyped(MapT(IdentifierT, TypeT, default)) => {
            val mapT = MapT(IdentifierT, TypeT, default)
            // TODO - do we really want to consider this a parameter list, or just treat it like another map?
            typeCheckParamsStandalone(values, env, mapT, default)
          }
          case ExplicitlyTyped(MapT(keyType, valueType, defaultValue)) => {
            val mapT = MapT(keyType, valueType, defaultValue)
            for {
              mapValues <- typeCheckLiteralMap(values, Some(mapT), env)
            } yield {
              NewMapObjectWithType.withTypeE(MapInstance(mapValues, defaultValue), mapT)
            }
          }
          case ExplicitlyTyped(StructT(params: Vector[(String, NewMapType)])) => {
            typeCheckStructStandalone(params, values, env)
          }
          case ExplicitlyTyped(CaseT(params: Vector[(String, NewMapType)])) => {
            if (values.length != 1) {
              Failure("A case may only select 1 value")
            } else {
              val firstValue = values(0)
              firstValue match {
                case BindingCommandItem(valueIdentifier, valueObject) => {
                  val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

                  valueIdOpt match {
                    case Some(valueId) => {
                      params.toMap.get(valueId) match {
                        case Some(nType) => {
                          for {
                            nObjectWithType <- typeCheck(valueObject, ExplicitlyTyped(nType), env)
                          } yield {
                            val valueObject = nObjectWithType.nObject
                            NewMapObjectWithType(CaseInstance(valueId, valueObject), expectedType)
                          }
                        }
                        case _ => {
                          Failure("Identifier " + valueId + " not a member of case class: " + params)
                        }
                      }
                    }
                    case None => {
                      Failure("Expected idenfier: " + valueIdentifier)
                    }
                  }
                }
                case _ => {
                  Failure("Value " + firstValue + " not a member of case class: " + params)
                }
              }
            }
          }
          case ExplicitlyTyped(TypeT) => {
            // Since this commandList is a type, we're going to assume that it's a struct or case description
            // TODO: This should be Index(0) if this is a "Case" - need to figure out that possibility
            val default = Index(1) 
            typeCheckParamsStandalone(values, env, TypeT, default)
          }
          case ImplicitlyTyped(Vector()) => {
            // Assume that it's a map?
            // TODO - maybe there needs to be more logic here
            for {
              mapValues <- typeCheckLiteralMap(values, None, env)
            } yield {
              // TODO: default value is given by index(0) - but in reality this is a ReqMap over its inputs
              // TODO: it's untyped, but should accumulate implicit types though typeCheckLiteralMap
              NewMapObjectWithType.untyped(MapInstance(mapValues, Index(0)))
            }
          }
          case _ => Failure("CommandLists not working yet with this expected type: " + values + " exp: " + expectedType)
        }
      }
      case BindingCommandItem(key, value) => {
        typeCheck(CommandList(Vector(expression)), expectedType, env)
      }
      case LambdaParse(params, expression) if (expectedType == ExplicitlyTyped(TypeT)) => {
        for {
          inputType <- typeSpecificTypeChecker(params, env)
          newEnv = inputType match {
            case StructT(inputParams) => env.newParams(inputParams)
            case _ => env
          }

          outputType <- typeSpecificTypeChecker(expression, newEnv)
        } yield {
          NewMapObjectWithType.withTypeE(
            LambdaType(ConvertNewMapTypeToObject(inputType), ConvertNewMapTypeToObject(outputType)),
            TypeT
          )
        }
      }
      case LambdaParse(params, expression) => {
        for {
          newParams <- params match {
            case CommandList(values) => typeCheckParameterList(values, env)
            case BindingCommandItem(key, value) => typeCheckParameterList(Vector(params), env)
            case IdentifierParse(id, _) => {
              expectedType match {
                case ExplicitlyTyped(LambdaT(input, output)) => {
                  Success(Vector(id -> input))
                }
                case _ => {
                  Failure("Lambda Expression is untypes, and this is not implemented yet.")
                }
              }

            }
            case _ => Failure("Lambda Values must be variable bindings " + params + " -- " + expression)
          }

          tc <- typeCheck(expression, NewMapTypeInfo.init, env.newParams(newParams))

          typeFound <- tc.nTypeInfo match {
            case ExplicitlyTyped(nType) => Success(nType)
            case ImplicitlyTyped(convs) => {
              expectedType match {
                case ExplicitlyTyped(TypeT) => {
                  // TODO: more cases
                  Success(TypeT)
                }
                // TODO: this case must be further filled out
                case _ => Failure("Cannot do implicitly typed lambda statements for expected " + expectedType)
              }
            }
          }
        } yield {
          val lambdaParams: Vector[(String, NewMapObject)] = {
            ConvertNewMapTypeToObject.paramsToObjectParams(newParams)
          }
          NewMapObjectWithType.withTypeE(
            LambdaInstance(StructParams(lambdaParams), tc.nObject),
            LambdaT(StructT(newParams), typeFound)
          )
        }
      }
    }

    for {
      objectWithType <- result

      // TODO: figure out what to really do here
      withAdditionalType <- expectedType match {
        case ExplicitlyTyped(nType) => additionalExpectedType(objectWithType, nType, env)
        case ImplicitlyTyped(convs) => additionalExpectedTypes(objectWithType, convs, env)
      }
    } yield withAdditionalType
  }

  def typeCheckLiteralMap(
    values: Vector[ParseTree],
    expectedTypeOpt: Option[MapT],
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        val kType = expectedTypeOpt.map(e => ExplicitlyTyped(e.key)).getOrElse(NewMapTypeInfo.init)
        val vType = expectedTypeOpt.map(e => ExplicitlyTyped(e.value)).getOrElse(NewMapTypeInfo.init)

        for {
          tc <- typeCheck(k, kType, env)
          objectFoundKey = tc.nObject

          tc2 <- typeCheck(v, vType, env)
          objectFoundValue = tc2.nObject

          restOfMap <- typeCheckLiteralMap(restOfValues, expectedTypeOpt, env)
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

  /** 
   * There is a requirement that the objectWithType is a member of typeExpected, or at least convertible.
   * If this is already the case, then return the original objectWithType
   * If objectWithType cannot be converted, then fail
   * If objectWithType is implicitly typed and can be of typeExpected, but it requires appending more type information to it 
   * @param objectWithType the object under consideration
   * @param typeExpected the type that the object must fit into
   * @param env the environment
   */
  def additionalExpectedType(
    objectWithType: NewMapObjectWithType,
    typeExpected: NewMapType,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    objectWithType.nTypeInfo match {
      case ExplicitlyTyped(nType) => {
        if (isTypeConvertible(nType, typeExpected, env)) {
          Success(objectWithType) 
        } else {
          Failure("In expression " + objectWithType.nObject.toString + " expected type " + typeExpected.toString + " found type " + nType.toString + " and cannot convert\n")
        }
      }
      case ImplicitlyTyped(typeConversions) => {
        if (typeConversions.isEmpty) {
          for {
            unit <- isRawObjectConvertibleToType(objectWithType, typeExpected, env)
          } yield {
            NewMapObjectWithType(objectWithType.nObject, ImplicitlyTyped(Vector(typeExpected)))
          }
        } else if (typeConversions.length == 1) {
          // TODO, this head is fine, but clean up your scala MAX!
          val singleType = typeConversions.head
          if (isTypeConvertible(typeExpected, singleType, env)) {
            // We already know we can convert to singleType, so no change neccesary
            Success(objectWithType)
          } else if (isTypeConvertible(singleType, typeExpected, env)) {
            // In this case, go with single type because it is more specific
            Success(NewMapObjectWithType(objectWithType.nObject, ImplicitlyTyped(Vector(singleType))))
          } else {
            // TODO: there are cases where this should be possible, so add those here.
            Failure("Object "+ objectWithType.nObject.toString + " cannot be a part of both type " + singleType.toString + " and type " + typeExpected.toString)
          }
        } else {
          Failure("Cannot do ImplicitlyTyped stuff yet for multiple implicit types")
        }
      }
    }
  }

  def additionalExpectedTypes(
    objectWithType: NewMapObjectWithType,
    typeConvs: Vector[NewMapType],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    typeConvs match {
      case nType +: restOfTypes => {
        for {
          withFirstType <- additionalExpectedType(objectWithType, nType, env)
          withOtherTypes <- additionalExpectedTypes(objectWithType, restOfTypes, env)
        } yield withOtherTypes
      }
      case _ => Success(objectWithType)
    }
  }

  // TODO: ultimately, more potential conversions will be added to the environment, making this function more interesting
  def isTypeConvertible(
    startingType: NewMapType,
    endingType: NewMapType,
    env: Environment
  ): Boolean = {
    val resolvedStartingType = resolveType(startingType, env)
    val resolvedEndingType = resolveType(endingType, env)

    resolvedEndingType match {
      case TypeT => refersToAType(resolvedStartingType, env)
      case IndexT(i) => {
        // The indecies must match, even though you can theoretically convert a smaller index to a bigger index.
        // This conversion is not made explicit to prevent people from accessing an array or map with the wrong index value
        resolvedStartingType match {
          case IndexT(j) => (j == i)
          case _ => false
        }        
      }
      case SubtypeFromMapType(endingMi) => {
        resolvedStartingType match {
          case SubtypeFromMapType(startingMi) => {
            (endingMi.default == startingMi.default) && (
              startingMi.values.forall(v => endingMi.values.exists(_._1 == v._1))
            )
          }
          case _ => false
        }
      }
      case LambdaT(inputType, outputType) => {
        resolvedEndingType match {
          case LambdaT(endingInputType, endingOutputType) => {
            isTypeConvertible(outputType, endingOutputType, env) && isTypeConvertible(inputType, endingInputType, env)
          }
          case _ => false
        }
      }
      case _ => (resolvedEndingType == resolvedStartingType)
    }
  }

  // Can this untyped object be interpreted as type nType?
  def isRawObjectConvertibleToType(
    nObjectWithType: NewMapObjectWithType,
    nType: NewMapType,
    env: Environment
  ): Outcome[Unit, String] = {
    def failMsg(extra: String) = {
      Failure(extra + " Could not implicitly interpret object " + nObjectWithType + " as type " + nType)
    }

    val success = Success(())

    val nObject = nObjectWithType.nObject

    resolveType(nType, env) match {
      case IndexT(i) => nObject match {
        case Index(j) => if (j < i) success else failMsg("A")
        case _ => failMsg("B")
      }
      case TypeT => {
        for {
          _ <- Evaluator.convertObjectToType(nObject, env)
        } yield ()
      }
      case CountT => nObject match {
        case Index(_) => success
        case _ => failMsg("Only counting numbers can convert to the count type.\n")
      } 
      case IdentifierT => nObject match {
        case IdentifierInstance(_) => success
        case _ => failMsg("D")
      }
      case MapT(key, value, defaultInType) => nObject match {
        case MapInstance(values, default) => {
          // TODO: we need a helper function here to get the right message
          if (
            (defaultInType == default) && (
              values.forall(x => {
                isRawObjectConvertibleToType(NewMapObjectWithType.untyped(x._1), key, env).isSuccess &&
                isRawObjectConvertibleToType(NewMapObjectWithType.untyped(x._2), value, env).isSuccess
              })
            )
          ) success else failMsg("E")
        }
        case _ => failMsg("F")
      }
      case StructT(params) => nObject match {
        case StructInstance(value) => {
          //////////////////// TODO  not always
          success
        }
        case otherObject => {
          // This could also be the first input to the struct

          params match {
            case firstParam +: otherParams => {
              if (otherParams.length == 0) {
                val firstParamType: NewMapType = firstParam._2
                isRawObjectConvertibleToType(nObjectWithType, firstParamType, env)
              } else {
                failMsg("Can't convert non-struct to multiple-valued struct")
              }
            }
            case _ => failMsg("G")
          }
        }
      }
      case CaseT(params) => nObject match {
        case CaseInstance(constructor, input) => {
          params.toMap.get(constructor) match {
            case None => failMsg("No constructor for type: " + constructor)
            case Some(t) => {
              isRawObjectConvertibleToType(
                NewMapObjectWithType.untyped(input),
                t,
                env
              )
            }
          }
        }
        // TODO: there can be some conversions here; we don't neccesarily need to go to the fail case
        case _ => failMsg("G Case")
      }
      case LambdaT(params, result) => nObject match {
        case LambdaInstance(params, expression) => {
          //////////////////// TODO  not always
          success
        }
        case _ => failMsg("H")
      }
      case SubstitutableT(s) => {
        // TODO: I believe that if we don't actually know the type we can't figure this out
        failMsg("I")
      }
      case Subtype(parent) => nObject match {
        case SubtypeFromMap(values) => {
          //////////////////// TODO  not always
          success
        }
        case _ => failMsg("J")
      }
      case SubtypeFromMapType(mi) => {
        if (mi.values.exists(_._1 == nObject)) success else failMsg("K")
      }
      case IncrementT(_) => failMsg("L")
      //case MutableT(_, _, _, _) => failMsg("Not implemented: MutableT\n")
    }
  }

  def resolveType(
    typeFound: NewMapType,
    env: Environment
  ): NewMapType = {
    typeFound match {
      case IndexT(_) | TypeT | CountT | IdentifierT | SubtypeFromMapType(_) => typeFound
      case SubstitutableT(name) => {
        env.objectOf(name) match {
          case Some(resObj) => {
            Evaluator.convertObjectToType(resObj, env) match {
              case Success(t) => t
              case Failure(_) => {
                typeFound // TODO: this failure should propogate?
              }
            }
          }
          case None => typeFound
        }
      }
      case IncrementT(t) => {
        resolveType(t, env) match {
          case IndexT(i) => IndexT(i + 1)
          case other => IncrementT(other)
        }
      }
      case MapT(key, value, default) => MapT(resolveType(key, env), resolveType(value, env), default)
      case StructT(params) => StructT(params.map(p => (p._1 -> resolveType(p._2, env))))
      case CaseT(params) => StructT(params.map(p => (p._1 -> resolveType(p._2, env))))
      case LambdaT(input, result) => LambdaT(resolveType(input, env), resolveType(result, env))
      case Subtype(parent) => Subtype(resolveType(parent, env))
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
        env.objectOf(name) match {
          case Some(ParameterObj(name)) => FoundIdentifier(name)
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

  // TODO: This fails on a struct!!!
  def typeCheckParameterList(
    parameterList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
    parameterList match {
      case BindingCommandItem(identifier, typeOfIdentifier) +: otherIdentifiers => {
        checkForIdentifier(identifier, env) match {
          case NotAnIdentifier => Failure("Expected Identifier: " + identifier.toString)
          case FoundIdentifier(name) => {
            // Now we have the variable.. next step we need the type
            // TODO - we need a type check that's specific to a type
            for {
              _ <- typeCheck(typeOfIdentifier, ExplicitlyTyped(TypeT), env)
              typeOfIdentifierAsType <- typeSpecificTypeChecker(typeOfIdentifier, env)
              expandedEnv = env.newParam(name, typeOfIdentifierAsType)
              restOfParams <- typeCheckParameterList(otherIdentifiers, expandedEnv)
            } yield {
              (name -> typeOfIdentifierAsType) +: restOfParams
            }
          }
          case FoundIdentifierUnknownValue => {
            for {
              _ <- typeCheck(typeOfIdentifier, ExplicitlyTyped(TypeT), env)
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
    expectedType: NewMapType,
    default: NewMapObject
  ): Outcome[NewMapObjectWithType, String] = for {
    newParams <- typeCheckParameterList(parameterList, env)
  } yield {
    NewMapObjectWithType.withTypeE(ConvertNewMapTypeToObject.paramsToObject(newParams, default), expectedType)
  }

  /*
   * We want to ensure that the struct was created correctly
   */
  def typeCheckStruct(
    parameterList: Vector[(String, NewMapType)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[(String, NewMapType, NewMapObject)], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifier), env)

              envCommand = Environment.eCommand(paramId, typeOfIdentifier, tc.nObject)
              newEnv = env.newCommand(envCommand)
              result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
            } yield {
              (paramId, typeOfIdentifier, tc.nObject) +: result
            }
          }
          case Some(valueId) => {
            Failure("Ids don't match: " + paramId + " --- " + valueId)
          }
          case None => {
            Failure("Id note found for " + paramId)
          }
        }
      }      
      case (((paramId, typeOfIdentifier) +: restOfParamList), (valueObject +: restOfValueList)) => {
        // TODO: this is pasted code from inside the case above.
        for {
          tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifier), env)

          envCommand = Environment.eCommand(paramId, typeOfIdentifier, tc.nObject)
          newEnv = env.newCommand(envCommand)
          result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
        } yield {
          (paramId, typeOfIdentifier, tc.nObject) +: result
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

  def typeCheckStructStandalone(
    parameterList: Vector[(String, NewMapType)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = for {
    envCommands <- typeCheckStruct(parameterList, valueList, env)
  } yield {
    val typeFound = StructT(envCommands.map(c => c._1 -> c._2))
    val objectFound = StructInstance(envCommands.map(c => c._1 -> c._3))
    NewMapObjectWithType.withTypeE(objectFound, typeFound)
  }

  // In this case, we want the object to be a type, so we return that type
  // TODO - this repeats a lot of stuff - perhaps combine this with something else?
  def typeSpecificTypeChecker(
    parseTree: ParseTree,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      tc <- typeCheck(parseTree, ExplicitlyTyped(TypeT), env)
      nmt <- Evaluator.convertObjectToType(tc.nObject, env)
    } yield nmt
  }

  sealed abstract class FunctionTypeChecked(val inputType: NewMapType)
  case class StaticTypeFunctionChecked(
    override val inputType: NewMapType,
    outputType: NewMapType
  ) extends FunctionTypeChecked(inputType)

  // This is what happens in a struct, where different inputs produce different outputs
  case class DynamicTypeFunctionChecked(
    override val inputType: NewMapType,
    params: Vector[(NewMapObject, NewMapObject)],
    paramsDefault: NewMapObject
  ) extends FunctionTypeChecked(inputType)

  // TODO: there should be a case here for overloaded functions (accepts several different types and returns a type based on the input type)

  // The type in question is purported to be a function (or anything applyable)
  // Therefore, it should have an input type, and an output type
  // If not, there's a problem
  def typeCheckFunctionType(
    functionType: NewMapType
  ): Outcome[FunctionTypeChecked, String] = {
    functionType match {
      case IndexT(_) | TypeT | IdentifierT | SubstitutableT(_) | Subtype(_) | SubtypeFromMapType(_) | CountT | IncrementT(_) => {
        Failure("Type " + functionType + " not generally callable.")
      }
      case MapT(key: NewMapType, value: NewMapType, default: NewMapObject) => {
        Success(StaticTypeFunctionChecked(key, value))
      }
      case StructT(params: Vector[(String, NewMapType)]) => {
        // This will work (uncomment DynamicTypeFunctionChecked) when index type can be quantified
        Success(DynamicTypeFunctionChecked(
          // TODO - this first param should really be SubtypeFromMapType(ConvertNewMapTypeToObject.paramsToObject(params))
          //  However, the identifier currently can't be recognized as that type, even if it's a param.
          //  So once that's fixed, this can be changed over 
          IdentifierT,
          ConvertNewMapTypeToObject.paramsToObject(params, Index(1)).values,
          Index(1)
        ))
      }
      case CaseT(params: Vector[(String, NewMapType)]) => {
        Failure("Case types are not functions: " + functionType)      
      }
      case LambdaT(inputType: NewMapType, outputType: NewMapType) => {
        Success(StaticTypeFunctionChecked(inputType, outputType))
      }
      //case MutableT(staticType, _, _, _) => typeCheckFunctionType(staticType)
    }
  }

  /*
   * Returns true if the values of type nType are themselves types!
   */
  def refersToAType(
    nType: NewMapType,
    env: Environment
  ): Boolean = {
    typeDepth(nType, env) > 0
  }

  /*
   * Returns 0 if the objects in nType are NOT types
   * Returns 1 if they are types
   * Returns 2 if their members are also types
   * Etc...
   *
   * TODO: maybe this should be done in the type object (ie not in the type checker itself)
   */
  def typeDepth(
    nType: NewMapType,
    env: Environment
  ): Long = nType match {
    case IndexT(i) => Long.MaxValue
    case TypeT => 1
    case IdentifierT => 0
    case StructT(params) => {
      // TODO - define a maxByOption
      if (params.isEmpty) 0L else params.map(_._2).map(t => typeDepth(t, env)).max
    }
    case CaseT(params) => {
      // TODO - define a minByOption
      // TODO - default should be infinite, not long value
      if (params.isEmpty) Long.MaxValue else params.map(_._2).map(t => typeDepth(t, env)).min
    }
    case MapT(key, value, default) => {
      if (isTypeConvertible(key, IdentifierT, env) && isTypeConvertible(value, TypeT, env)) {
        default match {
          // TODO: Index(0) and Index(1) are both in Index(2)... can we do some kind of check here?
          case Index(0) | Index(1) => 1 // This refers to the case and struct type respectively
          case _ => 0 
        }
      } else {
        0
      }
    }
    case LambdaT(_, _) => 0
    case SubstitutableT(s: String) => {
      env.typeOf(s) match {
        case Failure(_) => -2 // TODO: this should be an error
        case Success(ExplicitlyTyped(nType)) => {
          val depth = typeDepth(nType, env)
          if (depth == 0) -1 // TODO: this should also be an error.. this type shouldn't exist
          else depth - 1
          depth - 1
        }
        case Success(ImplicitlyTyped(types)) => {
          -3 // TODO: Figure out what to do here 
        }
      }
    }
    case Subtype(t: NewMapType) => {
      typeDepth(t, env) + 1 // TODO: this is also fishy, make a test
    }
    case SubtypeFromMapType(mapInstance: MapInstance) => {
      1 // TODO: this is fishy, make a test
    }
    case CountT => Long.MaxValue // TODO: should be infinite
    case IncrementT(base) => Long.MaxValue
    //case MutableT(staticType, _, _, _) => typeDepth(staticType, env)
  }

  def processMultipleFunctionApplications(
    startingFunction: NewMapObjectWithType,
    applications: Vector[ParseTree],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    applications match {
      case (firstApplication +: restOfApplications) => {
        for {
          functionType <- startingFunction.nTypeInfo match {
            case ExplicitlyTyped(nType) => Success(nType)
            case ImplicitlyTyped(convertibles) => {
              if (convertibles.length > 1) {
                Failure(
                  "Implicit function types are not yet supported for multiple convertibles\n" ++
                  startingFunction.nObject.toString
                )
              }

              convertibles.headOption match {
                case None => Failure("Function not assigned any type")
                case Some(nType) => Success(nType)
              }
            }
          }

          functionTypeChecked <- typeCheckFunctionType(functionType)
          resultOfFirstApplication <- processFunctionApplication(startingFunction, functionTypeChecked, firstApplication, env)
          result <- processMultipleFunctionApplications(resultOfFirstApplication, restOfApplications, env)
        } yield result
      }
      case _ => {
        Success(startingFunction)
      }
    }
  }

  def processFunctionApplication(
    startingFunction: NewMapObjectWithType,
    functionTypeChecked: FunctionTypeChecked,
    input: ParseTree,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    for {
      // Ensures that the input can be converted to the right type
      typeCheckFunctionInputResult <- typeCheckFunctionInput(
        input,
        functionTypeChecked,
        env
      )

      successfullyTypeChecked = typeCheckFunctionInputResult.newMapObjectWithType

      inputValue = successfullyTypeChecked.nObject

      fullOutputType <- functionTypeChecked match {
        case StaticTypeFunctionChecked(_, outputType) => Success(outputType)
        case DynamicTypeFunctionChecked(_, params, paramsDefault) => {
          dynamicTyping(inputValue, params, paramsDefault, env)
        }
      }

      // We're either peeling away one parameter, or we're going to the full output
      outputType <- if (typeCheckFunctionInputResult.firstParamOnly) {
        functionTypeChecked.inputType match {
          case StructT(params) => {
            if (params.length == 1) Success(fullOutputType) else {
              val newEnv = env.newCommand(FullEnvironmentCommand(params(0)._1, successfullyTypeChecked))

              val newParams = params.drop(1).map(param => {
                param._1 -> resolveType(param._2, newEnv)
              })

              for {
                newStruct <- subsituteType(StructT(newParams), newEnv)
                newOutputType <- subsituteType(fullOutputType, newEnv.newParams(newParams))
              } yield LambdaT(newStruct, newOutputType)
            }
          }
          // TODO: refactor to avoid this case?
          case _ => Failure("This shouldn't happen")
        }
      } else Success(fullOutputType)
    } yield {
      NewMapObjectWithType.withTypeE(
        ApplyFunction(
          startingFunction.nObject,
          inputValue
        ),
        outputType
      )
    }
  }

  def subsituteType(
    newMapType: NewMapType,
    env: Environment
  ): Outcome[NewMapType, String] = {
    Evaluator.convertObjectToType(
      Evaluator.makeRelevantSubsitutions(ConvertNewMapTypeToObject(newMapType), env),
      env
    )
  }

  def dynamicTyping(
    inputValue: NewMapObject,
    params: Vector[(NewMapObject, NewMapObject)],
    paramsDefault: NewMapObject,
    env: Environment
  ): Outcome[NewMapType, String] = params match {
    case firstParam +: restOfParams => {
      if (firstParam._1 == inputValue) {
        Evaluator.convertObjectToType(firstParam._2, env)
      } else {
        for {
          firstParamType <- Evaluator.convertObjectToType(firstParam._2, env)

          newEnv = firstParam._1 match {
            case IdentifierInstance(id) => env.newParam(id, firstParamType)
            case _ => {
              // TODO: I think this is unusable right now, check if we can get rid of this.
              env
            }
          }

          result <- dynamicTyping(inputValue, restOfParams, paramsDefault, newEnv)
        } yield result
        
      }
    }
    case _ => {
      Evaluator.convertObjectToType(paramsDefault, env)
    }
  }

  case class TypeCheckFunctionInputResult(
    newMapObjectWithType: NewMapObjectWithType,
    firstParamOnly: Boolean
  )

  def typeCheckFunctionInput(
    input: ParseTree,
    functionTypeChecked: FunctionTypeChecked,
    env: Environment
  ): Outcome[TypeCheckFunctionInputResult, String] = {
    typeCheck(
      input,
      ExplicitlyTyped(functionTypeChecked.inputType),
      env
    ) match {
      case Success(nmo) => Success(TypeCheckFunctionInputResult(nmo, false))
      case Failure(msg) => {
        // Now we have a second try, because the input might be the first value of an input struct
        functionTypeChecked.inputType match {
          case StructT(params) => {
            // TODO: type unsafe until we enforce at least one param in scala
            val firstParam = params.head

            typeCheck(input, ExplicitlyTyped(firstParam._2), env) match {
              case Success(nmo) => Success(TypeCheckFunctionInputResult(nmo, true))
              case Failure(msg2) => {
                val introStr = "Could not interpret input " + input + " as valid for function explicitly typed as " + firstParam._2 + "."
                val err1 = "Error for direct approach: " + msg
                val err2 = "Error for trying to match it to the first parameter: " + msg2
                Failure(introStr + "\n" + err1 + "\n" + err2)
              }
            }
          }
          case _ => Failure(msg)
        }
      }
    }
  }

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObjectWithType, String] = {
    typeCheck(expression, NewMapTypeInfo.init, Environment.Base)
  }
}