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
               // TODO - can we do better here? gotta call typeCheckFunctionType later - maybe we can put a function kind here
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
            typeCheckParamsStandalone(values, env, mapT)
          }
          case ExplicitlyTyped(MapT(keyType, valueType, defaultValue)) => {
            val mapT = MapT(keyType, valueType, defaultValue)
            for {
              mapValues <- typeCheckLiteralMap(values, mapT, env)
            } yield {
              NewMapObjectWithType.withTypeE(MapInstance(mapValues, defaultValue), mapT)
            }
          }
          case ExplicitlyTyped(StructT(params: Vector[(String, NewMapType)])) => {
            typeCheckStructStandalone(params, values, env)
          }
          case ExplicitlyTyped(TypeT) => {
            // Since this commandList is a type, we're going to assume that it's a struct description
            // TODO: What if it's a case description??
            typeCheckParamsStandalone(values, env, TypeT)
          }
          case _ => Failure("CommandLists must be explicit: " + values + " exp: " + expectedType)
        }
      }
      case BindingCommandItem(key, value) => {
        typeCheck(CommandList(Vector(expression)), expectedType, env)
      }
      case LambdaParse(params, expression) => {
        for {
          paramValues <- params match {
            case CommandList(values) => Success(values)
            case _ => Failure("Lambda Values must be variable bindings")
          }

          newParams <- typeCheckParameterList(paramValues, env)
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
          NewMapObjectWithType.withTypeE(
            LambdaInstance(paramsToObjectParams(newParams), tc.nObject),
            LambdaT(newParams, typeFound)
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
    expectedType: MapT,
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    values match {
      case BindingCommandItem(k, v) +: restOfValues => {
        for {
          tc <- typeCheck(k, ExplicitlyTyped(expectedType.key), env)
          objectFoundKey = tc.nObject

          tc2 <- typeCheck(v, ExplicitlyTyped(expectedType.value), env)
          objectFoundValue = tc2.nObject

          restOfMap <- typeCheckLiteralMap(restOfValues, expectedType, env)
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

  // TODO - I expect this function to get more complex
  // TODO - Include Index Type (with open question)
  //  Suppose I expect Index(4). If I have given "2", obviously that's alright
  //  But if I'm given x, which is a param memmber Index(3) - should I automatically convert?
  //  I would say not automatically, because it probably indicates a code error
  //  Solution: use expected value above when calculating the type of a number
  // TODO: might have to remove this!!!
  /**
   * @param typeFoundExplicit was the type explicitly set, or was it found implicitly in the code? If it was found implicitly, the requirements are lax
   */
  def isSubtype(
    typeFound: NewMapType,
    typeExpected: NewMapType,
    env: Environment,
    typeFoundExplicit: Boolean
  ): Boolean = {
    val resolvedType = resolveType(typeFound, env)

    resolveType(typeExpected, env) match {
      case TypeT => refersToAType(resolvedType, env)
      case IndexT(i) => {
        typeFound match {
          case IndexT(j) => if (typeFoundExplicit) (j == i) else (j <= i)
          case _ => false
        }        
      }
      case SubtypeFromMapType(mi) => mi match {
        case MapInstance(values, default) => {
          false // TODO: fix this! Need Literals
        }
      }
      case _ => (typeExpected == resolvedType)
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
          if (isRawObjectConvertibleToType(objectWithType.nObject, typeExpected, env)) {
            Success(NewMapObjectWithType(objectWithType.nObject, ImplicitlyTyped(Vector(typeExpected))))
          } else {
            Failure("Could not implicitly interpret object " + objectWithType.nObject.toString + " as type " + typeExpected.toString)
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
      case _ => (resolvedEndingType == resolvedStartingType)
    }
  }

  // Can this untyped object possibly be interpreted as type nType?
  // TODO: don't just return a boolean, return an outcome with a reason for failure.
  def isRawObjectConvertibleToType(
    nObject: NewMapObject,
    nType: NewMapType,
    env: Environment
  ): Boolean = {
    resolveType(nType, env) match {
      case IndexT(i) => nObject match {
        case Index(j) => j < i
        case _ => false
      }
      case TypeT => Evaluator.convertObjectToType(nObject, env).isSuccess
      case IdentifierT => nObject match {
        case IdentifierInstance(_) => true
        case _ => false
      }
      case MapT(key, value, defaultInType) => nObject match {
        case MapInstance(values, default) => {
          (defaultInType == default) && (
            values.forall(x => {
              isRawObjectConvertibleToType(x._1, key, env) &&
              isRawObjectConvertibleToType(x._2, value, env)
            })
          )
        }
        case _ => false
      }
      case StructT(params) => nObject match {
        case StructInstance(value) => {
          //////////////////// TODO  not always
          true
        }
        case _ => false
      }
      case LambdaT(params, result) => nObject match {
        case LambdaInstance(params, expression) => {
          //////////////////// TODO  not always
          true
        }
        case _ => false
      }
      case SubstitutableT(s) => {
        // TODO: I believe that if we don't actually know the type we can't figure this out
        false
      }
      case Subtype(parent) => nObject match {
        case SubtypeFromMap(values) => {
          //////////////////// TODO  not always
          true
        }
        case _ => false
      }
      case SubtypeFromMapType(mi) => {
        mi.values.exists(_._1 == nObject)
      }
    }
  }

  def resolveType(
    typeFound: NewMapType,
    env: Environment
  ): NewMapType = {
    typeFound match {
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
      case _ => typeFound
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
    expectedType: NewMapType
  ): Outcome[NewMapObjectWithType, String] = for {
    newParams <- typeCheckParameterList(parameterList, env)
  } yield {
    NewMapObjectWithType.withTypeE(paramsToObject(newParams), expectedType)
  }

  /*
   * We want to ensure that the struct was created correctly
   */
  def typeCheckStruct(
    parameterList: Vector[(String, NewMapType)],
    valueList: Vector[ParseTree],
    env: Environment
  ): Outcome[Vector[EnvironmentCommand], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), (BindingCommandItem(valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              tc <- typeCheck(valueObject, ExplicitlyTyped(typeOfIdentifier), env)

              envCommand = EnvironmentCommand(paramId, typeOfIdentifier, tc.nObject)
              newEnv = env.newCommand(envCommand)
              result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
            } yield {
              envCommand +: result
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

          envCommand = EnvironmentCommand(paramId, typeOfIdentifier, tc.nObject)
          newEnv = env.newCommand(envCommand)
          result <- typeCheckStruct(restOfParamList, restOfValueList, newEnv)
        } yield {
          envCommand +: result
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
    val typeFound = StructT(envCommands.map(c => c.id -> c.nType))
    val objectFound = StructInstance(envCommands.map(c => c.id -> c.nObject))
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
      case IndexT(_) | TypeT | IdentifierT | SubstitutableT(_) | Subtype(_) | SubtypeFromMapType(_) => {
        Failure("Type " + functionType.toString + " not generally callable.")
      }
      case MapT(key: NewMapType, value: NewMapType, default: NewMapObject) => {
        Success(StaticTypeFunctionChecked(key, value))
      }
      case StructT(params: Vector[(String, NewMapType)]) => {
        // This will work (uncomment DynamicTypeFunctionChecked) when index type can be quantified
        Success(DynamicTypeFunctionChecked(
          // TODO - this first param should really be SubtypeFromMapType(paramsToObject(params))
          //  However, the identifier currently can't be recognized as that type, even if it's a param.
          //  So once that's fixed, this can be changed over 
          IdentifierT,
          paramsToObject(params).values,
          Index(1)
        ))
      }
      case LambdaT(
        params: Vector[(String, NewMapType)],
        result: NewMapType
      ) => {
        Success(StaticTypeFunctionChecked(StructT(params), result))
      }
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
  ): Long = {
    val result = nType match {
      case IndexT(i) => i
      case TypeT => 1
      case IdentifierT | StructT(_) => 0
      case MapT(key, value, default) => {
        if (isSubtype(key, IdentifierT, env, false) && isSubtype(value, TypeT, env, false)) {
          default match {
            // TODO: Index(0) and Index(1) are both in Index(2)... can we do some kind of check here?
            case Index(0) | Index(1) => 1 // This refers to the case and struct type respectively
            case _ => 0 
          }
        } else {
          0
        }
      }
      case LambdaT(params: Vector[(String, NewMapType)], result: NewMapType) => {
        typeDepth(result, env.newParams(params))
      }
      case SubstitutableT(s: String) => {
        env.typeOf(s).map(t => typeDepth(t, env)) match {
          case None => -2 // TODO: this should be an error
          case Some(0) => -1 // TODO: this should also be an error.. this type shouldn't exist
          case Some(i) => i - 1
        }
      }
      case Subtype(t: NewMapType) => {
        typeDepth(t, env) + 1 // TODO: this is also fishy, make a test
      }
      case SubtypeFromMapType(mapInstance: MapInstance) => {
        1 // TODO: this is fishy, make a test
      }
    }
    result
  }

  def typeToObject(newMapType: NewMapType): NewMapObject = {
    newMapType match {
      case IndexT(i: Long) => Index(i)
      case IdentifierT => IdentifierType
      case MapT(key, value, default) => MapType(typeToObject(key), typeToObject(value), default)
      case StructT(params: Vector[(String, NewMapType)]) => paramsToObject(params)
      case LambdaT(params, result) => {
        LambdaInstance(paramsToObjectParams(params), typeToObject(result))
      }
      case SubstitutableT(s: String) => ParameterObj(s)
      case TypeT => TypeType
      case Subtype(t: NewMapType) => SubtypeType(typeToObject(t))
      case SubtypeFromMapType(m: MapInstance) => SubtypeFromMap(m)
    }
  }

  // TODO(max): These 2 methods should not be needed
  // Vector[(String, NewMapObject)] should not be stored, only type. We'll have to fix this somehow
  def paramsToObjectParams(params: Vector[(String, NewMapType)]): Vector[(String, NewMapObject)] = {
    params.map(param => (param._1 -> typeToObject(param._2)))
  }
  def objectParamsToParams(params: Vector[(String, NewMapObject)], env: Environment): Vector[(String, NewMapType)] = {
    // This one in particular is unsafe, but hopefully will be removed at some point
    params.map(param => (param._1 -> Evaluator.convertObjectToType(param._2, env).toOption.get))
  }


  def paramsToObject(params: Vector[(String, NewMapType)]): MapInstance = {
    val paramsAsObjects: Vector[(NewMapObject, NewMapObject)] = for {
      (name, nmt) <- params
    } yield {
      IdentifierInstance(name) -> typeToObject(nmt)
    }
    MapInstance(paramsAsObjects, Index(1))
  }

  def processMultipleFunctionApplications(
    startingFunction: NewMapObjectWithType,
    applications: Vector[ParseTree],
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = applications match {
    case (firstApplication +: restOfApplications) => {
      for {
        functionType <- startingFunction.nTypeInfo match {
          case ExplicitlyTyped(nType) => Success(nType)
          case ImplicitlyTyped(_) => Failure(
            "Functions must be explicitly typed for now. Implicit function types are not yet supported\n" ++
            startingFunction.nObject.toString
          )
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

  def processFunctionApplication(
    startingFunction: NewMapObjectWithType,
    functionTypeChecked: FunctionTypeChecked,
    input: ParseTree,
    env: Environment
  ): Outcome[NewMapObjectWithType, String] = {
    for {
      // Ensures that the input can be converted to the right type
      successfullyTypeChecked <- typeCheck(
        input,
        ExplicitlyTyped(functionTypeChecked.inputType),
        env
      )

      inputValue = successfullyTypeChecked.nObject

      outputType <- functionTypeChecked match {
        case StaticTypeFunctionChecked(_, outputType) => Success(outputType)
        case DynamicTypeFunctionChecked(_, params, paramsDefault) => {
          val obj = params.find(x => x._1 == inputValue).map(_._2).getOrElse(paramsDefault)
          Evaluator.convertObjectToType(obj, env)
        }
      }
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

  def apply(
    expression: ParseTree
  ): Outcome[NewMapObjectWithType, String] = {
    typeCheck(expression, NewMapTypeInfo.init, Environment.Base)
  }
}