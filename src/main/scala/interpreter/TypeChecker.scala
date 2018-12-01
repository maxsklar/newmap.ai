package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

case class TypeChecked(
  typeFound: NewMapType,
  objectFound: NewMapObject  // TODO: should we change this to value found??
)

object TypeChecker {
  /*
   * @param expression The literal expression that needs to be type-checked
   * @param expectedType The type that we are expecting the expression to be (defaults to ObjectType, which is equivalent to having no requirement)
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   */
  def typeCheck(
    expression: ParseTree,
    expectedType: NewMapType,
    env: Environment
  ): Outcome[TypeChecked, String] = {
    val result = expression match {
      case NaturalNumberParse(i: Long) => Success(TypeChecked(IndexT(i + 1), Index(i)))
      case IdentifierParse(s: String, true) => Success(TypeChecked(IdentifierT, IdentifierInstance(s)))
      case IdentifierParse(s: String, false) => {
        env.lookup(s) match {
          case Some(objectWithType) => Success(TypeChecked(objectWithType.nType, objectWithType.nObject))
          case None => Success(TypeChecked(IdentifierT, IdentifierInstance(s)))
        }
      }
      case ApplyParse(func: ParseTree, input: ParseTree) => {
        val funcResult = typeCheck(
          func,
          ObjectT, // TODO - can we do better here? gotta call typeCheckFunctionType later
          env
        )

        for {
          functionTypeChecked <- funcResult
          functionType = functionTypeChecked.typeFound
          functionValue = functionTypeChecked.objectFound

          functionTypeCheckerSuccess <- typeCheckFunctionType(functionType)

          inputType = functionTypeCheckerSuccess.inputType

          successfullyTypeChecked <- typeCheck(input, inputType, env)
          inputValue = successfullyTypeChecked.objectFound

          outputType <- functionTypeCheckerSuccess match {
            case StaticTypeFunctionChecked(_, outputType) => Success(outputType)
            case DynamicTypeFunctionChecked(_, params, paramsDefault) => {
              val obj = params.find(x => x._1 == inputValue).map(_._2).getOrElse(paramsDefault)
              Evaluator.convertObjectToType(obj, env)
            }
          }
        } yield {
          TypeChecked(outputType, ApplyFunction(functionValue, inputValue))
        }
      }
      case Enclosure(encloseType, values: Vector[(ParseTree, ParseTree)]) => {
        expectedType match {
          case mapT @ MapT(IdentifierT, TypeT, _) if (encloseType == Paren) => {
            // TODO - do we really want to consider this a parameter list, or just treat it like another map?
            typeCheckParamsStandalone(values, env, expectedType)
          }
          case mapT @ MapT(keyType, valueType, defaultValue) => {
            for {
              mapValues <- typeCheckLiteralMap(values, mapT, env)
            } yield {
              TypeChecked(expectedType, MapInstance(mapValues, defaultValue))
            }
          }
          case StructT(params: Vector[(String, NewMapType)]) => {
            typeCheckStructStandalone(params, values, env)
          }
          case TypeT => {
            // Since this enclose is a type, we're going to assume that it's a struct description
            // TODO: What if it's a case description??
            typeCheckParamsStandalone(values, env, expectedType)
          }
          case _ => Failure("Enclosures must be explicit: " + values + " exp: " + expectedType)
        }
      }
      case LambdaParse(params, expression) => {
        for {
          newParams <- typeCheckParameterList(params, env)
          tc <- typeCheck(expression, ObjectT, env.newParams(newParams))
        } yield {
          TypeChecked(
            LambdaT(newParams, tc.typeFound),
            LambdaInstance(paramsToObjectParams(newParams), tc.objectFound)
          )
        }
      }
    }

    for {
      tc <- result
      typeFound = tc.typeFound
      valueFound = tc.objectFound

      _ <- Outcome.failWhen(
        !isSubtype(
          typeFound,
          expectedType,
          env,
          typeFoundExplicit = false // TODO: set this to true if the type was explicitly set and it's just inferred
        ),
        "In expression " + expression.toString + " expected type " + expectedType.toString + " found type " + typeFound + " and cannot convert\n"
      )
    } yield {
      TypeChecked(typeFound, valueFound)
    }
  }

  def typeCheckLiteralMap(
    values: Vector[(ParseTree, ParseTree)],
    expectedType: MapT,
    env: Environment
  ): Outcome[Vector[(NewMapObject, NewMapObject)], String] = {
    values match {
      case (k, v) +: restOfValues => {
        for {
          tc <- typeCheck(k, expectedType.key, env)
          objectFoundKey = tc.objectFound

          tc2 <- typeCheck(v, expectedType.value, env)
          objectFoundValue = tc2.objectFound

          restOfMap <- typeCheckLiteralMap(restOfValues, expectedType, env)
        } yield {
          (objectFoundKey -> objectFoundValue) +: restOfMap
        }
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
      case ObjectT => true
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
    parameterList: Vector[(ParseTree, ParseTree)],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
    parameterList match {
      case (identifier, typeOfIdentifier) +: otherIdentifiers => {
        checkForIdentifier(identifier, env) match {
          case NotAnIdentifier => Failure("Expected Identifier: " + identifier.toString)
          case FoundIdentifier(name) => {
            // Now we have the variable.. next step we need the type
            // TODO - we need a type check that's specific to a type
            for {
              _ <- typeCheck(typeOfIdentifier, TypeT, env)
              typeOfIdentifierAsType <- typeSpecificTypeChecker(typeOfIdentifier, env)
              expandedEnv = env.newParam(name, typeOfIdentifierAsType)
              restOfParams <- typeCheckParameterList(otherIdentifiers, expandedEnv)
            } yield {
              (name -> typeOfIdentifierAsType) +: restOfParams
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
      case _ => Success(Vector.empty)
    }
  }

  def typeCheckParamsStandalone(
    parameterList: Vector[(ParseTree, ParseTree)],
    env: Environment,
    expectedType: NewMapType
  ): Outcome[TypeChecked, String] = for {
    newParams <- typeCheckParameterList(parameterList, env)
  } yield {
    TypeChecked(expectedType, paramsToObject(newParams))
  }

  /*
   * We want to ensure that the struct was created correctly
   */
  def typeCheckStruct(
    parameterList: Vector[(String, NewMapType)],
    valueList: Vector[(ParseTree, ParseTree)],
    env: Environment
  ): Outcome[Vector[EnvironmentCommand], String] = {
    (parameterList, valueList) match {
      case (((paramId, typeOfIdentifier) +: restOfParamList), ((valueIdentifier, valueObject) +: restOfValueList)) => {
        val valueIdOpt = checkForKnownIdentifier(valueIdentifier, env)

        valueIdOpt match {
          case Some(valueId) if (paramId == valueId) => {
            for {
              tc <- typeCheck(valueObject, typeOfIdentifier, env)

              envCommand = EnvironmentCommand(paramId, typeOfIdentifier, tc.objectFound)
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
    valueList: Vector[(ParseTree, ParseTree)],
    env: Environment
  ): Outcome[TypeChecked, String] = for {
    envCommands <- typeCheckStruct(parameterList, valueList, env)
  } yield {
    val typeFound = StructT(envCommands.map(c => c.id -> c.nType))
    val objectFound = StructInstance(envCommands.map(c => c.id -> c.nObject))
    TypeChecked(typeFound, objectFound)
  }

  // In this case, we want the object to be a type, so we return that type
  // TODO - this repeats a lot of stuff - perhaps combine this with something else?
  def typeSpecificTypeChecker(
    parseTree: ParseTree,
    env: Environment
  ): Outcome[NewMapType, String] = {
    for {
      tc <- typeCheck(parseTree, TypeT, env)
      nmt <- Evaluator.convertObjectToType(tc.objectFound, env)
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

  // The type in question is purported to be a function (or anything applyable)
  // Therefore, it should have an input type, and an output type
  // If not, there's a problem
  def typeCheckFunctionType(
    functionType: NewMapType
  ): Outcome[FunctionTypeChecked, String] = {
    functionType match {
      case IndexT(_) | TypeT | IdentifierT | ObjectT | SubstitutableT(_) | Subtype(_) | SubtypeFromMapType(_) => {
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
      case IdentifierT | ObjectT | StructT(_) => 0
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
      case ObjectT => ObjectType
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

  def apply(
    expression: ParseTree
  ): Outcome[TypeChecked, String] = {
    typeCheck(expression, ObjectT, Environment.Base)
  }
}