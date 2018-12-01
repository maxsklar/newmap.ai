package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Evaluates an expression that's already been type checked
object Evaluator {
  def apply(
    nObject: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    nObject match {
      case ApplyFunction(func, input) => applyFunctionAttempt(func, input, env)
      case _ => {
        // TODO - this is unimplemented. In reality you need to dive into the
        // nObject structure and figure out how to apply
        // Also - return something else if can't be evaluated fully
        Success(nObject)
      }
    }
  }

  def applyFunctionAttempt(
    func: NewMapObject,
    input: NewMapObject,
    env: Environment
  ): Outcome[NewMapObject, String] = {
    (func, input) match {
      case (LambdaInstance(params, expression), StructInstance(paramValues)) => {
        for {
          newEnv <- updateEnvironmentWithParamValues(params, paramValues, env)
          substitutedExpression = makeRelevantSubsitutions(expression, newEnv)
        } yield {
          substitutedExpression
        }
      }
      case (MapInstance(values, default), key) => {
        for {
          evaluatedKey <- this(key, env)
        } yield {
          values.find(_._1 == evaluatedKey).map(_._2) match {
            case Some(result) => result
            case None => default
          }
        }
      }
      case (StructInstance(value: Vector[(String, NewMapObject)]), identifier) => {
        val id = makeRelevantSubsitutions(identifier, env)
        Success(value.find(x => IdentifierInstance(x._1) == id).map(_._2).getOrElse(Index(0)))
      }
      case _ => {
        Failure("Not implemented: apply function when not lambdainstance and structinstance")
      }
    }
  }

  def updateEnvironmentWithParamValues(
    paramTypes: Vector[(String, NewMapObject)],
    paramValues: Vector[(String, NewMapObject)],
    env: Environment
  ): Outcome[Environment, String] = {
    (paramTypes, paramValues) match {
      case ((firstParamType +: addlParamTypes), (firstParamValue +: addlParamValues)) => {
        for {
          _ <- Outcome.failWhen(
            firstParamType._1 != firstParamValue._1,
            "Params don't agree: " + firstParamType._1 + " vs " + firstParamValue._1
          )
          typeInformation <- convertObjectToType(firstParamType._2, env)

          envCommand = EnvironmentCommand(
            firstParamType._1,
            typeInformation,
            firstParamValue._2
          )
          newEnv = env.newCommand(envCommand)

          result <- updateEnvironmentWithParamValues(addlParamTypes, addlParamValues, newEnv)
        } yield result
      }
      // TODO - what if one is longer than the other
      case _ => Success(env)
    }
  }

  // TODO: this shouldn't be part of the type checker
  def makeRelevantSubsitutions(
    expression: NewMapObject,
    env: Environment
  ): NewMapObject = {
    expression match {
      case Index(_) | ObjectType | TypeType | IdentifierType | IdentifierInstance(_) => expression
      case MapType(key, value, default) => {
        MapType(makeRelevantSubsitutions(key, env), makeRelevantSubsitutions(value, env), makeRelevantSubsitutions(default, env))
      }
      case MapInstance(values, default) => {
        val newValues = for {
          (k, v) <- values
        } yield (makeRelevantSubsitutions(k, env) -> makeRelevantSubsitutions(v, env))

        MapInstance(newValues, default)
      }
      case LambdaInstance(params, expression) => {
        val newEnv = includeParams(params, env)
        val newExpression = makeRelevantSubsitutions(expression, newEnv)
        LambdaInstance(params, newExpression)
      }
      case ApplyFunction(func, input) => {
        ApplyFunction(
          makeRelevantSubsitutions(func, env),
          makeRelevantSubsitutions(input, env)
        )
      }
      case ParameterObj(name) => {
        env.objectOf(name) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case StructType(values) => {
        StructType(makeRelevantSubsitutions(values, env))
      }
      case StructInstance(value) => {
        StructInstance(value.map(x => (x._1 -> makeRelevantSubsitutions(x._2, env))))
      }
      case SubtypeType(parentType) => {
        SubtypeType(makeRelevantSubsitutions(parentType, env))
      }
      case SubtypeFromMap(MapInstance(values, default)) => {
        val newValues = for {
          (k, v) <- values
        } yield (makeRelevantSubsitutions(k, env) -> makeRelevantSubsitutions(v, env))

        val newMapInstance = MapInstance(newValues, default)

        SubtypeFromMap(newMapInstance)
      }
    }
  }

  // TODO: this should also not be a part of the type checker
  def includeParams(
    params: Vector[(String, NewMapObject)],
    env: Environment
  ): Environment = {
    params match {
      case (paramName, paramObj) +: addlParams => {
        // TODO: fix unsafe object to type conversion
        // - This happens when we merge the object and type representations
        val nType = convertObjectToType(paramObj, env).toOption.get
        val newEnv = env.newParam(paramName, nType)
        includeParams(addlParams, newEnv)
      }
      case _ => env
    }
  }

    // Already assume the object is a type
  // TODO - once objects and type are unified, this should become unneccesary
  def convertObjectToType(
    objectFound: NewMapObject,
    env: Environment
  ): Outcome[NewMapType, String] = {
    objectFound match {
      case Index(i) => Success(IndexT(i))
      case ObjectType => Success(ObjectT)
      case TypeType => Success(TypeT)
      case IdentifierType => Success(IdentifierT)
      case MapType(key, value, default) => {
        for {
          keyType <- convertObjectToType(key, env) 
          valueType <- convertObjectToType(value, env)
        } yield {
          MapT(keyType, valueType, default)
        }
      }
      case LambdaInstance(params, result) => {
        // TODO: remove unsafe call to objectParamsToParams
        val newParams = TypeChecker.objectParamsToParams(params, env)
        for {
          resultType <- convertObjectToType(result, env.newParams(newParams))
        } yield {
          LambdaT(newParams, resultType)
        }
      }
      case MapInstance(values, Index(1)) => {
        // TODO - require an explicit conversion here? Maybe this should be left to struct type
        for {
          // This must be a identifier -> type map
          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          StructT(newParams)
        }
      }
      case ParameterObj(name) => {
        for {
          typeOfObjectFound <- Outcome(env.typeOf(name), "Could not get type from object name " + name)

          _ <- Outcome.failWhen(
            !TypeChecker.refersToAType(typeOfObjectFound, env),
            "Could not confirm " + name + " as a type. The elements of type " + typeOfObjectFound.toString + " are not generally types themselves."
          )
        } yield {
          SubstitutableT(name)
        }
      }
      case ApplyFunction(func, input) => {
        for {
          functionApplied <- applyFunctionAttempt(func, input, env)
          result <- convertObjectToType(functionApplied, env)
        } yield (result)
      }
      case StructType(params) => {
        for {
          mapInstance <- this(params, env)

          values <- mapInstance match {
            case MapInstance(v, Index(1)) => Success(v)
            case _ => Failure("Map Instance could not be resolved")
          }

          newParams <- convertMapInstanceStructToParams(values, env)
        } yield {
          StructT(newParams)
        }
      }
      case _ => {
        // TODO: Need to explicitly handle every case
        Failure("Couldn't convert into type: " + objectFound + " -- could be unimplemented")
      }
    }
  }

  def convertMapInstanceStructToParams(
    values: Vector[(NewMapObject, NewMapObject)],
    env: Environment
  ): Outcome[Vector[(String, NewMapType)], String] = {
    values match {
      case (key, value) +: restOfValues => key match {
        case IdentifierInstance(s) => {
          for {
            valueType <- convertObjectToType(value, env)
            restOfParams <- convertMapInstanceStructToParams(restOfValues, env.newParam(s, valueType))
          } yield {
            (s -> valueType) +: restOfParams
          }
        }
        // TODO - what if the key substitutes to an identifier?? Better Logic on that
        case _ => Failure("Key must be identifier: " + key)
      }
      case _ => {
        Success(Vector.empty)
      }
    }
  }
}