package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles the substitution of a parameter for it's given value
object MakeSubstitution {
  // TODO - once we implement the hashing, we can 
  def apply(
    expression: NewMapObject,
    env: Environment
  ): NewMapObject = {
    expression match {
      case nType: NewMapSubtype => makeRelevantSubstitutionsOfType(nType, env)
      case Index(_) | IdentifierInstance(_) | IdentifierPattern(_, _) => expression
      case ApplyFunction(func, input) => {
        ApplyFunction(
          this(func, env),
          this(input, env)
        )
      }
      case AccessField(struct, field) => {
        AccessField(
          this(struct, env),
          this(field, env)
        )
      }
      case ParameterObj(name, _) => {
        env.lookup(name) match {
          case Some(obj) => obj
          case None => expression
        }
      }
      case StructInstance(value, nType) => {
        StructInstance(
          value.map(x => (x._1 -> this(x._2, env))),
          nType
        )
      }
      case CaseInstance(constructor, value, caseT) => {
        CaseInstance(constructor, this(value, env), caseT)      
      }
      case IsCommandFunc | RangeFunc(_) | IncrementFunc => expression
      case MapInstance(values, mapT) => {
        val newValues = for {
          (k, v) <- values
        } yield (this(k, env) -> this(v, env))

        MapInstance(newValues,
          MapT(
            makeRelevantSubstitutionsOfType(mapT.inputType, env),
            makeRelevantSubstitutionsOfType(mapT.outputType, env),
            mapT.completeness,
            mapT.featureSet
        ))
      }
      case LambdaInstance(params, expression) => {
        // TODO - any way to remove reliance on the evaluator?
        val newEnv = Evaluator.includeLambdaParams(params, env)
        val newExpression = this(expression, newEnv)
        LambdaInstance(params, newExpression)
      }
    }
  }

  def makeRelevantSubstitutionsOfType(
    expression: NewMapSubtype,
    env: Environment
  ): NewMapSubtype = {
    expression match {
      case CountT | TypeT | AnyT | IdentifierT => expression
      case MapT(inputType, outputType, completeness, featureSet) => {
        MapT(
          makeRelevantSubstitutionsOfType(inputType, env),
          makeRelevantSubstitutionsOfType(outputType, env),
          completeness,
          featureSet
        )
      }
      case StructT(values) => {
        StructT(this(values, env))
      }
      case CaseT(cases) => {
        CaseT(this(cases, env))
      }
      case SubstitutableT(s, _) => {
        env.lookup(s) match {
          case Some(obj) => {
            // TODO(2022): This is unsafe! Separate out types in the environment and then it'll be safe again
            Evaluator.convertObjectToType(obj, env).toOption match {
              case Some(t) => t
              case None => throw new Exception(s"Unable to cast to type: $obj")
            }
          }
          case None => expression
        }
      }
      case SubtypeT(isMember) => {
        SubtypeT(this(isMember, env))
      }
    }
  }
}