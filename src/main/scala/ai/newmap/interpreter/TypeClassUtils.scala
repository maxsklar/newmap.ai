package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles type classes, and their composition
object TypeClassUtils {
  // If the type can be represented by an index, return the refined version of the type that does this.
  def typeIsExpectingAnIndex(
    nType: NewMapType,
    i: Long,
    env: Environment
  ): Outcome[NewMapType, String] = {
    nType match {
      case WildcardT(_) => Success(CountT)
      case TypeT => Success(nType)
      case CountT => Success(nType)
      case BooleanT => {
        for {
          _ <- Outcome.failWhen(i > 1, s"Input to Boolean must be 0 or 1.. was $i")
        } yield nType
      }
      case IndexT(UIndex(j)) => {
        for {
          _ <- Outcome.failWhen(i >= j, s"Proposed index $i is too large for type $j")
        } yield nType
      }
      case CustomT(name, _, typeSystemId) => {
        for {
          underlyingType <- env.typeSystem.historicalUnderlyingType(name, typeSystemId)

          //TODO: The env should be updated with currentParameterPattern
          result <- typeIsExpectingAnIndex(underlyingType._2, i, env)
        } yield result
        
      }
      case SubtypeT(isMember, parentType, _) => {
        for {
          refinedType <- typeIsExpectingAnIndex(parentType, i, env)
          membershipCheck <- Evaluator.applyFunction(isMember, UIndex(i), env)
          _ <- Outcome.failWhen(membershipCheck == UInit, s"Value $i not a member of subtype $nType")
        } yield refinedType
      }
      case StructT(UMap(params), _, _, _) => {
        if (params.length == 1) {
          for {
            typeObj <- Evaluator(params.head._2, env)
            t <- typeObj.asType
            result <- typeIsExpectingAnIndex(t, i, env)
          } yield result
        } else {
          Failure(s"Struct Type couldn't accept index $i as value: $nType")
        }
      }
      case ParamIdT(_) => {
        Success(nType) // Is this right?
      }
      case _ => {
        Failure(s"Type couldn't accept index $i as value: $nType")
      }
    }
  }
}