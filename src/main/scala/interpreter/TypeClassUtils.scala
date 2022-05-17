package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles type classes, and their composition
object TypeClassUtils {
  def typeIsExpectingAnIdentifier(
    nType: NewMapType,
    s: String,
    env: Environment
  ): Outcome[Unit, String] = nType match {
    case IdentifierT => Success()
    case CustomT(_, t) => typeIsExpectingAnIdentifier(t, s, env)
    case SubtypeT(isMember, parentType, _) => {
      for {
        _ <- typeIsExpectingAnIdentifier(parentType, s, env)
        membershipCheck <- Evaluator.applyFunctionAttempt(isMember, UIdentifier(s), env)
        _ <- Outcome.failWhen(membershipCheck == UInit, s"Value $s not a member of subtype $nType")
      } yield ()
    }
    case _ => {
      Failure(s"Type couldn't accept identifier $s as value: $nType")
    }
  }

  def typeIsExpectingAnIndex(
    nType: NewMapType,
    i: Long,
    env: Environment
  ): Outcome[Unit, String] = nType match {
    case TypeT => Success()
    case CountT => Success()
    case BooleanT => Outcome.failWhen(i > 1, s"Input to Boolean must be 0 or 1.. was $i")
    case IndexT(j) => Outcome.failWhen(i >= j, s"Proposed index $i is too large for type $j")
    case CustomT(_, t) => typeIsExpectingAnIndex(t, i, env)
    case SubtypeT(isMember, parentType, _) => {
      for {
        _ <- typeIsExpectingAnIndex(parentType, i, env)
        membershipCheck <- Evaluator.applyFunctionAttempt(isMember, UIndex(i), env)
        _ <- Outcome.failWhen(membershipCheck == UInit, s"Value $i not a member of subtype $nType")
      } yield ()
    }
    case StructT(params, _, _, _) => {
      if (params.length == 1) {
        for {
          typeObj <- Evaluator(params.head._2, env)
          t <- Evaluator.asType(typeObj, env)
          result <- typeIsExpectingAnIndex(t, i, env)
        } yield result
      } else {
        Failure(s"Struct Type couldn't accept index $i as value: $nType")
      }
    }
    case _ => {
      Failure(s"Type couldn't accept index $i as value: $nType")
    }
  }

  def intersectTypeClasses(
    typeClassA: Vector[NewMapPattern],
    typeClassB: Vector[NewMapPattern],
    env: Environment
  ): Vector[NewMapPattern] = {
    var retVal: Vector[NewMapPattern] = Vector.empty

    for {
      patternA <- typeClassA
      patternB <- typeClassB
    } {
      val remainingPatterns = intersectTypeClassPatterns(patternA, patternB, env)
      retVal = retVal ++ remainingPatterns
    }

    retVal
  }

  // Return conversion instructions
  def isTypeConvertibleToPattern(
    startingType: NewMapType,
    endingTypePattern: NewMapPattern,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    endingTypePattern match {
      case ObjectPattern(UType(endingType)) => {
        SubtypeUtils.isTypeConvertible(startingType, endingType, env)
      }
      case WildcardPattern(_) => {
        Success(Vector.empty)
      }
      case _ => {
        Failure(s"Unimplemented isTypeConvertibleToPattern: $startingType --> $endingTypePattern")
      }
    }
  }

  def isPatternConvertibleToPattern(
    startingTypePattern: NewMapPattern,
    endingTypePattern: NewMapPattern,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    startingTypePattern match {
      case ObjectPattern(UType(startingType)) => {
        isTypeConvertibleToPattern(startingType, endingTypePattern, env)
      }
      case ObjectPattern(UInit) => {
        throw new Exception(s"Here: $startingTypePattern --> $endingTypePattern")
      }
      case _ => {
        endingTypePattern match {
          case WildcardPattern(_) => Success(Vector.empty)
          case _ => {
            throw new Exception(s"Unimplemented isPatternConvertibleToPattern: $startingTypePattern --> $endingTypePattern")
            Failure(s"Unimplemented isPatternConvertibleToPattern: $startingTypePattern --> $endingTypePattern")
          }
        }
      }
    }
  }

  def isObjectConvertibleToPattern(
    startingObject: NewMapObject,
    endingTypePattern: NewMapPattern,
    env: Environment
  ): Outcome[Vector[NewMapObject], String] = {
    endingTypePattern match {
      case ObjectPattern(UType(endingType)) => {
        SubtypeUtils.isObjectConvertibleToType(startingObject, endingType, env)
      }
      case WildcardPattern(_) => {
        Success(Vector.empty)
      }
      case _ => {
        Failure(s"Unimplemented isObjectConvertibleToPattern: $startingObject --> $endingTypePattern")
      }
    }
  }

  def intersectTypeClassPatterns(
    typePatternA: NewMapPattern,
    typePatternB: NewMapPattern,
    env: Environment
  ): Vector[NewMapPattern] = {
    (typePatternA, typePatternB) match {
      case (ObjectPattern(objA), ObjectPattern(objB)) => {
        for {
          aT <- Evaluator.asType(objA, env)
          bT <- Evaluator.asType(objB, env)
        } yield {
          val aAllowed = SubtypeUtils.isTypeConvertible(aT, bT, env).toOption.nonEmpty
          val bAllowed = SubtypeUtils.isTypeConvertible(bT, aT, env).toOption.nonEmpty

          var retVal = Vector.empty
          if (aAllowed) retVal :+ typePatternA
          if (bAllowed) retVal :+ typePatternB
          retVal
        }
      }.toOption.getOrElse(Vector.empty)
      case (WildcardPattern(_), _) => Vector(typePatternB)
      case (_, WildcardPattern(_)) => Vector(typePatternA)
      case _ => Vector.empty
    }
  }
}