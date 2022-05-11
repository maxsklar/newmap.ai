package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

// Handles type classes, and their composition
object TypeClassUtils {
  def typeIsExpectingAnIdentifier(
    nType: NewMapType,
    env: Environment
  ): Boolean = nType match {
    case IdentifierT => true
    case CustomT(_, t) => typeIsExpectingAnIdentifier(t, env)
    case SubtypeT(_, parentType, _) => typeIsExpectingAnIdentifier(parentType, env)
    case _ => false
  }

  def typeIsExpectingAnIndex(
    nType: NewMapType,
    env: Environment
  ): Boolean = nType match {
    case TypeT => true
    case CountT => true
    case IndexT(_) => true // with condition
    case CustomT(_, t) => typeIsExpectingAnIndex(t, env)
    case SubtypeT(_, parentType, _) => typeIsExpectingAnIndex(parentType, env)
    case _ => false
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