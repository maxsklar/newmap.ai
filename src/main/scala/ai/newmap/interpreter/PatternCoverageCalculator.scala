package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

/*
 * Tells us whether a set of pattern matches covers a full type, or whether there are possibly cases unaccounted for
 */
object PatternCoverageCalculator {
  // For ReqMaps, we need to ensure that all of the values are accounted for.
  // For Maps, we want to know that the default value is never used
  def doPatternsCoverType(
    keys: Vector[UntaggedObject],
    nType: NewMapType,
    env: Environment
  ): Outcome[Boolean, String] = {
    val nTypeOutcome = TypeChecker.getFinalUnderlyingType(nType, env)

    // This is the generic pattern, which means that everything will match
    // TODO: This is going to get more complicated with more patterns!!
    // - In the future, we want to know if the keys as a group have all the patterns to cover the type
    val WildcardExists = keys.exists(k => isCatchallPattern(k, nType, env))

    if (WildcardExists) {
      Success(true)
    }
    else {
      val piecemealCompletenessOutcome = nTypeOutcome match {
        case Success(CaseT(cases, _, _)) => {
          for {
            caseBindings <- cases.getMapBindings
          } yield {
            checkCaseComplete(keys, caseBindings, env)
          }
        }
        case Success(StructT(params, _, _, _)) => for {
          paramBindings <- params.getMapBindings
        } yield {
          checkStructComplete(keys, paramBindings, env)
        }
        case _ => Success(false)
      }

      piecemealCompletenessOutcome match {
        case Success(true) => Success(true)
        case _ => {
          val patternsMap = keys.map(key => key -> UIndex(1))

          for {
            keysToMatch <- IterationUtils.enumerateAllValuesIfPossible(nType, env)
          } yield {
            keysToMatch.forall(uKey => {
              Evaluator.patternMatchInOrder(patternsMap, uKey, env).isSuccess
            })
          }
        }
      }
    }
  }

  def checkCaseComplete(
    keys: Vector[UntaggedObject],
    cases: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Boolean = {
    // For each case key, we want to make sure that this case key is completely covered
    val constructors = cases.map(_._1)

    var returnVal = true

    for {
      constructor <- constructors
    } yield {
      // Look at our keys, and find the ones that are only for this case key, and save those patterns
      val patternsWithThisConstructor = keys.flatMap(key => key match {
        case UCase(untaggedConstructor, pattern) => {
          if (untaggedConstructor == constructor) Some(pattern) else None
        }
        case _ => None
      })

      // Find the input type for this constructor, and make sure that all of THOSE inputs are accounted for
      // TODO - cleanup this pattern matching!!!
      Evaluator.patternMatchInOrder(cases, constructor, env) match {
        case Success(inputTypeExpression) => {
          //println(s"$constructor --- $inputTypeExpression -- $returnVal")
          val inputType = Evaluator(inputTypeExpression, env).toOption.get
          // TODO - we need to be able to convert an expression to a pattern
          // - Some of the parameters in the expression with map to patterns instead of objects
          // - Whole new Evaluator line!!

          inputType.asType match {
            case Failure(_) => returnVal = false
            case Success(inputTypeT) => {
              doPatternsCoverType(patternsWithThisConstructor, inputTypeT, env) match {
                case Success(false) | Failure(_) => returnVal = false
                case _ => ()
              }
            }
          }
        }
        case _ => returnVal = false
      }
    }

    returnVal
  }

  def checkStructComplete(
    keys: Vector[UntaggedObject],
    params: Vector[(UntaggedObject, UntaggedObject)],
    env: Environment
  ): Outcome[Boolean, String] = {
    // TODO: There is surely a better, more comprehensive algorithm for this, but
    //  we'll have something here for now to attempt to solve this.

    // TODO: Implement
    Success(false)
  }

  // Returns true if the object is a pattern that will match everything in the type
  // nType is a pattern that represents a type
  def isCatchallPattern(pattern: UntaggedObject, nType: NewMapType, env: Environment): Boolean = {
    pattern match {
      case UWildcard(_) => true
      case UCase(UWildcard(_), UWildcard(_)) => true
      case _ => {
        val result = for {
          bindings <- pattern.getMapBindings
        } yield {
          isCatchallPatternStruct(bindings.map(_._2), nType, env)
        }

        result.toOption.getOrElse(false)
      }
    }
  }

  def isCatchallPatternStruct(patterns: Vector[UntaggedObject], nType: NewMapType, env: Environment): Boolean = {
    nType match {
      case StructT(params, _, _, _) => {
        val x = for {
          paramBindings <- params.getMapBindings.toOption

          if (paramBindings.length == patterns.length)
        } yield {
          (patterns, paramBindings.map(_._2)).zipped.toVector.forall(x => {
            Evaluator(x._2, env).toOption.map(nObject => {
              isCatchallPattern(x._1, nObject.asType.toOption.get, env)
            }).getOrElse(false) // We're not really set up for this yet!
          })
        }

        x.getOrElse(false)
      }
      case _ => false 
    }
  }
}