package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success}

object StatementEvaluator {
  /*
   * @param sParse The statement parses
   * @param env This is a map of identifiers which at this point are supposed to be subsituted.
   * @return Evaluate the environment command so that it's ready to be executed
   */
  def apply(
    command: EnvironmentCommand,
    env: Environment
  ): Outcome[EnvironmentCommand, String] = {
    command match {
      case c@FullEnvironmentCommand(_, nExpression, _) => {
        for {
          evaluatedObject <- Evaluator(nExpression.uObject, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)
          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, nExpression.nType, env)
        } yield {
          c.copy(nObject = nObject)
        }
      }
      case c@ApplyIndividualCommand(_, command) => {
        for {
          evaluatedCommand <- Evaluator(command, env)
          constantCommand = Evaluator.stripVersioningU(evaluatedCommand, env)
        } yield {
          c.copy(nObject = constantCommand)
        }
      }
      case ExpOnlyEnvironmentCommand(exp) => {
        for {
          evaluatedObject <- Evaluator(exp.uObject, env)
          constantObject = Evaluator.stripVersioningU(evaluatedObject, env)
          nObject <- TypeChecker.tagAndNormalizeObject(constantObject, exp.nType, env)
        } yield {
          ExpOnlyEnvironmentCommand(nObject)
        }
      }
     case _ => Success(command)
    }
  }
}