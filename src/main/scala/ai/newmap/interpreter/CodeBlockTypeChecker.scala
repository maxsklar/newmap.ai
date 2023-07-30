package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

object CodeBlockTypeChecker {
  def apply(
    statements: Seq[EnvStatementParse],
    expression: ParseTree,
    expectedType: NewMapType,
    env: Environment,
    featureSet: MapFeatureSet
  ): Outcome[TypeChecker.TypeCheckResponse, String] = {
    statements match {
      case firstStatement +: otherStatements => {
        for {
          envCommand <- StatementInterpreter(firstStatement, env)

          // TODO - what if the command is not executable yet due to unset variables, but we still know what the types are going to be?
          newEnv = env.newCommand(envCommand)

          result <- this(otherStatements, expression, expectedType, newEnv, featureSet)
        } yield result
      }
      case Nil => {
        TypeChecker.typeCheck(expression, expectedType, env, featureSet)
      }
    }
  }
}