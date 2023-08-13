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
    featureSet: MapFeatureSet,
    tcParameters: Map[String, NewMapType]
  ): Outcome[TypeChecker.TypeCheckResponse, String] = {
    if (statements.isEmpty) {
      TypeChecker.typeCheck(expression, expectedType, env, featureSet, tcParameters)
    } else {
      var envCommands: Seq[EnvironmentCommand] = Vector.empty

      // TODO - what if the command is not executable yet due to unset variables, but we still know what the types are going to be?
      var newEnv: Environment = env

      statements.foreach(statement => {
        StatementInterpreter(statement, newEnv) match {
          case Success(envCommand) => {
            // TODO - are we getting new parameters here too?
            envCommands = envCommands :+ envCommand
            newEnv = newEnv.newCommand(envCommand)
          }
          case Failure(f) => return Failure(f)
        }
      })

      for {
        tcResult <- TypeChecker.typeCheck(expression, expectedType, newEnv, featureSet, tcParameters)
      } yield {
        // TODO - there's a bug here where the expression is a ULink that points to an object in the newEnv that doesn't exist in env.
        val resultingExpression = ULet(envCommands.toVector, tcResult.nExpression)
        TypeChecker.TypeCheckResponse(resultingExpression, tcResult.refinedTypeClass)
      }
    }
  }
}