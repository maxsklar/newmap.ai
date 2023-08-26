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

      var newParams: Map[String, NewMapType] = tcParameters

      statements.foreach(statement => {
        val result = for {
          interpreted <- StatementInterpreter(statement, env, newParams)
        } yield {
          envCommands = envCommands :+ interpreted.command
          newParams = interpreted.tcParameters
        }
      })

      for {
        tcResult <- TypeChecker.typeCheck(expression, expectedType, env, featureSet, newParams)
      } yield {
        val resultingExpression = ULet(envCommands.toVector, tcResult.nExpression)
        TypeChecker.TypeCheckResponse(resultingExpression, tcResult.refinedTypeClass)
      }
    }
  }
}