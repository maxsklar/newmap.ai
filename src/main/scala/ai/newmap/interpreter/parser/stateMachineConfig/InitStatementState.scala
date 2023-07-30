package ai.newmap.interpreter.parser.stateMachineConfig

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Comment, Identifier}
import ai.newmap.model.{DefStatement, EmptyStatement, EnvStatementParse, ExpressionOnlyStatementParse, ParseTree, ValStatement}
import ai.newmap.util.{Failure, Success, Outcome}

case object InitStatementState extends ParseState[EnvStatementParse] {
  override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
    case Identifier(id) => id match {
      case "disconnectChannel" => Success(DisconnectChannelPath.InitState())
      case "iterate" => Success(IteratePath.InitState())
      case "connectChannel" => Success(ConnectChannelPath.InitState())
      case "fork" => Success(ForkPath.InitState())
      case "data" => Success(DataPath.InitState())
      case "write" => Success(WritePath.InitState())
      case "update" => Success(ApplyCommandPath.InitState())
      case "updates" => Success(ApplyCommandsPath.InitState())
      case "typeclass" => Success(TypeClassPath.InitState())
      case "ver" => Success(VersionedPath.InitState())
      case "new" => Success(VersionedPath.InitStateNew())
      case "val" => Success(ValPath.InitState(ValStatement))
      case "def" => Success(ValPath.InitState(DefStatement))
      case _ => ExpressionOnlyPath().update(token)
    }
    case Comment(_) => Success(this)
    case _ => {
      val state = ExpressionOnlyPath()
      state.update(token)
    }
  }

  override def generateOutput: Option[EnvStatementParse] = Some(EmptyStatement)
}

case class ExpressionOnlyPath(exp: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
  override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
    for {
      newExp <- exp.update(token)
    } yield this.copy(exp = newExp)
  }

  override def generateOutput: Option[EnvStatementParse] = {
    exp.generateOutput.map(ExpressionOnlyStatementParse(_))
  }
}