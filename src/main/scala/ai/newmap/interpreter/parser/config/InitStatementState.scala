package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Comment, Identifier}
import ai.newmap.model.{DefStatement, EmptyStatement, EnvStatementParse, ExpressionOnlyStatementParse, ParseTree, ValStatement}
import ai.newmap.util.{Success, Outcome}

case object InitStatementState extends ParseState[EnvStatementParse] {
  override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
    case Identifier(id) => statementKeywordToState.get(id) match {
      case Some(state) => Success(state)
      case None => ExpressionOnlyPath().update(token)
    }
    case Comment(_) => Success(this)
    case _ => ExpressionOnlyPath().update(token)
  }

  val statementKeywordToState: Map[String, ParseState[EnvStatementParse]] = Map(
    "disconnectChannel" -> DisconnectChannelPath.InitState(),
    "iterate" -> IteratePath.InitState(),
    "connectChannel" -> ConnectChannelPath.InitState(),
    "fork" -> ForkPath.InitState(),
    "data" -> DataPath.InitState(),
    "write" -> WritePath.InitState(),
    "update" -> ApplyCommandPath.InitState(),
    "updates" -> ApplyCommandsPath.InitState(),
    "typeclass" -> TypeClassPath.InitState(),
    "ver" -> VersionedPath.InitState(),
    "new" -> NewFieldVersionedPath.InitState(),
    "val" -> ValPath.InitState(ValStatement),
    "def" -> ValPath.InitState(DefStatement),
  )

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