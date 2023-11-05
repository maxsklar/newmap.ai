package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.{Identifier, Symbol}
import ai.newmap.model.{UpdateTypeclassWithFieldCommandParse, UpdateTypeclassWithTypeCommandParse, EnvStatementParse, IdentifierParse, ParseTree}
import ai.newmap.util.{Failure, Success, Outcome}

object UpdateClassPath {
  case class UpdateIdParamFieldAsName(
    identifier: String,
    param: String,
    nTypeExp: ParseTree,
    fieldName: String,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      for {
        newExpressionState <- expressionState.update(token)
      } yield this.copy(expressionState = newExpressionState)
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- expressionState.generateOutput
      } yield {
        UpdateTypeclassWithFieldCommandParse(
          identifier,
          param,
          nTypeExp,
          fieldName,
          parseTree,
          isCommand = false
        )
      }
    }
  }

  case class UpdateIdParamFieldAs(
    identifier: String,
    param: String,
    nTypeExp: ParseTree
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => UpdateIdParamFieldAsName(identifier, param, nTypeExp, id))
    }
  }

  case class UpdateIdParamField(
    identifier: String,
    param: String,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpOpt = expressionState.generateOutput
      (token, currentExpOpt) match {
        case (Identifier("as"), Some(currentExp)) => {
          Success(UpdateIdParamFieldAs(identifier, param, currentExp))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield {
            this.copy(expressionState = newExpressionState)
          }
        }
      }
    }
  }

  case class UpdateIdTypeColon(
    val identifier: String,
    val nTypeExp: ParseTree,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      for {
        newExpressionState <- expressionState.update(token)
      } yield this.copy(expressionState = newExpressionState)
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- expressionState.generateOutput
      } yield {
        UpdateTypeclassWithTypeCommandParse(
          IdentifierParse(identifier),
          nTypeExp,
          parseTree
        )
      }
    }
  }

  case class UpdateIdType(
    val identifier: String,
    val expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpOpt = expressionState.generateOutput
      (token, currentExpOpt) match {
        case (Symbol(":"), Some(currentExp)) => {
          Success(UpdateIdTypeColon(identifier, currentExp))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield {
            this.copy(expressionState = newExpressionState)
          }
        }
      }
    }
  }

  case class UpdateIdParamWith(
    val identifier: String,
    val paramName: String
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificToken(token, Identifier("field"), UpdateIdParamField(identifier, paramName))
    }
  }

  case class UpdateIdParam(
    val identifier: String,
    val paramName: String
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificToken(token, Identifier("with"), UpdateIdParamWith(identifier, paramName))
    }
  }

  case class UpdateIdWith(
    val identifier: String
  ) extends ParseState[EnvStatementParse] {

    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificToken(token, Identifier("type"), UpdateIdType(identifier))
    }
  }

  case class UpdateId(
    id: String
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("with") => Success(UpdateIdWith(id))
      case Identifier(paramName) => Success(UpdateIdParam(id, paramName))
      case _ => Failure("Expected identifier")
    }
  }

  case class InitState() extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => UpdateId(id))
    }
  }
}