package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer.{Identifier, Symbol}
import ai.newmap.model._
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

/** Parse map/field creation like so:
 * new basic map on 3 as f returning String = ...
 *
 * Later on, you can then call 3.f and recieve a String
 */
object NewFieldVersionedPath {
  case class InitStateNewExpressionAsIdentifier(exp: ParseTree, id: String) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      Failure("New Versioned Items can only be stored in an identifier")
    }

    override def generateOutput: Option[EnvStatementParse] = {
      Some(NewVersionedStatementParse(IdentifierParse(id), exp))
    }
  }

  case class NewMapEq(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    id: String,
    returnTypeParse: ParseTree,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
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
        NewVersionedFieldParse(featureSet, typeParse, id, returnTypeParse, parseTree)
      }
    }
  }

  case class NewMapOnExpIdReturning(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    id: String,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpressionOpt = expressionState.generateOutput
      (token, currentExpressionOpt) match {
        case (Symbol("="), Some(currentExpression)) => {
          Success(NewMapEq(featureSet, typeParse, id, currentExpression))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield NewMapOnExpIdReturning(featureSet, typeParse, id, newExpressionState)
        }
      }
    }
  }

  case class NewMapOnExpId(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    id: String
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificIdentifier(token, "returning", NewMapOnExpIdReturning(featureSet, typeParse, id))
    }
  }

  case class NewMapOnExp(
    featureSet: MapFeatureSet,
    typeParse: ParseTree
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => NewMapOnExpId(featureSet, typeParse, id))
    }
  }

  case class NewMapOnCalled(
    featureSet: MapFeatureSet,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpressionOpt = expressionState.generateOutput
      (token, currentExpressionOpt) match {
        case (Identifier("as"), Some(currentExpression)) => {
          Success(NewMapOnExp(featureSet, currentExpression))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield NewMapOnCalled(featureSet, newExpressionState)
        }
      }
    }
  }



  case class NewMapCalled(featureSet: MapFeatureSet) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificIdentifier(token, "on", NewMapOnCalled(featureSet))
    }
  }

  case class NewMapWithFeatureSet(featureSet: MapFeatureSet) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificIdentifier(token, "map", NewMapCalled(featureSet))
    }
  }

  case class InitState(expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      tokenToFeatureSet(token) match {
        case Some(featureSet) => Success(NewMapWithFeatureSet(featureSet))
        case None => token match {
          case Identifier("map") => Success(???)
          case Identifier(keyword) => Failure("Expected map or map qualifier(basic, pattern, simple, etc) " + keyword)
          case _ => Failure("Expected identifier, got token " + token)
        }
      }
    }

    def tokenToFeatureSet(token: Lexer.Token): Option[MapFeatureSet] = token match {
      case Identifier("basic") => Some(BasicMap)
      case Identifier("pattern") => Some(PatternMap)
      case Identifier("simple") => Some(SimpleFunction)
      case Identifier("grounded") => Some(WellFoundedFunction)
      case Identifier("full") => Some(FullFunction)
      case _ => None
    }
  }
}