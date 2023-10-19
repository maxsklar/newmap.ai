package ai.newmap.parser.config

import ai.newmap.parser.ParseState
import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.{Identifier, Symbol}
import ai.newmap.model._
import ai.newmap.util.{Failure, Success, Outcome}

/** Parse map/field creation like so:
 * new basic map on 3 as f returning String = ...
 *
 * Later on, you can then call 3.f and recieve a String
 */
object NewFieldVersionedPath {
  sealed abstract class NewKeywordConstruction
  case object MapCons extends NewKeywordConstruction
  case object CommandCons extends NewKeywordConstruction

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

  case class NewCommandOnExpWhereIdDotCommandPattern(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    takingTypeParse: ParseTree,
    selfPattern: String,
    commandName: String,
    inputPattern: ParseTree,
    outputRule: ParseState[ParseTree] = ExpressionPath.InitState 
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      for {
        newOutputRule <- outputRule.update(token)
      } yield this.copy(outputRule = newOutputRule)
    }

    override def generateOutput: Option[EnvStatementParse] = {
      for {
        parseTree <- outputRule.generateOutput
      } yield {
        NewCommandParse(featureSet, typeParse, takingTypeParse, selfPattern, commandName, inputPattern, parseTree)
      }
    }
  }

  case class NewCommandOnExpWhereIdDotCommand(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    takingTypeParse: ParseTree,
    selfPattern: String,
    commandName: String,
    inputPatternExpState: ParseState[ParseTree] = ExpressionPath.InitState 
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpressionOpt = inputPatternExpState.generateOutput
      (token, currentExpressionOpt) match {
        case (Symbol("="), Some(currentExpression)) => {
          Success(NewCommandOnExpWhereIdDotCommandPattern(
            featureSet,
            typeParse,
            takingTypeParse,
            selfPattern,
            commandName,
            currentExpression
          ))
        }
        case _ => {
          for {
            newExpressionState <- inputPatternExpState.update(token)
          } yield this.copy(inputPatternExpState = newExpressionState)
        }
      }
    }
  }

  case class NewCommandOnExpWhereIdDot(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    takingTypeParse: ParseTree,
    selfPattern: String
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => NewCommandOnExpWhereIdDotCommand(featureSet, typeParse, takingTypeParse, selfPattern, id))
    }
  }

  case class NewCommandOnExpWhereId(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    takingTypeParse: ParseTree,
    id: String
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificToken(token, Symbol("."), NewCommandOnExpWhereIdDot(featureSet, typeParse, takingTypeParse, id))
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

  case class NewCommandOnExpWhere(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    takingTypeParse: ParseTree
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingIdentifier(token, id => NewCommandOnExpWhereId(featureSet, typeParse, takingTypeParse, id))
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

  case class NewCommandOnExp(
    featureSet: MapFeatureSet,
    typeParse: ParseTree,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpressionOpt = expressionState.generateOutput
      (token, currentExpressionOpt) match {
        case (Identifier("where"), Some(currentExpression)) => {
          Success(NewCommandOnExpWhere(featureSet, typeParse, currentExpression))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield NewCommandOnExp(featureSet, typeParse, newExpressionState)
        }
      }
    }
  }

  case class NewOnCalled(
    featureSet: MapFeatureSet,
    cons: NewKeywordConstruction,
    expressionState: ParseState[ParseTree] = ExpressionPath.InitState
  ) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      val currentExpressionOpt = expressionState.generateOutput
      (token, currentExpressionOpt) match {
        case (Identifier("as"), Some(currentExpression)) if (cons == MapCons) => {
          Success(NewMapOnExp(featureSet, currentExpression))
        }
        case (Identifier("taking"), Some(currentExpression)) if (cons == CommandCons) => {
          Success(NewCommandOnExp(featureSet, currentExpression))
        }
        case (Identifier("where"), Some(currentExpression)) if (cons == CommandCons) => {
          Success(NewCommandOnExpWhere(featureSet, currentExpression, EmptyParse))
        }
        case _ => {
          for {
            newExpressionState <- expressionState.update(token)
          } yield NewOnCalled(featureSet, cons, newExpressionState)
        }
      }
    }
  }

  case class NewCalled(featureSet: MapFeatureSet, cons: NewKeywordConstruction) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      ParseState.expectingSpecificIdentifier(token, "on", NewOnCalled(featureSet, cons))
    }
  }

  case class NewMapWithFeatureSet(featureSet: MapFeatureSet) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = token match {
      case Identifier("map") => Success(NewCalled(featureSet, MapCons))
      case Identifier("command") => Success(NewCalled(featureSet, CommandCons))
      case _ => Failure("Expecting Map or Command")
    }
  }

  case class InitState(expressionState: ParseState[ParseTree] = ExpressionPath.InitState) extends ParseState[EnvStatementParse] {
    override def update(token: Lexer.Token): Outcome[ParseState[EnvStatementParse], String] = {
      tokenToFeatureSet(token) match {
        case Some(featureSet) => Success(NewMapWithFeatureSet(featureSet))
        case None => token match {
          case Identifier("map") => Success(NewCalled(BasicMap, MapCons))
          case Identifier("command") => Success(NewCalled(BasicMap, CommandCons))
          case Identifier(keyword) => Failure("Expected map or map qualifier(basic, pattern, simple, etc) " + keyword)
          case _ => Failure("Expected identifier, got token " + token)
        }
      }
    }

    def tokenToFeatureSet(token: Lexer.Token): Option[MapFeatureSet] = token match {
      case Identifier("basic") => Some(BasicMap)
      case Identifier("pattern") => Some(PatternMap)
      case Identifier("simple") => Some(SimpleFunction)
      case Identifier("recursive") => Some(WellFoundedFunction)
      case Identifier("turing") => Some(FullFunction)
      case _ => None
    }
  }
}