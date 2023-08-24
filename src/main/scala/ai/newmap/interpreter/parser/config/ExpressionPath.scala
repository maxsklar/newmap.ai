package ai.newmap.interpreter.parser.config

import ai.newmap.interpreter.parser.ParseState
import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Lexer._
import ai.newmap.model._
import ai.newmap.util.{Failure, Success, Outcome}
import scala.collection.mutable.ListBuffer

object ExpressionPath {

  case class ExpressionInBinaryOpNoRight(
    symbol: String,
    firstParameter: ParseState[ParseTree]
  ) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      for {
        rightState <- InitState.update(token)
      } yield {
        ExpressionInBinaryOp(symbol, firstParameter, rightState)
      }
    }
  }

  case class ExpressionInBinaryOp(
    symbol: String,
    firstParameter: ParseState[ParseTree],
    secondParameter: ParseState[ParseTree]
  ) extends ParseState[ParseTree] {

    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      // Test if the second parameter is complete
      val secondParameterComplete = secondParameter.generateOutput
      
      if (secondParameterComplete.isEmpty) {
        // If the second parameter is not complete, then the new token must be for the second parameter
        for {
          newSecondParameter <- secondParameter.update(token)
        } yield this.copy(secondParameter = newSecondParameter)
      } else {
        val connectiveSymbol = token match {
          case Symbol(s) => s
          case _ => ""
        }

        val thisPrecedence = symbolPrecedence(connectiveSymbol)
        val currPrecedence = symbolPrecedence(symbol)

        val thisSymbolAssociation = symbolAssociation(connectiveSymbol)

        val applyToSecondParam = (thisPrecedence > currPrecedence) || (
          (connectiveSymbol == symbol) && (thisSymbolAssociation == Right)
        )

        if (applyToSecondParam) {
          for {
            newSecondParameter <- secondParameter.update(token)
          } yield this.copy(secondParameter = newSecondParameter)
        } else {
          token match {
            case Symbol(s) => Success(ExpressionInBinaryOpNoRight(connectiveSymbol, this))
            case _ => {
              for {
                newSecondParameter <- InitState.update(token)
              } yield ExpressionInBinaryOp(connectiveSymbol, this, newSecondParameter)
            }
          }
        }
      }
    }

    override def generateOutput: Option[ParseTree] = {
      for {
        firstExp <- firstParameter.generateOutput
        secondExp <- secondParameter.generateOutput

        result <- symbol match {
          case "." | "|" => Some(ConstructCaseParse(firstExp, secondExp))
          case ":" => Some(KeyValueBinding(firstExp, secondExp))
          case "," => {
            firstExp match {
              case LiteralListParse(items, MapType) => {
                Some(LiteralListParse(items :+ secondExp, MapType))
              }
              case _ => Some(LiteralListParse(Vector(firstExp, secondExp), MapType))
            }
          }
          case "=>" => Some(LambdaParse(firstExp, secondExp))
          case "" => Some(ApplyParse(firstExp, secondExp))
          case _ => {
            None
          }
        }
      } yield result
    }
  }

  case class ExpressionStart(parseTree: ParseTree) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = token match {
      // TODO - these items shouldn't be lexed as symbols!!! 
      case Symbol(s) if (s != "`" && s != "~") => {
        val thisPredecence = symbolPrecedence(s)
        Success(ExpressionInBinaryOpNoRight(s, this))
      }
      case NewLine() => Success(this)
      case _ => ExpressionInBinaryOpNoRight("", this).update(token)
    }

    override def generateOutput: Option[ParseTree] = {
      Some(parseTree)
    }
  }

  case class ExpressionInEnc(
    enc: EnclosureSymbol,
    exp: ParseState[ParseTree] = InitState
  ) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      exp.update(token) match {
        case Success(result) => Success(ExpressionInEnc(enc, result))
        case Failure(reason) => token match {
          case Enc(encS, false) => {
            if (encS == enc) {
              exp.generateOutput match {
                case Some(parseTree) => {
                  val expression = enc match {
                    case Paren => parseTree match {
                      case EmptyParse => LiteralListParse(Vector.empty, MapType)
                      case _ => parseTree
                    }
                    case SquareBracket => parseTree match {
                      case EmptyParse => LiteralListParse(Vector.empty, ArrayType)
                      case LiteralListParse(values, _) => LiteralListParse(values, ArrayType) // Square brackets indicate an array
                      case _ => LiteralListParse(Vector(parseTree), ArrayType)
                    }
                    case _ => {
                      //TODO: handle this properly
                      throw new Exception("Curly Brace Alert")
                    }
                  }

                  Success(ExpressionStart(expression))
                }
                case None => Failure("Tried to close " + Lexer.closedFormOfEnclosure(enc) + " with unfinished expression " + exp.toString)
              }
            } else {
              Failure("Tried to close " + Lexer.closedFormOfEnclosure(encS) + " but it matched with " + Lexer.openFormOfEnclosure(enc))
            }
          }
          case _ => Failure(reason)
        }
      }
    }
  }

  case class MultilineMidStatement(
    statementState: ParseState[EnvStatementParse],
    existingStatements: Seq[EnvStatementParse] = Vector.empty
  ) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      (statementState.generateOutput, token) match {
        case (Some(statement), Identifier(id)) => {
          val newStatementList = existingStatements :+ statement
          InitStatementState.statementKeywordToState.get(id) match {
            case Some(state) => Success(MultilineMidStatement(state, newStatementList))
            case None => {
              for {
                newStatementState <- statementState.update(token)
              } yield this.copy(statementState = newStatementState)
            }
          }
        }
        case (Some(statement), NewLine()) => {
          val newStatementList = existingStatements :+ statement
          Success(MultilineExpression(newStatementList))
        }
        case (Some(statement), Enc(CurlyBrace, false)) => {
          val newStatementList = existingStatements :+ statement
          Success(ExpressionStart(LiteralCode(newStatementList.toVector, EmptyParse)))
        }
        case _ => {
          for {
            newStatementState <- statementState.update(token)
          } yield this.copy(statementState = newStatementState)
        }
      }
    }
  }

  case class MultilineMidExpression(
    currentExpression: ParseState[ParseTree],
    existingStatements: Seq[EnvStatementParse] = Vector.empty
  ) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      (currentExpression.generateOutput, token) match {
        case (Some(expression), Enc(CurlyBrace, false)) => {
          Success(ExpressionStart(LiteralCode(existingStatements.toVector, expression)))
        }
        case _ => {
          // Then we continue building ths current expression
          for {
            newExpressionState <- currentExpression.update(token)
          } yield {
            this.copy(currentExpression = newExpressionState)
          }
        }
      }
    }
  }

  case class MultilineExpression(
    existingStatements: Seq[EnvStatementParse] = Vector.empty
  ) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      token match {
        case Identifier(id) => InitStatementState.statementKeywordToState.get(id) match {
          case Some(state) => Success(MultilineMidStatement(state, existingStatements))
          case None => {
            InitState.update(token).map(expressionState => {
              MultilineMidExpression(expressionState, existingStatements)
            })
          }
        }
        case Enc(CurlyBrace, false) => {
          // You've closed the block without having any expression
          Success(ExpressionStart(EmptyParse))
        }
        case NewLine() => Success(this)
        case _ => InitState.update(token).map(expressionState => {
          MultilineMidExpression(expressionState, existingStatements)
        })
      }
    }
  }

  case class UnaryExpression(s: String, internalExpression: ParseState[ParseTree]) extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = {
      internalExpression.update(token) match {
        case Success(newInternalExpression) => Success(this.copy(internalExpression = newInternalExpression))
        case Failure(reason) => {
          generateOutput match {
            case Some(expression) => ExpressionStart(expression).update(token)
            case None => Failure("Unary Expression Failure: " + s + " -- " + internalExpression + " -- " + token + " -- " + reason)
          }
        }
      }
    }

    override def generateOutput: Option[ParseTree] = {
      internalExpression.generateOutput.map(expression => {
        val symbolAsIdentifier = IdentifierParse(s, force = false)
        ApplyParse(symbolAsIdentifier, expression)
      })
    }
  }

  case class ExpressionForceId() extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = token match {
      case Identifier(id) => Success(ExpressionStart(IdentifierParse(id, true)))
      case _ => Failure("Expected identifier after ~")
    }
  }

  case class ExpressionTickMark() extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = token match {
      case Identifier(id) => Success(ExpressionStart(CharacterParse(id)))
      case Number(n) => Success(ExpressionStart(CharacterParse(n.toString)))
      case _ => Failure("Token " + token + " doesn't go after a tick mark")
    }
  }

  case object InitState extends ParseState[ParseTree] {
    override def update(token: Lexer.Token): Outcome[ParseState[ParseTree], String] = token match {
      case Enc(enc, true) => enc match {
        case Paren => Success(ExpressionInEnc(Paren))
        case SquareBracket => Success(ExpressionInEnc(SquareBracket))
        case CurlyBrace => Success(MultilineExpression())
      }
      case Enc(symbol, false) => {
        Failure("Unmatched enclosure symbol: " + Lexer.closedFormOfEnclosure(symbol))
      }
      case Identifier(id) => Success(ExpressionStart(IdentifierParse(id)))
      case Number(i) => Success(ExpressionStart(NaturalNumberParse(i)))
      case Symbol("~") => Success(ExpressionForceId())
      case Symbol("`") => Success(ExpressionTickMark())
      case Symbol(s) => Success(UnaryExpression(s, InitState))
      case DQuote(s) => Success(ExpressionStart(StringParse(s)))
      case Comment(_) | NewLine() => Success(this)
    }

    override def generateOutput: Option[ParseTree] = Some(EmptyParse)
  }

  def symbolPrecedence(symbol: String): Int = symbol match {
    case "|" => 11
    case "" => 10
    case "." => 9
    case "^" => 8
    case "*" | "/" => 7
    case "+" | "-" => 6
    case ":" => 5
    case "==" => 4
    case "," => 3
    case "=>" => 1
    case _ => {
      println("Unknown Symbol: " + symbol)
      0
    }
  }

  sealed abstract class SymbolAssociation
  case object Right extends SymbolAssociation
  case object Left extends SymbolAssociation

  def symbolAssociation(symbol: String): SymbolAssociation = symbol match {
    case "=>" => Right
    case _ => Left
  }
}