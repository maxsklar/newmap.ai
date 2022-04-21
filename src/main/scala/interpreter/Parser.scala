package ai.newmap.interpreter

import ai.newmap.model._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, Position, NoPosition}
import ai.newmap.util.{Outcome, Success, Failure}

object NewMapParser extends Parsers {
  override type Elem = Lexer.Token

  class TokenReader(tokens: Seq[Lexer.Token]) extends Reader[Lexer.Token] {
    override def first: Lexer.Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Lexer.Token] = new TokenReader(tokens.tail)
  }

  private def naturalNumber: Parser[NaturalNumberParse] = {
    accept("number", { case Lexer.Number(i) => {
      NaturalNumberParse(i)
    }})
  }

  private def identifier: Parser[IdentifierParse] = {
    accept("identifier", { case Lexer.Identifier(id) => {
      IdentifierParse(id)
    }})
  }

  private def forcedId: Parser[IdentifierParse] = {
    Lexer.Tilda() ~ identifier ^^ {
      case _ ~ IdentifierParse(s, _) => IdentifierParse(s, force = true)
    }
  }

  private def emptyParens: Parser[ParseTree] = {
    Lexer.Enc(Paren, true) ~ Lexer.Enc(Paren, false) ^^ {
      case _ ~ _ => {
        CommandList(Vector.empty)
      }
    }
  }

  private def expressionInParens: Parser[ParseTree] = {
    Lexer.Enc(Paren, true) ~ expressionListWithOperations ~ Lexer.Enc(Paren, false) ^^ {
      case _ ~ exps ~ _ => {
        exps
      }
    }
  }

  private def comma: Parser[BinaryOpParse] = {
    accept("comma", { case Lexer.Comma() => {
      CommaBinaryOpParse()
    }})
  }

  private def colon: Parser[BinaryOpParse] = {
    accept("colon", { case Lexer.Colon() => {
      ColonBinaryOpParse()
    }})
  }

  private def arrow: Parser[BinaryOpParse] = {
    accept("arrow", { case Lexer.Arrow() => {
      ArrowBinaryOpParse()
    }})
  }

  private def period: Parser[BinaryOpParse] = {
    accept("period", { case Lexer.Period() => {
      PeriodBinaryOpParse()
    }})
  }

  private def binaryOpParse: Parser[BinaryOpParse] = {
    comma | colon | arrow
  }

  private def expressionListWithOperations: Parser[ParseTree] = {
    val pattern = expressionList ~ rep(binaryOpParse ~ expressionList)

    pattern ^^ {
      case startingExp ~ otherExps => {

        val o0: Vector[(BinaryOpParse, ParseTree)] = otherExps.map(_ match {
          case (symbol ~ exp) => (symbol, exp)
        }).toVector

        // Precedence of operations is, highest to lowest: Period, Colon, Comma, Arrow
        // Arrow associates to the right, while the others associate to the left

        // TODO - this binding code is confusing. It's just order of operations, should be
        //  possible to rewrite

        val (s2, o2) = bindBinaryOpParse(startingExp, o0, ColonBinaryOpParse(), (a, b, first) => {
          BindingCommandItem(a, b)
        })

        val (s3, o3) = bindBinaryOpParse(s2, o2, CommaBinaryOpParse(), (a, b, first) => {
          // CommandList(Vector(a, b))
          a match {
            case CommandList(commands) => CommandList(commands :+ b)
            case _ => CommandList(Vector(a, b))
          }
        })

        def bindArrow(in: ParseTree, out: ParseTree, first: Boolean): ParseTree = {
          in match {
            case LambdaParse(inIn, inOut) if (!first) => LambdaParse(inIn, bindArrow(inOut, out, false))
            case _ => LambdaParse(in, out)
          }
        }

        val (s4, o4) = bindBinaryOpParse(s3, o3, ArrowBinaryOpParse(), (a, b, first) => {
          bindArrow(a, b, first)
        })

        if (o4.nonEmpty) {
          println("Warning: the parser is dropping stuff: " + o3)
        }

        s4
      }
    }
  }

  def bindBinaryOpParse(
    startingExp: ParseTree,
    otherExpressions: Vector[(BinaryOpParse, ParseTree)],
    binaryOpToBind: BinaryOpParse,
    bindingInstructions: (ParseTree, ParseTree, Boolean) => ParseTree
  ): (ParseTree, Vector[(BinaryOpParse, ParseTree)]) = {
    var newStarting = startingExp

    val initial = otherExpressions.takeWhile(x => {
      x._1 == binaryOpToBind
    })

    var firstTime = true

    for {
      init <- initial
    } {
      newStarting = bindingInstructions(newStarting, init._2, firstTime)
      firstTime = false
    }

    val nextOtherExpressions = otherExpressions.drop(initial.length)

    val newOthers = nextOtherExpressions match {
      case exp +: tailExp => {
        var completedExps: Vector[(BinaryOpParse, ParseTree)] = Vector.empty
        var currentExp = exp
        var firstTime = true

        for {e <- tailExp} {
          if (e._1 == binaryOpToBind) {
            currentExp = currentExp._1 -> bindingInstructions(currentExp._2, e._2, firstTime)
            firstTime = false
          } else {
            completedExps :+= currentExp
            currentExp = e
            firstTime = true
          }
        }

        completedExps :+ currentExp
      }
      case _ => Vector.empty
    }

    (newStarting, newOthers)
  }

  private def baseExpression: Parser[ParseTree] = {
    expressionInParens | emptyParens | naturalNumber | identifier | forcedId
  }

  private def baseExpressionWithFieldAccess: Parser[ParseTree] = {
    val pattern = rep(baseExpression ~ period) ~ baseExpression
    pattern ^^ {
      case startingExps ~ lastExp => {
        val start: Vector[ParseTree] = startingExps.map(_ match {
          case (exp ~ _) => exp
        }).toVector

        val fullExps = start :+ lastExp
        fullExps reduceLeft ApplyParse
      }
    }
  }

  private def expressionList: Parser[ParseTree] = {
    rep1(baseExpressionWithFieldAccess) ^^ {
      case exps => exps reduceLeft ApplyParse
    }
  }

  private def fullStatement: Parser[FullStatementParse] = {
    Lexer.Identifier("val") ~ identifier ~ Lexer.Colon() ~ expressionListWithOperations ~ Lexer.Equals() ~ expressionListWithOperations ^^ {
      case _ ~ id ~ _ ~ typeExp ~ _ ~ exp => {
        FullStatementParse(ValStatement, id, typeExp, exp)
      }
    }
  }

  private def newVersionedStatement: Parser[NewVersionedStatementParse] = {
    Lexer.Identifier("ver") ~ identifier ~ Lexer.Equals() ~ Lexer.Identifier("new") ~ expressionListWithOperations ^^ {
      case _ ~ id ~ _ ~ _ ~ exp => {
        NewVersionedStatementParse(id, exp)
      }
    }
  }

  // TODO - why isn't this working!!
  /*private def newVersionedStatement: Parser[NewVersionedStatementParse] = {
    Lexer.Identifier("new") ~ expressionListWithOperations ~ Lexer.Identifier("as") ~ identifier ^^ {
      case _ ~ exp ~ _ ~ id => {
        NewVersionedStatementParse(id, exp)
      }
    }
  }*/

  private def forkedVersionedStatement: Parser[ForkedVersionedStatementParse] = {
    Lexer.Identifier("fork") ~ identifier ~ Lexer.Identifier("as") ~ identifier ^^ {
      case _ ~ existingId ~ _ ~ id => {
        ForkedVersionedStatementParse(id, existingId)
      }
    }
  }

  private def applyCommand: Parser[ApplyCommandStatementParse] = {
    Lexer.Identifier("update") ~ identifier ~ expressionListWithOperations ^^ {
      case _ ~ id ~ exp => {
        ApplyCommandStatementParse(id, exp)
      }
    }
  }

  private def applyCommands: Parser[ApplyCommandsStatementParse] = {
    Lexer.Identifier("updates") ~ identifier ~ expressionListWithOperations ^^ {
      case _ ~ id ~ exp => {
        ApplyCommandsStatementParse(id, exp)
      }
    }
  }

  private def inferredTypeStatement: Parser[InferredTypeStatementParse] = {
    Lexer.Identifier("val") ~ identifier ~ Lexer.Equals() ~ expressionListWithOperations ^^ {
      case _ ~ id ~ _ ~ exp => {
        InferredTypeStatementParse(ValStatement, id, exp)
      }
    }
  }

  private def expOnlyStatmentParse: Parser[ExpressionOnlyStatementParse] = {
    expressionListWithOperations ^^ {
      case exp => {
        ExpressionOnlyStatementParse(exp)
      }
    }
  }

  def apply(
    tokens: Seq[Lexer.Token]
  ): Outcome[ParseTree, String] = {
    val reader = new TokenReader(tokens)
    val program = phrase(expressionListWithOperations)

    program(reader) match {
      case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
      case Success(result, next) => ai.newmap.util.Success(result)
    }
  }

  def statementParse(
    tokens: Seq[Lexer.Token]
  ): Outcome[EnvStatementParse, String] = {
    val reader = new TokenReader(tokens)
    val program = phrase(fullStatement | newVersionedStatement | forkedVersionedStatement | applyCommand | applyCommands | inferredTypeStatement | expOnlyStatmentParse)

    program(reader) match {
      case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
      case Success(result, next) => ai.newmap.util.Success(result)
    }
  }
}