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
    Lexer.Symbol("~") ~ identifier ^^ {
      case _ ~ IdentifierParse(s, _) => IdentifierParse(s, force = true)
    }
  }

  private def character: Parser[CharacterParse] = {
    Lexer.Symbol("`") ~ identifier ^^ {
      case _ ~ IdentifierParse(s, _) => CharacterParse(s)
    }
  }

  private def string: Parser[StringParse] = {
    accept("string", { case Lexer.DQuote(s) => {
      StringParse(s)
    }})
  }

  private def characterForNumber: Parser[CharacterParse] = {
    Lexer.Symbol("`") ~ naturalNumber ^^ {
      case _ ~ NaturalNumberParse(n) => CharacterParse(n.toString)
    }
  }

  private def emptyParens: Parser[ParseTree] = {
    Lexer.Enc(Paren, true) ~ Lexer.Enc(Paren, false) ^^ {
      case _ ~ _ => {
        LiteralListParse(Vector.empty, MapType)
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

  private def emptyBrackets: Parser[ParseTree] = {
    Lexer.Enc(SquareBracket, true) ~ Lexer.Enc(SquareBracket, false) ^^ {
      case _ ~ _ => {
        LiteralListParse(Vector.empty, ArrayType)
      }
    }
  }

  private def nonEmptyBrackets: Parser[ParseTree] = {
    Lexer.Enc(SquareBracket, true) ~ expressionListWithOperations ~ Lexer.Enc(SquareBracket, false) ^^ {
      case _ ~ LiteralListParse(values, _) ~ _ => {
        LiteralListParse(
          values,
          ArrayType // Square brackets indicate an array
        )
      }
      case _ ~ exp ~ _ => {
        // Singleton array case
        LiteralListParse(
          Vector(exp),
          ArrayType
        )
      }
    }
  }

  private def comma: Parser[BinaryOpParse] = {
    accept("comma", { case Lexer.Symbol(",") => {
      CommaBinaryOpParse()
    }})
  }

  private def colon: Parser[BinaryOpParse] = {
    accept("colon", { case Lexer.Symbol(":") => {
      ColonBinaryOpParse()
    }})
  }

  private def arrow: Parser[BinaryOpParse] = {
    accept("arrow", { case Lexer.Symbol("=>") => {
      ArrowBinaryOpParse()
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
          KeyValueBinding(a, b)
        })

        val (s3, o3) = bindBinaryOpParse(s2, o2, CommaBinaryOpParse(), (a, b, first) => {
          // LiteralListParse(Vector(a, b), _)
          // This part of the parser has been such an annoyance!!
          // Assumed to be a map and not an array without brackets
          a match {
            case LiteralListParse(items, t) => LiteralListParse(items :+ b, t)
            case _ => LiteralListParse(Vector(a, b), MapType)
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
    expressionInParens | nonEmptyBrackets | emptyParens | emptyBrackets | naturalNumber | identifier | forcedId | character | characterForNumber | string
  }

  private def baseExpressionWithFieldAccess: Parser[ParseTree] = {
    val pattern = rep(baseExpression ~ Lexer.Symbol("|")) ~ baseExpression
    pattern ^^ {
      case startingExps ~ lastExp => {
        val start: Vector[ParseTree] = startingExps.map(_ match {
          case (exp ~ _) => exp
        }).toVector

        val fullExps = start :+ lastExp
        fullExps reduceLeft ConstructCaseParse
      }
    }
  }

  private def expressionList: Parser[ParseTree] = {
    rep1(baseExpressionWithFieldAccess) ^^ {
      case exps => exps reduceLeft ApplyParse
    }
  }

  private def fullStatement: Parser[FullStatementParse] = {
    Lexer.Identifier("val") ~ identifier ~ Lexer.Symbol(":") ~ expressionListWithOperations ~ Lexer.Symbol("=") ~ expressionListWithOperations ^^ {
      case _ ~ id ~ _ ~ typeExp ~ _ ~ exp => {
        FullStatementParse(ValStatement, id, typeExp, exp)
      }
    }
  }

  private def newVersionedStatement: Parser[NewVersionedStatementParse] = {
    Lexer.Identifier("ver") ~ identifier ~ Lexer.Symbol("=") ~ Lexer.Identifier("new") ~ expressionListWithOperations ^^ {
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

  private def newTypeCommand: Parser[NewTypeStatementParse] = {
    Lexer.Identifier("data") ~ identifier ~ Lexer.Symbol("=") ~ expressionListWithOperations ^^ {
      case _ ~ id ~ _ ~ exp => {
        NewTypeStatementParse(id, exp)
      }
    }
  }

  private def newParamTypeCommand: Parser[NewParamTypeStatementParse] = {
    Lexer.Identifier("data") ~ identifier ~ expressionListWithOperations ^^ {
      case _ ~ id ~ params => {
        NewParamTypeStatementParse(id, params)
      }
    }
  }

  private def newTypeClassCommand: Parser[NewTypeClassStatementParse] = {
    Lexer.Identifier("typeclass") ~ identifier ~ expressionListWithOperations ^^ {
      case _ ~ id ~ exp => {
        NewTypeClassStatementParse(id, exp)
      }
    }
  }

  private def iterateIntoCommand: Parser[IterateIntoStatementParse] = {
    Lexer.Identifier("iterate") ~ identifier ~ Lexer.Identifier("into") ~ identifier ^^ {
      case _ ~ obj ~ _ ~ dest => {
        IterateIntoStatementParse(obj, dest)
      }
    }
  }

  private def addChannel: Parser[AddChannelParse] = {
    Lexer.Identifier("addChannel") ~ identifier ~ expressionListWithOperations ^^ {
      case _ ~ id ~ obj => {
        AddChannelParse(id, obj)
      }
    }
  }

  private def connectChannel: Parser[ConnectChannelParse] = {
    Lexer.Identifier("connectChannel") ~ identifier ~ identifier ^^ {
      case _ ~ id ~ obj => {
        ConnectChannelParse(id, obj)
      }
    }
  }

  private def disconnectChannel: Parser[DisconnectChannelParse] = {
    Lexer.Identifier("disconnectChannel") ~ identifier ~ identifier ^^ {
      case _ ~ id ~ obj => {
        DisconnectChannelParse(id, obj)
      }
    }
  }

  private def writeToChannel: Parser[WriteToChannelParse] = {
    Lexer.Identifier("write") ~ identifier ~ expressionListWithOperations ^^ {
      case _ ~ id ~ obj => {
        WriteToChannelParse(id, obj)
      }
    }
  }

  private def inferredTypeStatement: Parser[InferredTypeStatementParse] = {
    Lexer.Identifier("val") ~ identifier ~ Lexer.Symbol("=") ~ expressionListWithOperations ^^ {
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

  // New Statment for defining a function
  /*private def defFunctionStatment: Parser[DefineFunctionStatment] = {
    Lexer.Identifier("def") ~ identifier ~ Lexer.Colon() ~ expressionListWithOperations ~ Lexer.Symbol("=") ~ expressionListWithOperations ^^ {
      case _ ~ id ~ _ ~ typeExp ~ _ ~ exp => {
        FullStatementParse(ValStatement, id, typeExp, exp)
      }
    }
  }*/

  def apply(
    tokens: Seq[Lexer.Token]
  ): Outcome[ParseTree, String] = {
    val tokensWithoutComments = tokens.filter(!isComment(_))

    if (tokensWithoutComments.isEmpty) {
      ai.newmap.util.Success(EmptyParse())
    } else {
      val reader = new TokenReader(tokensWithoutComments)
      val program = phrase(expressionListWithOperations)

      program(reader) match {
        case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
        case Success(result, next) => ai.newmap.util.Success(result)
      }
    }
  }

  def isComment(token: Lexer.Token): Boolean = token match {
    case Lexer.Comment(_) => true
    case _ => false
  }

  def statementParse(
    tokens: Seq[Lexer.Token]
  ): Outcome[EnvStatementParse, String] = {
    val tokensWithoutComments = tokens.filter(!isComment(_))

    if (tokensWithoutComments.isEmpty) {
      ai.newmap.util.Success(ExpressionOnlyStatementParse(EmptyParse()))
    } else {
      val reader = new TokenReader(tokensWithoutComments)
      val program = phrase(fullStatement | newVersionedStatement | newParamTypeCommand | newTypeClassCommand | iterateIntoCommand | addChannel | connectChannel | disconnectChannel | writeToChannel | newTypeCommand | forkedVersionedStatement | applyCommand | applyCommands | inferredTypeStatement | expOnlyStatmentParse)

      program(reader) match {
        case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
        case Success(result, next) => ai.newmap.util.Success(result)
      }
    }
  }
}