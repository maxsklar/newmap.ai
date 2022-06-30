package ai.newmap.interpreter

import ai.newmap.model._
import scala.util.parsing.combinator.RegexParsers
import ai.newmap.util.{Outcome, Success, Failure}

object Lexer extends RegexParsers {
  sealed trait Token
  
  case class Enc(symbol: EnclosureSymbol, isOpen: Boolean) extends Token
  case class Identifier(id: String) extends Token
  case class Number(i: Long) extends Token
  case class Comma() extends Token
  case class Colon() extends Token
  case class Equals() extends Token
  case class Tilda() extends Token
  case class Arrow() extends Token
  case class Period() extends Token
  case class TikMark() extends Token
  case class DQuote(s: String) extends Token

  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def identifier: Parser[Identifier] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }
  }

  def number: Parser[Number] = {
    "0|([1-9][0-9]*)".r ^^ { str => Number(str.toLong) }
  }

  def comma: Parser[Comma] = {
    "," ^^ { str => Comma() }
  }

  def colon: Parser[Colon] = {
    ":" ^^ { str => Colon() }
  }

  def tik: Parser[TikMark] = {
    "`" ^^ { str => TikMark() }
  }

  def dquote: Parser[DQuote] = {
    "\"([^\"]*)\"".r ^^ { str => DQuote(str) }
  }

  def equals: Parser[Equals] = {
    "=" ^^ { str => Equals() }
  }

  def tilda: Parser[Tilda] = {
    "~" ^^ { str => Tilda() }
  }

  def arrow: Parser[Arrow] = {
    "=>" ^^ { str => Arrow() }
  }

  def period: Parser[Period] = {
    "." ^^ { str => Period() }
  }

  def enclosure: Parser[Enc] = {
    "[\\(\\[\\{\\}\\]\\)]".r ^^ { str => {
      str match {
        case "(" => Enc(Paren, isOpen = true)
        case ")" => Enc(Paren, isOpen = false)
        case "[" => Enc(SquareBracket, isOpen = true)
        case "]" => Enc(SquareBracket, isOpen = false)
        case "{" => Enc(CurlyBrace, isOpen = true)
        case "}" => Enc(CurlyBrace, isOpen = false)
      }
    }}
  }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(identifier | number | comma | colon | tik | dquote | arrow | equals | tilda | period | enclosure )) ^^ { rawTokens =>
      rawTokens
    }
  }

  def apply(code: String): Outcome[List[Token], String] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => ai.newmap.util.Failure(msg)
      case Success(result, next) => ai.newmap.util.Success(result)
    }
  }
}