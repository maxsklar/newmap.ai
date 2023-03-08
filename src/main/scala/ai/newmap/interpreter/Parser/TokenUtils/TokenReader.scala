package ai.newmap.interpreter.Parser.TokenUtils

import ai.newmap.interpreter.Lexer

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenReader(tokens: Seq[Lexer.Token]) extends Reader[Lexer.Token] {
  override def first: Lexer.Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[Lexer.Token] = new TokenReader(tokens.tail)
}