package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.interpreter._
import ai.newmap.interpreter.Lexer._
import ai.newmap.model.Paren
import ai.newmap.util.Success

class TestLexer extends FlatSpec {
  "An identifier " should " be lexed correctly" in {
    val code = "hello world"
    assert(Lexer(code) == Success(List(
      Identifier("hello"),
      Identifier("world")
    )))
  }

  "A number " should " be lexed correctly" in {
    val code = "hello 67 world 28"
    assert(Lexer(code) == Success(List(
      Identifier("hello"),
      Number(67),
      Identifier("world"),
      Number(28)
    )))
  }

  it should "divide up when starting with a 0" in {
    val code = "067 world"
    assert(Lexer(code) == Success(List(
      Number(0),
      Number(67),
      Identifier("world")
    )))
  }

  it should "succeed as a single 0 when starting with a 0" in {
    val code = "hello 0"
    assert(Lexer(code) == Success(List(
      Identifier("hello"),
      Number(0)
    )))
  }

  "A list of parameters " should " be lexed correctly" in {
    val code = "(a: 6, b: a, c: Type)"
    assert(Lexer(code) == Success(List(
      Enc(Paren, isOpen = true),
      Identifier("a"),
      Colon(),
      Number(6),
      Comma(),
      Identifier("b"),
      Colon(),
      Identifier("a"),
      Comma(),
      Identifier("c"),
      Colon(),
      Identifier("Type"),
      Enc(Paren, isOpen = false)
    )))
  }

  "An environment command " should " be lexed correctly" in {
    val code = "a: 10 = 6"
    assert(Lexer(code) == Success(List(
      Identifier("a"),
      Colon(),
      Number(10),
      Equals(),
      Number(6)
    )))
  }

  "A Lambda transformer expressioned " should " be lexed correctly" in {
    val code = "\\ (T => T)"
    assert(Lexer(code) == Success(List(
      LambdaTransformer(),
      Enc(Paren, isOpen = true),
      Identifier("T"),
      Arrow(),
      Identifier("T"),
      Enc(Paren, isOpen = false)
    )))
  }
}