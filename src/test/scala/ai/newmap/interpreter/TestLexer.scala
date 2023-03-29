package ai.newmap.interpreter

import ai.newmap.interpreter.Lexer.{Enc, Identifier, Number, Symbol}
import org.scalatest._
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

  "A constructed value " should " be lexed correctly" in {
    assert(Lexer("a|b") == Success(List(
      Identifier("a"),
      Symbol("|"), 
      Identifier("b")
    )))
  }

  "A count statement" should " be lexed correctly" in {
    val code = "val a: Count = 5"
    assert(Lexer(code) == Success(List(
        Identifier("val"), 
        Identifier("a"), 
        Symbol(":"), 
        Identifier("Count"), 
        Symbol("="), 
        Number(5)
    )))
  }

  "A map statement" should " be lexed correctly" in {
    val code = "val a : Map(3, 100, 0) = (0:20, 1:43, 2:67)"
    assert(Lexer(code) == Success(List(
        Identifier("val"),
        Identifier("a"),
        Symbol(":"),
        Identifier("Map"),
        Enc(Paren, isOpen = true),
        Number(3),
        Symbol(","),
        Number(100),
        Symbol(","),
        Number(0),
        Enc(Paren, isOpen = false),
        Symbol("="),
        Enc(Paren, isOpen = true),
        Number(0),
        Symbol(":"),
        Number(20),
        Symbol(","),
        Number(1),
        Symbol(":"),
        Number(43),
        Symbol(","),
        Number(2),
        Symbol(":"),
        Number(67),
        Enc(Paren, isOpen = false)
      )))
    }

  "A list of parameters " should " be lexed correctly" in {
    val code = "(a: 6, b: a, c: Type)"
    assert(Lexer(code) == Success(List(
      Enc(Paren, isOpen = true),
      Identifier("a"),
      Symbol(":"),
      Number(6),
      Symbol(","),
      Identifier("b"),
      Symbol(":"),
      Identifier("a"),
      Symbol(","),
      Identifier("c"),
      Symbol(":"),
      Identifier("Type"),
      Enc(Paren, isOpen = false)
    )))
  }

  "An environment command " should " be lexed correctly" in {
    val code = "a: 10 = 6"
    assert(Lexer(code) == Success(List(
      Identifier("a"),
      Symbol(":"),
      Number(10),
      Symbol("="),
      Number(6)
    )))
  }

  "A struct field call " should " be lexed correctly" in {
    val code = "val a: 12 = date.getMonth"
    assert(Lexer(code) == Success(List(
      Identifier("val"),
      Identifier("a"),
      Symbol(":"),
      Number(12),
      Symbol("="),
      Identifier("date"),
      Symbol("."),
      Identifier("getMonth")
    )))
  }
}