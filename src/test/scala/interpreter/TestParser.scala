package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.interpreter._
import ai.newmap.interpreter.Lexer._
import ai.newmap.interpreter.NewMapParser._
import ai.newmap.model._
import ai.newmap.util.Success

class TestParser extends FlatSpec {
  "A number " should " be parsed correctly" in {
    val tokens = Vector(Number(123))
    assert(NewMapParser(tokens) == Success(
      NaturalNumberParse(123)
    ))
  }

  "An identifier " should " be parsed correctly" in {
    val tokens = Vector(Identifier("x"))
    assert(NewMapParser(tokens) == Success(
      IdentifierParse("x")
    ))
  }

  it should " work when forced with a Tilda" in {
    val tokens = Vector(Tilda(), Identifier("x"))
    assert(NewMapParser(tokens) == Success(
      IdentifierParse("x", force = true)
    ))
  }

  "Parens for grouping " should " work on a number" in {
    val tokens = Vector(Enc(Paren, true), Number(6), Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      NaturalNumberParse(6)
    ))
  }

  "Apply function " should " work for f(x)" in {
    val tokens = Vector(Identifier("f"), Enc(Paren, true), Identifier("x"), Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      ApplyParse(IdentifierParse("f"), IdentifierParse("x"))
    ))
  }

  it should " work for f 5" in {
    val tokens = Vector(Identifier("f"), Number(5))
    assert(NewMapParser(tokens) == Success(
      ApplyParse(IdentifierParse("f"), NaturalNumberParse(5))
    ))
  }

  "An enclosure " should " be parsed with one element" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("digit"),
      Colon(),
      Number(10),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      Enclosure(Paren, Vector(IdentifierParse("digit") -> NaturalNumberParse(10)))
    ))
  }

  it should " be parsed with multiple elements" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("digit"),
      Colon(),
      Number(10),
      Comma(),
      Identifier("T"),
      Colon(),
      Identifier("Type"),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      Enclosure(
        Paren,
        Vector(
          IdentifierParse("digit") -> NaturalNumberParse(10),
          IdentifierParse("T") -> IdentifierParse("Type")
        )
      )
    ))
  }

  it should " be parsed with zero elements" in {
    val tokens = Vector(
      Enc(Paren, true),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      Enclosure(Paren, Vector.empty)
    ))
  }

  "A lambda expression " should " work in the simplest case" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("d"),
      Colon(),
      Number(5),
      Enc(Paren, false),
      Enc(CurlyBrace, true),
      Identifier("d"),
      Enc(CurlyBrace, false)
    )
    assert(NewMapParser(tokens) == Success(
      LambdaParse(Vector(IdentifierParse("d") -> NaturalNumberParse(5)), IdentifierParse("d"))
    ))
  }
}