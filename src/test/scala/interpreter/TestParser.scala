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
      ApplyParse(IdentifierParse("f"), Vector(IdentifierParse("x")))
    ))
  }

  it should " work for f 5" in {
    val tokens = Vector(Identifier("f"), Number(5))
    assert(NewMapParser(tokens) == Success(
      ApplyParse(IdentifierParse("f"), Vector(NaturalNumberParse(5)))
    ))
  }

  "A command list " should " be parsed with one element" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("digit"),
      Colon(),
      Number(10),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      BindingCommandItem(IdentifierParse("digit"), NaturalNumberParse(10))
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
      CommandList(
        Vector(
          BindingCommandItem(IdentifierParse("digit"), NaturalNumberParse(10)),
          BindingCommandItem(IdentifierParse("T"), IdentifierParse("Type"))
        )
      )
    ))
  }

  it should " be parsed with zero elements" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("3"),
      Enc(Paren, false))
  }

  it should " work with singleton elements" in {
    val tokens = Vector(
      Enc(Paren, true),
      Number(10),
      Comma(),
      Number(5),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      CommandList(
        Vector(
          NaturalNumberParse(10), 
          NaturalNumberParse(5)
        )
      )
    ))
  }

  "A lambda expression " should " work in the simplest case" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("d"),
      Colon(),
      Number(5),
      Enc(Paren, false),
      Arrow(),
      Identifier("d")
    )
    assert(NewMapParser(tokens) == Success(
      LambdaParse(
        BindingCommandItem(IdentifierParse("d"), NaturalNumberParse(5)),
        IdentifierParse("d")
      )
    ))
  }

  it should " work correctly for multiple types" in {
    val tokens = Vector(Number(1), Arrow(), Number(2), Arrow(), Number(3))
    assert(NewMapParser(tokens) == Success(
      LambdaParse(NaturalNumberParse(1), LambdaParse(NaturalNumberParse(2), NaturalNumberParse(3)))
    ))
  }

  it should " work correctly when the types are grouped" in {
    val tokens = Vector(Enc(Paren, true), Number(1), Arrow(), Number(2), Enc(Paren, false), Arrow(), Number(3))
    assert(NewMapParser(tokens) == Success(
      LambdaParse(LambdaParse(NaturalNumberParse(1), NaturalNumberParse(2)), NaturalNumberParse(3))
    ))
  }

  "A lambda transformer " should " be possible to make" in {
    val tokens = Vector(
      LambdaTransformer(),
      Enc(Paren, isOpen = true),
      Identifier("T"),
      Arrow(),
      Identifier("T"),
      Enc(Paren, isOpen = false)
    )
    assert(NewMapParser(tokens) == Success(
      LambdaTransformerParse(
        LambdaParse(IdentifierParse("T"), IdentifierParse("T"))
      )
    ))
  }
}