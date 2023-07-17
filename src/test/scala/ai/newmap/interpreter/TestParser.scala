package ai.newmap.interpreter

import ai.newmap.interpreter.Lexer.{Enc, Identifier, Number, Symbol}
import ai.newmap.interpreter.parser.NewMapParser
import org.scalatest._
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
    val tokens = Vector(Symbol("~"), Identifier("x"))
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

  it should " apply values from left to right" in {
    val tokens = Vector(Identifier("f"), Number(5), Number(2))
    assert(NewMapParser(tokens) == Success(
      ApplyParse(
        ApplyParse(IdentifierParse("f"), NaturalNumberParse(5)),
        NaturalNumberParse(2)
      )
    ))
  }

  "A List of tokens of a count statement " should " be parsed correctly" in {
    val tokens = List(
      Identifier("val"), 
      Identifier("a"), 
      Symbol(":"), 
      Identifier("Count"), 
      Symbol("="), 
      Number(5)
    )
    assert(NewMapParser.statementParse(tokens) ==
      Success(
        FullStatementParse(ValStatement,
          IdentifierParse("a", false),
          IdentifierParse("Count", false),
          NaturalNumberParse(5)
        )
      )
    )
  }

  "A new versioned object " should " be parsed correctly" in {
    val tokens = List(
      Identifier("ver"),
      Identifier("x"),
      Symbol("="),
      Identifier("new"),
      Identifier("Count")
    )
    assert(NewMapParser.statementParse(tokens) ==
      Success(
        NewVersionedStatementParse(
          IdentifierParse("x", false),
          IdentifierParse("Count", false)
        )
      )
    )
  }

  "A List of tokens of map statement " should " be parsed correctly" in {
    val tokens = List(
      Identifier("val"), 
      Identifier("a"), 
      Symbol(":"), 
      Identifier("Map"), 
      Enc(Paren,true), 
      Number(3), 
      Symbol(","), 
      Number(100), 
      Symbol(","), 
      Number(0), 
      Enc(Paren,false), 
      Symbol("="), 
      Enc(Paren,true), 
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
      Enc(Paren,false)
    )
    assert(NewMapParser.statementParse(tokens) ==
      Success(
        FullStatementParse(ValStatement,
          IdentifierParse("a",false),
          ApplyParse(IdentifierParse("Map",false),
            LiteralListParse(Vector(NaturalNumberParse(3), 
              NaturalNumberParse(100), 
              NaturalNumberParse(0)), MapType)
          ),
          LiteralListParse(Vector(KeyValueBinding(NaturalNumberParse(0),
            NaturalNumberParse(20)), 
            KeyValueBinding(NaturalNumberParse(1),
              NaturalNumberParse(43)), 
              KeyValueBinding(NaturalNumberParse(2),NaturalNumberParse(67))),
            MapType
          )
        )
      )
    )
  }

  "A literal map " should " be parsed with one element" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("digit"),
      Symbol(":"),
      Number(10),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      KeyValueBinding(IdentifierParse("digit"), NaturalNumberParse(10))
    ))
  }

  it should " be parsed with multiple elements" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("digit"),
      Symbol(":"),
      Number(10),
      Symbol(","),
      Identifier("T"),
      Symbol(":"),
      Identifier("Type"),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      LiteralListParse(
        Vector(
          KeyValueBinding(IdentifierParse("digit"), NaturalNumberParse(10)),
          KeyValueBinding(IdentifierParse("T"), IdentifierParse("Type"))
        ),
        MapType
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
      Symbol(","),
      Number(5),
      Enc(Paren, false))
    assert(NewMapParser(tokens) == Success(
      LiteralListParse(
        Vector(
          NaturalNumberParse(10), 
          NaturalNumberParse(5)
        ),
        MapType
      )
    ))
  }

  "A lambda expression " should " work in the simplest case" in {
    val tokens = Vector(
      Enc(Paren, true),
      Identifier("d"),
      Symbol(":"),
      Number(5),
      Enc(Paren, false),
      Symbol("=>"),
      Identifier("d")
    )
    assert(NewMapParser(tokens) == Success(
      LambdaParse(
        KeyValueBinding(IdentifierParse("d"), NaturalNumberParse(5)),
        IdentifierParse("d")
      )
    ))
  }

  it should " work correctly for multiple types" in {
    val tokens = Vector(Number(1), Symbol("=>"), Number(2), Symbol("=>"), Number(3))
    assert(NewMapParser(tokens) == Success(
      LambdaParse(NaturalNumberParse(1), LambdaParse(NaturalNumberParse(2), NaturalNumberParse(3)))
    ))
  }

  it should " work correctly when the types are grouped" in {
    val tokens = Vector(Enc(Paren, true), Number(1), Symbol("=>"), Number(2), Enc(Paren, false), Symbol("=>"), Number(3))
    assert(NewMapParser(tokens) == Success(
      LambdaParse(LambdaParse(NaturalNumberParse(1), NaturalNumberParse(2)), NaturalNumberParse(3))
    ))
  }

  "Field access " should " work in an expression" in {
    val tokens = Vector(
      Identifier("Object"),
      Identifier("getId"),
      Symbol(":"),
      Number(5)
    )
    assert(NewMapParser(tokens) == Success(
      KeyValueBinding(
        ApplyParse(IdentifierParse("Object"), IdentifierParse("getId")),
        NaturalNumberParse(5)
      )
    ))
  }

  it should " have the right precedence" in {
    val tokens = Vector(Identifier("a"), Identifier("b"), Identifier("c"))

    assert(NewMapParser(tokens) == Success(
      ApplyParse(
        ApplyParse(IdentifierParse("a"), IdentifierParse("b")),
        IdentifierParse("c")
      )
    ))
  }

  it should " use parens to fix precedence" in {
    val tokens = Vector(Identifier("a"), Enc(Paren, true), Identifier("b"), Identifier("c"), Enc(Paren, false))

    assert(NewMapParser(tokens) == Success(
      ApplyParse(
        IdentifierParse("a"),
        ApplyParse(IdentifierParse("b"), IdentifierParse("c")),
      )
    ))
  }

  "Arrays " should " work in the singleton case" in {
    val tokens = Vector(Enc(SquareBracket, true), Number(10), Enc(SquareBracket, false))

    assert(NewMapParser(tokens) == Success(
      LiteralListParse(
        Vector(NaturalNumberParse(10)),
        ArrayType
      )
    ))
  }
}