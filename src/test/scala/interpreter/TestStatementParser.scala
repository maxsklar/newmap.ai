package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.interpreter._
import ai.newmap.interpreter.Lexer._
import ai.newmap.interpreter.NewMapParser._
import ai.newmap.model._
import ai.newmap.util.Success

class TestStatementParser extends FlatSpec {
  "A statement " should " be parsed correctly" in {
    val tokens = Vector(Identifier("val"), Identifier("x"), Symbol(":"), Number(100), Symbol("="), Number(10))
    assert(NewMapParser.statementParse(tokens) == Success(
      FullStatementParse(
        ValStatement,
        IdentifierParse("x"),
        NaturalNumberParse(100),
        NaturalNumberParse(10)
      )
    ))
  }
}