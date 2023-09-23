package ai.newmap.interpreter

import ai.newmap.parser.Lexer
import ai.newmap.parser.Lexer.{DQuote, Identifier, Number, Symbol}
import org.scalatest._
import ai.newmap.util.Success
import scala.io.Source


class TestNewmapFiles extends FlatSpec{

  "An identifier " should " be lexed correctly" in {

    val filename = "src/main/newmap/TestScripts/HelloWorld.nm"
    val lines = Source.fromFile(filename).getLines.toList

    assert(Lexer(lines.apply(0)) == Success(List(
      Identifier("val"),
      Identifier("myString"),
      Symbol("="),
      DQuote("\"hello world !!\\n\"")
    )))

    assert(Lexer(lines.apply(1)) == Success(List(
      Identifier("write"),
      Identifier("stdout"),
      Identifier("myString")
    )))
  }

  "A number " should " be lexed correctly" in {
    val filename = "src/main/newmap/TestScripts/TokenizeNumberAndStrings.nm"
    val lines = Source.fromFile(filename).getLines.toList

    assert(Lexer(lines.apply(0)) == Success(List(
      Identifier("val"),
      Identifier("myString"),
      Symbol("="),
      Identifier("hello"),
      Number(67),
      Identifier("world"),
      Number(28)
    )))
  }

  it should "divide up when starting with a 0" in {

    val filename = "src/main/newmap/TestScripts/DivideWhenStartsWithZero.nm"
    val lines = Source.fromFile(filename).getLines.toList

    assert(Lexer(lines.apply(0)) == Success(List(
      Identifier("val"),
      Identifier("myString"),
      Symbol("="),
      Number(0),
      Number(67),
      Identifier("world")
    )))
  }

}
