package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.interpreter._
import ai.newmap.interpreter.Lexer._
import ai.newmap.interpreter.NewMapParser._
import ai.newmap.model._
import ai.newmap.util.{Failure, Success}

case class CodeExpectation(
  line: String, // The line of code entered into an environment interpreter
  resultExpectation: ResultExpectation // The expected response from the interpreter  
)

sealed abstract class ResultExpectation

// Tells the tester: expect a failure from this line of code
case object FailureCheck extends ResultExpectation

// Expect a success from this line of code
case object GeneralSuccessCheck extends ResultExpectation

// Expect a success from this line of code, with the environment command given
case class SuccessCheck(com: EnvironmentCommand) extends ResultExpectation



class TestFullEnvironmentInterpreter extends FlatSpec {



  /**
   * test a bunch of lines of newmap code
   * on each line, you can check that it succeeds, fails, or 
   */
  def testCodeScript(expectations: Vector[CodeExpectation]): Unit = {
    val interp = new EnvironmentInterpreter()

    expectations.foreach(expectation => {
      val interpretation = interp(expectation.line)
      expectation.resultExpectation match {
        case FailureCheck => assert(interpretation.isFailure)
        case GeneralSuccessCheck => {
          interpretation match {
            case Success(msg) => ()
            case Failure(msg) => fail(msg)
          }
        }
        case SuccessCheck(com) => {
          interpretation match {
            case Success(msg) => assert(msg == com.toString)
            case Failure(msg) => fail(msg)
          }
        }
      }
    })
  }

  def testCodeLine(expectation: CodeExpectation): Unit = {
    testCodeScript(Vector(expectation))
  }

  def testLineWorks(line: String): Unit = {
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  def testLineFails(line: String): Unit = {
    testCodeLine(CodeExpectation(line, FailureCheck))
  }

  "A number " should " be allowed if it's one less than the type" in {
    testLineWorks("val x: 5 = 4")
  }

  it should " be allowed if it's less than the type" in {
    testLineWorks("val x: 10 = 4")
  }

  it should " fail when equal to the type" in {
    testLineFails("val x: 9 = 9")
  }

  it should " fail when greater than the type" in {
    testLineFails("val x: 2 = 13")
  }

  "A static map " should " be creatable" in {
    val code = "val m: Map (key: 3, value: 100, default: 0) = (0: 20, 1: 43, 2: 67)"

    val correctCommand = FullEnvironmentCommand(
      "m",
      MapT(IndexT(3), IndexT(100), Index(0)),
      MapInstance(Vector(
        Index(0) -> Index(20),
        Index(1) -> Index(43),
        Index(2) -> Index(67)
      ), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type" in {
    val code = "val m: Type = Map (key: 3, value: 100, default: 0)"

    val correctCommand = FullEnvironmentCommand(
      "m",
      TypeT,
      MapType(Index(3), Index(100), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be applyable to a key" in {
    val correctCommand = FullEnvironmentCommand("result", IndexT(100), Index(43))
    testCodeScript(Vector(
      CodeExpectation("val m: Map (key: 3, value: 100, default: 0) = (0: 20, 1: 43, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be applyable to a key not specified and use the default" in {
    val correctCommand = FullEnvironmentCommand("result", IndexT(100), Index(0))

    testCodeScript(Vector(
      CodeExpectation("val m: Map (key: 3, value: 100, default: 0) = (0: 20, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be creatable without explicit parameters" in {
    // This is the first use of automatic param interpolation
    val code = "val m: Type = Map (3, 100, 0)"
    val codeSimplified = "val m: Type = Map 3 100 0"

    val correctCommand = FullEnvironmentCommand(
      "m",
      TypeT,
      MapType(Index(3), Index(100), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
    testCodeLine(CodeExpectation(codeSimplified, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type object pair" in {
    val code = "val m: Map 3 100 0 = (0: 10, 2: 3)"

    val correctCommand = FullEnvironmentCommand(
      "m",
      MapT(IndexT(3), IndexT(100), Index(0)),
      MapInstance(Vector(Index(0) -> Index(10), Index(2) -> Index(3)), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A struct " should " be created" in {
    val code = "val s: (Struct (a: 2, b: 3)) = (a:0, b:0)"
    
    val correctCommand = FullEnvironmentCommand(
      "s",
      StructT(Vector(("a",IndexT(2)), ("b",IndexT(3)))),
      StructInstance(Vector(("a",Index(0)), ("b",Index(0)))))

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be callable " in {
    val interp = new EnvironmentInterpreter()
    val correctCommand = FullEnvironmentCommand(
      "q",
      IndexT(3),
      Index(1)
    )

    testCodeScript(Vector(
      CodeExpectation("val s: Struct (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s b", SuccessCheck(correctCommand))
    ))
  }

  it should " be considered a type in a type input" in {
    val code = "val testType: Type = Map (key: Struct (a: 3, b: 3), value: 100, default: 0)"

    val correctCommand = FullEnvironmentCommand(
      "testType",
      TypeT,
      MapType(
        StructType(
          MapInstance(
            Vector(
              IdentifierInstance("a") -> Index(3),
              IdentifierInstance("b") -> Index(3)
            ),
            Index(1)
          )
        ),
        Index(100),
        Index(0)
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A parameter map for a struct " should " be creatable" in {
    testLineWorks("val m: Map (Identifier, Type, 1) = (a: 6)")
  }

  "A case " should " be created and instantiated" in {
    testCodeScript(Vector(
      CodeExpectation("val Option: (Type => Type) = (t => Case (None: 1, Some: t)))", GeneralSuccessCheck),
      CodeExpectation("val maybeSix = Option 6", GeneralSuccessCheck),
      CodeExpectation("val x: maybeSix = (None: 0)", GeneralSuccessCheck),
      CodeExpectation("val y: maybeSix = (Some: 1)", GeneralSuccessCheck),
      CodeExpectation("val z: maybeSix = (None: 3)", FailureCheck),
    ))
  }

  "Lambda expressions" should " be creatable as a type, object and applied" in {
    val correctCommandCreateFunc = FullEnvironmentCommand(
      "f",
      LambdaT(StructT(Vector("a" -> IndexT(3))), IndexT(4)),
      LambdaInstance(
        StructParams(Vector("a" -> Index(3))),
        ApplyFunction(
          MapInstance(Vector(Index(0) -> Index(2), Index(1) -> Index(3), Index(2) -> Index(1)), Index(0)),
          ParameterObj("a")
        )
      )
    )

    val correctCommandUseFunc = FullEnvironmentCommand(
      "result",
      IndexT(4),
      Index(3)
    )

    val correctCommandUseSimpleFunc = FullEnvironmentCommand(
      "resultSimple",
      IndexT(4),
      Index(1)
    )

    val correctCommandUseParenFunc = FullEnvironmentCommand(
      "resultParen",
      IndexT(4),
      Index(2)
    )

    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = ((a: 3) => 4)", SuccessCheck(FullEnvironmentCommand(
        "fSig",
        TypeT,
        LambdaType(
          MapInstance(Vector(IdentifierInstance("a") -> Index(3)), Index(1)),
          Index(4)
        )
      ))),
      CodeExpectation("val m: Map(3, 4, 0) = (0: 2, 1: 3, 2: 1)", GeneralSuccessCheck),
      CodeExpectation("val f: fSig = ((a: 3) => m a)", SuccessCheck(correctCommandCreateFunc)),
      CodeExpectation("val result: 4 = f (a: 1)", SuccessCheck(correctCommandUseFunc)),
      CodeExpectation("val resultSimple: 4 = f 2", SuccessCheck(correctCommandUseSimpleFunc)),
      CodeExpectation("val resultParen: 4 = f(0)", SuccessCheck(correctCommandUseParenFunc))
    ))
  }

  it should "be able to take arbitrary types as inputs" in {
    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = (3 => 4)", GeneralSuccessCheck),
      CodeExpectation("val m: Map(3, 4, 0) = (0: 2, 1: 3, 2: 1)", GeneralSuccessCheck),
      CodeExpectation("val f: fSig = (a => m a)", GeneralSuccessCheck),
      CodeExpectation("val resultSimple: 4 = f 2", GeneralSuccessCheck),
    ))
  }
}