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

    val correctCommand = Environment.eCommand(
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

    val correctCommand = Environment.eCommand(
      "m",
      TypeT,
      MapType(Index(3), Index(100), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be applyable to a key" in {
    val correctCommand = Environment.eCommand("result", IndexT(100), Index(43))
    testCodeScript(Vector(
      CodeExpectation("val m: Map (key: 3, value: 100, default: 0) = (0: 20, 1: 43, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be applyable to a key not specified and use the default" in {
    val correctCommand = Environment.eCommand("result", IndexT(100), Index(0))

    testCodeScript(Vector(
      CodeExpectation("val m: Map (key: 3, value: 100, default: 0) = (0: 20, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be creatable without explicit parameters" in {
    // This is the first use of automatic param interpolation
    val code = "val m: Type = Map (3, 100, 0)"
    val codeSimplified = "val m: Type = Map 3 100 0"

    val correctCommand = Environment.eCommand(
      "m",
      TypeT,
      MapType(Index(3), Index(100), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
    testCodeLine(CodeExpectation(codeSimplified, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type object pair" in {
    val code = "val m: Map 3 100 0 = (0: 10, 2: 3)"

    val correctCommand = Environment.eCommand(
      "m",
      MapT(IndexT(3), IndexT(100), Index(0)),
      MapInstance(Vector(Index(0) -> Index(10), Index(2) -> Index(3)), Index(0))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A struct " should " be created" in {
    val code = "val s: (Struct (a: 2, b: 3)) = (a:0, b:0)"
    
    val correctCommand = Environment.eCommand(
      "s",
      StructT(Vector(("a",IndexT(2)), ("b",IndexT(3)))),
      StructInstance(Vector(("a",Index(0)), ("b",Index(0)))))

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be callable " in {
    val interp = new EnvironmentInterpreter()
    val correctCommand = Environment.eCommand(
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

    val correctCommand = Environment.eCommand(
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
      CodeExpectation("val Option: (Type => Type) = (t => Case (None: 1, Some: t))", GeneralSuccessCheck),
      CodeExpectation("val maybeSix = Option 6", GeneralSuccessCheck),
      CodeExpectation("val x: maybeSix = (None: 0)", GeneralSuccessCheck),
      CodeExpectation("val y: maybeSix = (Some: 1)", GeneralSuccessCheck),
      CodeExpectation("val z: maybeSix = (None: 3)", FailureCheck),
    ))
  }

  "Lambda expressions" should " be creatable as a type, object and applied" in {
    val correctCommandCreateFunc = Environment.eCommand(
      "f",
      Environment.simpleFuncT(StructT(Vector("a" -> IndexT(3))), IndexT(4)),
      LambdaInstance(
        StructParams(Vector("a" -> Index(3))),
        ApplyFunction(
          MapInstance(Vector(Index(0) -> Index(2), Index(1) -> Index(3), Index(2) -> Index(1)), Index(0)),
          ParameterObj("a")
        )
      )
    )

    val correctCommandUseFunc = Environment.eCommand(
      "result",
      IndexT(4),
      Index(3)
    )

    val correctCommandUseSimpleFunc = Environment.eCommand(
      "resultSimple",
      IndexT(4),
      Index(1)
    )

    val correctCommandUseParenFunc = Environment.eCommand(
      "resultParen",
      IndexT(4),
      Index(2)
    )

    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = ((a: 3) => 4)", SuccessCheck(Environment.eCommand(
        "fSig",
        TypeT,
        ConvertNewMapTypeToObject(Environment.simpleFuncT(
          StructT(Vector("a" -> IndexT(3))),
          IndexT(4)
        ))
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

  // TODO: rename
  it should "be able to work as a map" in {
    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = \\(3: 4)", SuccessCheck(Environment.eCommand(
        "fSig",
        TypeT,
        ConvertNewMapTypeToObject(LambdaT(
          ReqMapInstance(Vector((Index(3), Index(4))))
        ))
      )))
    ))
  }

  it should "be creatable without a type given" in {
    val line = "val f = t => t"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  "Universal identity function " should " have a valid type signature" in {
    val line = "val u: Type = (T: Type, t: T) => T"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should " be creatable" in {
    val line = "val id: ((T: Type, t: T) => T) = (input => input t)"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should " be usable" in {
    testCodeScript(Vector(
      CodeExpectation("val id: ((T: Type, t: T) => T) = (T: Type, t: T) => t", GeneralSuccessCheck),
      CodeExpectation("val IdenId = id Identifier", GeneralSuccessCheck),
      CodeExpectation("IdenId hi", GeneralSuccessCheck),
      CodeExpectation("id 8 3", GeneralSuccessCheck),
    ))
  }

  "Type replacement " should " happen when a function is called" in {
    val correctCommand = Environment.eCommand(
      "ff",
      Environment.simpleFuncT(StructT(Vector("inputTwo" -> IndexT(3))), IndexT(3)),
      LambdaInstance(StructParams(Vector("inputTwo" -> Index(3))), ParameterObj("inputTwo"))
    )

    testCodeScript(Vector(
      CodeExpectation("val f: (inputOne: 5, inputTwo: inputOne) => inputOne = (inputOne: 5, inputTwo: inputOne) => inputTwo", GeneralSuccessCheck),
      CodeExpectation("val ff = f 3", SuccessCheck(correctCommand)),
    ))
  }

  it should " fail when the input makes no sense" in {
    testLineFails("val weird: Type = \\ 3")
  }

  "ReqMaps " should " be usable" in {
    testCodeScript(Vector(
      CodeExpectation("val x = (0: 1, 1: 3, 4: 5)", GeneralSuccessCheck),
      CodeExpectation("x 0", GeneralSuccessCheck),
      CodeExpectation("x 3", FailureCheck)
    ))
  }

  it should " require all the values" in {
    testCodeScript(Vector(
      CodeExpectation("val x: ReqMap 5 10 = (0: 1, 1: 3, 4: 5)", FailureCheck),
      CodeExpectation("val x: ReqMap 5 10 = (0: 1, 1: 3, 2: 3, 3: 9, 4: 5)", GeneralSuccessCheck),
      CodeExpectation("x 0", GeneralSuccessCheck),
      CodeExpectation("x 3", GeneralSuccessCheck)
    ))
  }

  "A Subset Type " should "work" in {
    testCodeScript(Vector(
      CodeExpectation("val x: Subtype 8 = (0: 1, 1: 1, 4: 1)", GeneralSuccessCheck),
      CodeExpectation("val y: x = 1", GeneralSuccessCheck),
      CodeExpectation("val y: x = 6", FailureCheck)
    ))
  }

  it should " have functions made from them" in {
    testCodeScript(Vector(
      CodeExpectation("val Bool: Subtype Identifier = (True: 1, False: 1)", GeneralSuccessCheck),
      CodeExpectation("val f: Bool => 100 = x => (True: 10, False: 20) x", GeneralSuccessCheck),
      CodeExpectation("val x: 100 = f True", GeneralSuccessCheck)
    ))
  }
}