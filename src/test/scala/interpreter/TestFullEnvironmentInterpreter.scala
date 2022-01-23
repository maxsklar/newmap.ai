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
    val code = "val m: Map (key: 3, value: 100) = (0: 20, 1: 43, 2: 67)"

    val correctCommand = Environment.eCommand(
      "m",
      MapT(Ord(3), Ord(100), CommandOutput, BasicMap),
      MapInstance(Vector(
        Ord(0) -> Ord(20),
        Ord(1) -> Ord(43),
        Ord(2) -> Ord(67)
      ))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type" in {
    val code = "val m: Type = Map (key: 3, value: 100)"

    val correctCommand = Environment.eCommand(
      "m",
      TypeT(0),
      MapT(Ord(3), Ord(100), CommandOutput, BasicMap)
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be applyable to a key" in {
    val correctCommand = Environment.eCommand("result", Ord(100), Ord(43))
    testCodeScript(Vector(
      CodeExpectation("val m: Map (key: 3, value: 100) = (0: 20, 1: 43, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be applyable to a key not specified and use the default" in {
    val correctCommand = Environment.eCommand("result", Ord(100), Ord(0))

    testCodeScript(Vector(
      CodeExpectation("val m: Map (key: 3, value: 100) = (0: 20, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be creatable without explicit parameters" in {
    // This is the first use of automatic param interpolation
    val code = "val m: Type = Map (3, 100)"
    
    val correctCommand = Environment.eCommand(
      "m",
      TypeT(0),
      MapT(Ord(3), Ord(100), CommandOutput, BasicMap)
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type object pair" in {
    val code = "val m: Map(3, 100) = (0: 10, 2: 3)"

    val correctCommand = Environment.eCommand(
      "m",
      MapT(Ord(3), Ord(100), CommandOutput, BasicMap),
      MapInstance(Vector(Ord(0) -> Ord(10), Ord(2) -> Ord(3)))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A struct " should " be created" in {
    val correctCommand = Environment.eCommand(
      "s",
      Environment.structTypeFromParams(Vector(("a",Ord(2)), ("b",Ord(3)))),
      StructInstance(Vector(("a",Ord(0)), ("b",Ord(0)))))

    testCodeScript(Vector(
      CodeExpectation("val Fields: Type = Subtype(Identifier, 2, (a: 1, b: 1))", GeneralSuccessCheck),
      CodeExpectation("val s: Struct(Fields, (a: 2, b: 3)) = (a:0, b:0)", SuccessCheck(correctCommand))
    ))
  }

  it should " be callable " in {
    val interp = new EnvironmentInterpreter()
    val correctCommand = Environment.eCommand(
      "q",
      Ord(3),
      Ord(1)
    )

    testCodeScript(Vector(
      CodeExpectation("val Fields: Type = Subtype(Identifier, 2, (a: 1, b: 1))", GeneralSuccessCheck),
      CodeExpectation("val s: Struct(Fields, (a: 2, b: 3)) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s b", SuccessCheck(correctCommand))
    ))
  }

  it should " be considered a type in a type input" in {
    val correctCommand = Environment.eCommand(
      "testType",
      TypeT(0),
      MapT(
        Environment.structTypeFromParams(
          Vector(
            "a" -> Ord(3),
            "b" -> Ord(3)
          )
        ),
        Ord(100),
        CommandOutput,
        BasicMap
      )
    )

    testCodeScript(Vector(
      CodeExpectation("val Fields: Type = Subtype(Identifier, 2, (a: 1, b: 1))", GeneralSuccessCheck),
      CodeExpectation("val s: Struct(Fields, (a: 2, b: 3)) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val testType: Type = Map (key: Struct(Fields, (a: 3, b: 3)), value: 100)", SuccessCheck(correctCommand))
    ))
  }

  "A Map to a non-command type " should " not be creatable" in {
    testLineFails("val m: Map (Identifier, Type) = (a: 6)")
  }

  "A case " should " be created and instantiated"  in {
    val correctCommand = Environment.eCommand(
      "x",
      Environment.caseTypeFromParams(Vector(("a",Ord(2)), ("b",Ord(3)))),
      CaseInstance(IdentifierInstance("a"), Ord(0))
    )

    testCodeScript(Vector(
      CodeExpectation("val Fields: Type = Subtype(Identifier, 2, (a: 1, b: 1))", GeneralSuccessCheck),
      CodeExpectation("val MyCase: Type = Case(Fields, (a: 2, b: 3))", GeneralSuccessCheck),
      CodeExpectation("val x: MyCase = (a:0)", SuccessCheck(correctCommand)),
      CodeExpectation("val y: MyCase = (a:0, b:1)", FailureCheck),
      CodeExpectation("val x: MyCase = (c:0)", FailureCheck),
    ))
  }

  // TODO: Test an option case
  /*"A case " should " be created and instantiated" in {
    testCodeScript(Vector(
      CodeExpectation("val Option: (Type => Type) = (t => Case (None: 1, Some: t))", GeneralSuccessCheck),
      CodeExpectation("val maybeSix = Option 6", GeneralSuccessCheck),
      CodeExpectation("val x: maybeSix = (None: 0)", GeneralSuccessCheck),
      CodeExpectation("val y: maybeSix = (Some: 1)", GeneralSuccessCheck),
      CodeExpectation("val z: maybeSix = (None: 3)", FailureCheck),
    ))
  }*/

  "Lambda expressions" should " be creatable as a type, object and applied" in {
    val correctCommandCreateFunc = Environment.eCommand(
      "f",
      Environment.simpleFuncT(Environment.structTypeFromParams(Vector("a" -> Ord(3))), Ord(4)),
      LambdaInstance(
        StructParams(Vector("a" -> Ord(3))),
        ApplyFunction(
          MapInstance(Vector(Ord(0) -> Ord(2), Ord(1) -> Ord(3), Ord(2) -> Ord(1))),
          ParameterObj("a")
        )
      )
    )

    val correctCommandUseFunc = Environment.eCommand(
      "result",
      Ord(4),
      Ord(3)
    )

    val correctCommandUseSimpleFunc = Environment.eCommand(
      "resultSimple",
      Ord(4),
      Ord(1)
    )

    val correctCommandUseParenFunc = Environment.eCommand(
      "resultParen",
      Ord(4),
      Ord(2)
    )

    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = ((a: 3) => 4)", SuccessCheck(Environment.eCommand(
        "fSig",
        TypeT(0),
        Environment.simpleFuncT(
          Environment.structTypeFromParams(Vector("a" -> Ord(3))),
          Ord(4)
        )
      ))),
      CodeExpectation("val m: Map(3, 4) = (0: 2, 1: 3, 2: 1)", GeneralSuccessCheck),
      CodeExpectation("val f: fSig = ((a: 3) => m a)", SuccessCheck(correctCommandCreateFunc)),
      CodeExpectation("val result: 4 = f (a: 1)", SuccessCheck(correctCommandUseFunc)),
      CodeExpectation("val resultSimple: 4 = f 2", SuccessCheck(correctCommandUseSimpleFunc)),
      CodeExpectation("val resultParen: 4 = f(0)", SuccessCheck(correctCommandUseParenFunc))
    ))
  }

  it should "be able to take arbitrary types as inputs" in {
    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = (3 => 4)", GeneralSuccessCheck),
      CodeExpectation("val m: Map(3, 4) = (0: 2, 1: 3, 2: 1)", GeneralSuccessCheck),
      CodeExpectation("val f: fSig = (a => m a)", GeneralSuccessCheck),
      CodeExpectation("val resultSimple: 4 = f 2", GeneralSuccessCheck),
    ))
  }

  it should "be creatable without a type given" in {
    val line = "val f = t => t"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should "be able to infer the function type" in {
    testCodeScript(Vector(
      CodeExpectation("val x: 12 = 6", GeneralSuccessCheck),
      CodeExpectation("(6: 10, 1: 3, 2: 1) x", GeneralSuccessCheck)
    ))
  }

//
  // Another test:
  // val x: S = ....
  // val y: T = (asdf) x
  // We should be able to infer that whatever is in "asdf" is of type S -> T


  // REDO these tests when we have type inference
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
      CodeExpectation("id(8, 3)", GeneralSuccessCheck),
    ))
  }

  "ReqMaps " should " require an input type" in {
    testCodeScript(Vector(
      CodeExpectation("val x = (0: 1, 1: 3, 4: 5)", FailureCheck)
    ))
  }

  it should " require all the values" in {
    testCodeScript(Vector(
      CodeExpectation("val x: ReqMap(5, 10) = (0: 1, 1: 3, 4: 5)", FailureCheck),
      CodeExpectation("val x: ReqMap(5, 10) = (0: 1, 1: 3, 2: 3, 3: 9, 4: 5)", GeneralSuccessCheck),
      CodeExpectation("x 0", GeneralSuccessCheck),
      CodeExpectation("x 3", GeneralSuccessCheck)
    ))
  }

  it should " be able to handle the most basic pattern matching" in {
    testCodeScript(Vector(
      CodeExpectation("val x: ReqMap(4, 4) = (0: 3, t: t)", GeneralSuccessCheck),
      CodeExpectation("x 0", SuccessCheck(ExpOnlyEnvironmentCommand(Ord(3)))),
      CodeExpectation("x 3", SuccessCheck(ExpOnlyEnvironmentCommand(Ord(3)))),
      CodeExpectation("x 2", SuccessCheck(ExpOnlyEnvironmentCommand(Ord(2))))
    ))
  }

  "A Subset Type " should "work" in {
    testCodeScript(Vector(
      CodeExpectation("val underlyingMap: Map(8, 2) = (0: 1, 1: 1, 4: 1)", GeneralSuccessCheck),
      CodeExpectation("val x: Type = Subtype(8, 2, underlyingMap)", GeneralSuccessCheck),
      CodeExpectation("val y: x = 1", GeneralSuccessCheck),
      CodeExpectation("val y: x = 6", FailureCheck)
    ))
  }

  it should " have functions made from them" in {
    testCodeScript(Vector(
      CodeExpectation("val Bool: Type = Subtype(Identifier, 2, (True: 1, False: 1))", GeneralSuccessCheck),
      CodeExpectation("val shorten: Map(Bool, 2) = (True: 1, False: 0)", GeneralSuccessCheck),
      CodeExpectation("val x: 2 = shorten True", GeneralSuccessCheck)
    ))
  }
}