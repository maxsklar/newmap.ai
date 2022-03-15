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
  def rangeType(i: Long): SubtypeT = SubtypeT(RangeFunc(i))

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
    val code = "val m: Map (3, 100) = (0: 20, 1: 43, 2: 67)"

    val correctCommand = Environment.eCommand(
      "m",
      MapInstance(
        Vector(
          ObjectPattern(Index(0)) -> Index(20),
          ObjectPattern(Index(1)) -> Index(43),
          ObjectPattern(Index(2)) -> Index(67)
        ),
        MapT(rangeType(3), rangeType(100), CommandOutput, BasicMap)
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type" in {
    val code = "val m: Type = Map (3, 100)"

    val correctCommand = Environment.eCommand(
      "m",
      MapT(rangeType(3), rangeType(100), CommandOutput, BasicMap)
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be applyable to a key" in {
    val correctCommand = Environment.eCommand("result", Index(43))
    testCodeScript(Vector(
      CodeExpectation("val m: Map (3, 100) = (0: 20, 1: 43, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be applyable to a key not specified and use the default" in {
    val correctCommand = Environment.eCommand("result", Index(0))

    testCodeScript(Vector(
      CodeExpectation("val m: Map (3, 100) = (0: 20, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }


  it should " be creatable as a type object pair" in {
    val code = "val m: Map(3, 100) = (0: 10, 2: 3)"

    val correctCommand = Environment.eCommand(
      "m",
      MapInstance(
        Vector(ObjectPattern(Index(0)) -> Index(10), ObjectPattern(Index(2)) -> Index(3)),
        MapT(rangeType(3), rangeType(100), CommandOutput, BasicMap)
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A struct " should " be created" in {
    val structType = Environment.structTypeFromParams(Vector(("a",rangeType(2)), ("b",rangeType(3))))
    val correctCommand = Environment.eCommand(
      "s",
      StructInstance(
        Vector((ObjectPattern(IdentifierInstance("a")), Index(0)), (ObjectPattern(IdentifierInstance("b")), Index(0))),
        structType
      )
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:0)", SuccessCheck(correctCommand))
    ))
  }

  it should " allow its fields to be accessed " in {
    val interp = new EnvironmentInterpreter()
    val correctCommand = Environment.eCommand(
      "q",
      Index(1)
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s.b", SuccessCheck(correctCommand))
    ))
  }

  it should " allow its fields to be accessed in expresions that can be evaluated to literals " in {
    val interp = new EnvironmentInterpreter()
    val correctCommand = Environment.eCommand(
      "q",
      Index(1)
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val fieldMap: ReqMap(2, Identifier) = (0: z, 1: b)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s.(fieldMap(1))", GeneralSuccessCheck)
    ))
  }

  it should " not allow fields that cannnot be evaluated to literals " in {
    val interp = new EnvironmentInterpreter()
    val correctCommand = Environment.eCommand(
      "q",
      Index(1)
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: Field => 3 = (x: s.x)", FailureCheck)
    ))
  }

  it should " be considered a type in a type input" in {
    val correctCommand = Environment.eCommand(
      "testType",
      MapT(
        Environment.structTypeFromParams(
          Vector(
            "a" -> rangeType(3),
            "b" -> rangeType(3)
          )
        ),
        rangeType(100),
        CommandOutput,
        BasicMap
      )
    )

    testCodeScript(Vector(
      CodeExpectation("val testType: Type = Map (Struct(a: 3, b: 3), 100)", SuccessCheck(correctCommand))
    ))
  }

  it should " be creatable with numerical indecies" in {
    testCodeScript(Vector(
      CodeExpectation("val s: CStruct(0: Type, 1: 10) = (Count, 6)", GeneralSuccessCheck)
    ))
  }

  "A Map to a non-command type " should " not be creatable" in {
    testLineFails("val m: Map (Identifier, Type) = (a: 6)")
  }

  "A case " should " be created and instantiated" in {
    val caseT = Environment.caseTypeFromParams(Vector(("a",rangeType(2)), ("b",rangeType(3))))

    val correctCommand = Environment.eCommand(
      "x",
      CaseInstance(IdentifierInstance("a"), Index(0), caseT)
    )

    testCodeScript(Vector(
      CodeExpectation("val MyCase: Type = Case (a: 2, b: 3)", GeneralSuccessCheck),
      CodeExpectation("val x: MyCase = MyCase.a 0", SuccessCheck(correctCommand)),
      CodeExpectation("val y: MyCase = MyCase.a.b", FailureCheck),
      CodeExpectation("val x: MyCase = MyCase.c", FailureCheck),
    ))
  }

  "A generic option case " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("val Option: ReqMap(Type, Type) = (t: Case (None: 1, Some: t))", GeneralSuccessCheck),
      CodeExpectation("val maybeSix = Option 6", GeneralSuccessCheck),
      CodeExpectation("val x: maybeSix = maybeSix.None 0", GeneralSuccessCheck),
      CodeExpectation("val y: maybeSix = maybeSix.Some 1", GeneralSuccessCheck),
      CodeExpectation("val z: maybeSix = maybeSix.None 3", FailureCheck),
    ))
  }

  // TODO: Return to this when there is better tooling!
  /*"A generic option case " should " be callable without naming it" in {
    testCodeScript(Vector(
      CodeExpectation("val Option: ReqMap(Type, Type) = (t: Case (None: 1, Some: t))", GeneralSuccessCheck),
      CodeExpectation("val x: Option Count = (Option Count).None 0", GeneralSuccessCheck),
      CodeExpectation("val y: Option Count = (Option Count).Some 20", GeneralSuccessCheck),
      CodeExpectation("val z: Option Count = (Option Count).None 3", FailureCheck),
    ))
  }*/

  "Lambda expressions" should " be creatable as a type, object and applied" in {
    val correctCommandCreateFunc = Environment.eCommand(
      "f",
      MapInstance(Vector(
        TypePattern("a", rangeType(3)) ->
        ApplyFunction(
          MapInstance(
            Vector(ObjectPattern(Index(0)) -> Index(2), ObjectPattern(Index(1)) -> Index(3), ObjectPattern(Index(2)) -> Index(1)),
            MapT(rangeType(3), rangeType(4), CommandOutput, BasicMap)
          ),
          ParamId("a")
        )
      ), MapT(rangeType(3), rangeType(4), RequireCompleteness, FullFunction))
    )

    val correctCommandUseFunc = Environment.eCommand(
      "result",
      Index(3)
    )

    val correctCommandUseSimpleFunc = Environment.eCommand(
      "resultSimple",
      Index(1)
    )

    val correctCommandUseParenFunc = Environment.eCommand(
      "resultParen",
      Index(2)
    )

    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = (3 => 4)", SuccessCheck(Environment.eCommand(
        "fSig",
        Environment.fullFuncT(rangeType(3), rangeType(4))
      ))),
      CodeExpectation("val m: Map(3, 4) = (0: 2, 1: 3, 2: 1)", GeneralSuccessCheck),
      CodeExpectation("val f: fSig = (a: m a)", SuccessCheck(correctCommandCreateFunc)),
      CodeExpectation("val result: 4 = f 1", SuccessCheck(correctCommandUseFunc)),
      CodeExpectation("val resultSimple: 4 = f 2", SuccessCheck(correctCommandUseSimpleFunc)),
      CodeExpectation("val resultParen: 4 = f(0)", SuccessCheck(correctCommandUseParenFunc))
    ))
  }

  it should " work with a struct input" in {
    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = (a: 5, b: (5 => 10)) => 10", GeneralSuccessCheck),
      CodeExpectation("val f: fSig = ((a, b): b a)", GeneralSuccessCheck),
      CodeExpectation("val m: Map(5, 10) = (0: 0, 1: 2, 2: 4, 3: 6, 4: 8)", GeneralSuccessCheck),
      CodeExpectation("f (1, m)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(2))))
    ))
  }

  it should " be creatable without a type given" in {
    val line = "val f = (t: t)"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should " be able to infer the function type" in {
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
    val line = "val u: Type = Any => Any"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should " be creatable" in {
    val line = "val id: Any => Any = (x: x)"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should " be usable" in {
    testCodeScript(Vector(
      CodeExpectation("val id: (Any => Any) = (t: t)", GeneralSuccessCheck),
      CodeExpectation("id ~hi", SuccessCheck(ExpOnlyEnvironmentCommand(IdentifierInstance("hi")))),
      CodeExpectation("id 5", SuccessCheck(ExpOnlyEnvironmentCommand(Index(5)))),
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
      CodeExpectation("x 0", SuccessCheck(ExpOnlyEnvironmentCommand(Index(3)))),
      CodeExpectation("x 3", SuccessCheck(ExpOnlyEnvironmentCommand(Index(3)))),
      CodeExpectation("x 2", SuccessCheck(ExpOnlyEnvironmentCommand(Index(2))))
    ))
  }

  // This test exists because of a bug which got this wrong
  // The solution to this was avoiding variable capture
  it should " be able to handle a nested ReqMap properly" in {
    val expectedResult = MapT(
      rangeType(2),
      MapT(
        rangeType(4),
        CountT,
        RequireCompleteness,
        SimpleFunction
      ),
      RequireCompleteness,
      SimpleFunction
    )

    testCodeScript(Vector(
      CodeExpectation(
        "ReqMap(2, ReqMap(4, Count))",
        SuccessCheck(ExpOnlyEnvironmentCommand(expectedResult))
      )
    ))
  }

  "SubMaps " should " work" in testCodeScript(Vector(
    CodeExpectation("val m: SubMap(8, 2) = (0: 1, 1: 1, 4: 1)", GeneralSuccessCheck),
    CodeExpectation("m 0", GeneralSuccessCheck),
    CodeExpectation("m 0", GeneralSuccessCheck),
    CodeExpectation("m 4", GeneralSuccessCheck),
    CodeExpectation("m 2", FailureCheck)
  ))

  "A struct pattern " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("val m1: ReqMap(4, Count) = (0: 100, 1: 101, 2: 102, 3: 103)", GeneralSuccessCheck),
      CodeExpectation("val m2: ReqMap(4, Count) = (0: 200, 1: 201, 2: 202, 3: 203)", GeneralSuccessCheck),
      CodeExpectation("val ifStatement: ReqMap(2, ReqMap(4, Count)) = (0: m1, 1: m2)", GeneralSuccessCheck),

      // TODO - clean up by building a shortcut
      CodeExpectation("val f: CStruct(0: 4, 1: 2) => Count = ((first, second): ifStatement second first)", GeneralSuccessCheck),
      CodeExpectation("f(0, 0)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(100)))),
      CodeExpectation("f(1, 1)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(201)))),
      CodeExpectation("f(2, 0)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(102))))
    ))

    //583
  }

  "A Subset Type " should "work" in {
    testCodeScript(Vector(
      CodeExpectation("val underlyingMap: Map(8, 2) = (0: 1, 1: 1, 4: 1)", GeneralSuccessCheck),
      CodeExpectation("val x: Type = Subtype(underlyingMap)", GeneralSuccessCheck),
      CodeExpectation("val y: x = 1", GeneralSuccessCheck),
      CodeExpectation("val y: x = 6", FailureCheck)
    ))
  }

  it should " have functions made from them" in {
    testCodeScript(Vector(
      // TODO: should be able to call Subtype(True: 1, False: 1) directly - but that requires generics
      CodeExpectation("val BoolMap: Map(Identifier, 2) = (True: 1, False: 1)", GeneralSuccessCheck),      
      CodeExpectation("val Bool: Type = Subtype(BoolMap)", GeneralSuccessCheck),
      CodeExpectation("val shorten: Map(Bool, 2) = (True: 1, False: 0)", GeneralSuccessCheck),
      CodeExpectation("val x: 2 = shorten True", GeneralSuccessCheck)
    ))
  }

  "The increment function " should " work on index numbers" in {
    testCodeScript(Vector(
      CodeExpectation(
        "val x = Increment 5",
        SuccessCheck(Environment.eCommand("x", Index(6)))
      )
    ))
  }

  it should " fail on other things" in {
    CodeExpectation("Increment Type", FailureCheck)
    CodeExpectation("Increment (1: 4, 2: 5)", FailureCheck)
  }
}