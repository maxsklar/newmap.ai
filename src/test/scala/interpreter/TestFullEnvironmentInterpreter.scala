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
  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)
  def IndexValue(i: Long, nObject: NewMapObject): NewMapObject = TaggedObject(UIndex(i), nObject)

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

  def bind(key: UntaggedObject, value: NewMapObject): (NewMapPattern, NewMapExpression) = {
    ObjectPattern(key) -> ObjectExpression(value)
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
      TaggedObject(
        UMap(Vector(
          bind(UIndex(0), IndexValue(20, Index(100))),
          bind(UIndex(1), IndexValue(43, Index(100))),
          bind(UIndex(2), IndexValue(67, Index(100)))
        )),
        MapT(Index(3), Index(100), CommandOutput, BasicMap)
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type" in {
    val code = "val m: Type = Map (3, 100)"

    val correctCommand = Environment.eCommand(
      "m",
      MapT(Index(3), Index(100), CommandOutput, BasicMap)
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be applyable to a key" in {
    val correctCommand = Environment.eCommand("result", IndexValue(43, Index(100)))
    testCodeScript(Vector(
      CodeExpectation("val m: Map (3, 100) = (0: 20, 1: 43, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be applyable to a key not specified and use the default" in {
    val correctCommand = Environment.eCommand("result", IndexValue(0, Index(100)))

    testCodeScript(Vector(
      CodeExpectation("val m: Map (3, 100) = (0: 20, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }


  it should " be creatable as a type object pair" in {
    val code = "val m: Map(3, 100) = (0: 10, 2: 3)"

    val correctCommand = Environment.eCommand(
      "m",
      TaggedObject(
        UMap(Vector(
          bind(UIndex(0), IndexValue(10, Index(100))),
          bind(UIndex(2),  IndexValue(3, Index(100)))
        )),
        MapT(Index(3), Index(100), CommandOutput, BasicMap)
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A struct " should " be created" in {
    val structType = Environment.structTypeFromParams(Vector(("a",Index(2)), ("b",Index(3))))
    val correctCommand = Environment.eCommand(
      "s",
      TaggedObject(
        UMap(Vector(
          bind(UIdentifier("a"), IndexValue(0, Index(2))), 
          bind(UIdentifier("b"), IndexValue(0, Index(3)))
        )),
        structType
      )
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:0)", SuccessCheck(correctCommand))
    ))
  }

  it should " allow its fields to be accessed " in {
    val correctCommand = Environment.eCommand(
      "q",
      IndexValue(1, Index(3))
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s.b", SuccessCheck(correctCommand))
    ))
  }

  it should " allow its fields to be accessed in expresions that can be evaluated to literals " in {
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
            "a" -> Index(3),
            "b" -> Index(3)
          )
        ),
        Index(100),
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
    testLineFails("val m: Map (10, Identifier) = (6: a)")
  }

  "A case " should " be created and instantiated" in {
    val caseT = Environment.caseTypeFromParams(Vector(("a",Index(2)), ("b",Index(3))))

    val correctCommand = Environment.eCommand(
      "x",
      TaggedObject(UCase(UIdentifier("a"), UIndex(0)), caseT)
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
      TaggedObject(
        UMap(Vector(
          WildcardPattern("a") ->
          ApplyFunction(
            ObjectExpression(TaggedObject(
              UMap(Vector(
                bind(UIndex(0), IndexValue(2, Index(4))),
                bind(UIndex(1), IndexValue(3, Index(4))),
                bind(UIndex(2), IndexValue(1, Index(4)))
              )),
              MapT(Index(3), Index(4), CommandOutput, BasicMap)
            )),
            ParamId("a")
          )
        )),
        MapT(Index(3), Index(4), RequireCompleteness, FullFunction
      ))
    )

    val correctCommandUseFunc = Environment.eCommand(
      "result",
      IndexValue(3, Index(4))
    )

    val correctCommandUseSimpleFunc = Environment.eCommand(
      "resultSimple",
      IndexValue(1, Index(4))
    )

    val correctCommandUseParenFunc = Environment.eCommand(
      "resultParen",
      IndexValue(2, Index(4))
    )

    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = (3 => 4)", SuccessCheck(Environment.eCommand(
        "fSig",
        Environment.fullFuncT(Index(3), Index(4))
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
      CodeExpectation("f (1, m)", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, Index(10)))))
    ))
  }

  /*it should " be creatable without a type given" in {
    val line = "val f = (t: t)"
    testCodeLine(CodeExpectation(line, GeneralSuccessCheck))
  }

  it should " be able to infer the function type" in {
    testCodeScript(Vector(
      CodeExpectation("val x: 12 = 6", GeneralSuccessCheck),
      CodeExpectation("(6: 10, 1: 3, 2: 1) x", GeneralSuccessCheck)
    ))
  }*/

  it should " be usable in a function" in {
    testCodeScript(Vector(
      CodeExpectation("val x: Type => Type = (t: t)", GeneralSuccessCheck),
      CodeExpectation("val y: Type => Type = (t: (t => 2))", GeneralSuccessCheck),
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
      CodeExpectation("id ~hi", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("hi")))),
      CodeExpectation("id 5", SuccessCheck(ExpOnlyEnvironmentCommand(Index(5)))),
    ))
  }

  "Variable Substitution " should " work" in {
    testCodeScript(Vector(
      CodeExpectation("val id5: ReqMap(5, 5) = (t: t)", GeneralSuccessCheck),
      CodeExpectation("val gimmeWhatIWant: ReqMap(5, ReqMap(5, 5)) = (t: (s: t))", GeneralSuccessCheck),
      CodeExpectation("gimmeWhatIWant 2 4", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, Index(5)))))
    ))
  }

  it should " respect inner-scoped variables" in {
    testCodeScript(Vector(
      CodeExpectation("val id5: ReqMap(5, 5) = (t: t)", GeneralSuccessCheck),
      CodeExpectation("val gimmeId5: ReqMap(Count, ReqMap(5, 5)) = (t: id5)", GeneralSuccessCheck),
      CodeExpectation("gimmeId5 10 1", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(1, Index(5)))))
    ))
  }

  it should " avoid variable capture" in {
    testCodeScript(Vector(
      // This works because the map doesn't attempt to evaluate when it is defined
      // But it might break when it does!
      CodeExpectation("val f: ReqMap(5, ReqMap(5, 5)) = (x: (z: x))", GeneralSuccessCheck),      
      CodeExpectation("val g: ReqMap(5, ReqMap(5, 5)) = (z: (f z))", GeneralSuccessCheck),
      CodeExpectation("g 1 3", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(1, Index(5)))))
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
      CodeExpectation("x 0", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(3, Index(4))))),
      CodeExpectation("x 3", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(3, Index(4))))),
      CodeExpectation("x 2", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, Index(4)))))
    ))
  }

  // This test exists because of a bug which got this wrong
  // The solution to this was avoiding variable capture
  it should " be able to handle a nested ReqMap properly" in {
    val expectedResult = MapT(
      Index(2),
      MapT(
        Index(4),
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
    CodeExpectation("m 1", GeneralSuccessCheck),
    CodeExpectation("m 4", GeneralSuccessCheck),
    CodeExpectation("m 2", FailureCheck)
  ))

  "A SimpleMap " should " be allowed to call other simple maps" in {
    testCodeScript(Vector(
      CodeExpectation("val m1: ReqMap(3, Identifier) = (0: Zero, 1: One, 2: Two)", GeneralSuccessCheck),
      CodeExpectation("val m2: ReqMap(3, Identifier) = (0: Nil, x: m1 x)", GeneralSuccessCheck),
      CodeExpectation("m2 0", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("Nil")))),
      CodeExpectation("m2 2", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("Two"))))
    ))
  }

  it should " not be allowed to call a full function map" in {
    testCodeScript(Vector(
      CodeExpectation("val m: ReqMap(Any, 2) = ((y: (Any => 2)): y 5, _: 0)", FailureCheck)
    ))
  }

  // TODO - the same thing must be caught for a map that's being updated with commands
  it should " disallow self referential function calls" in {
    testCodeScript(Vector(
      CodeExpectation("val m: ReqMap(Any, 2) = (_: 1)", GeneralSuccessCheck),
      CodeExpectation("val m: ReqMap(Any, 2) = (x: 1, _: 0)", GeneralSuccessCheck),
      CodeExpectation("val m: ReqMap(Any, 2) = (x: 1, _: 0)", GeneralSuccessCheck),
      CodeExpectation("val m: ReqMap(Any, Any) = (x: x, _: 0)", GeneralSuccessCheck),
      // Preventing Russell's paradox!
      CodeExpectation("val m: ReqMap(ReqMap(Any, Any), 2) = (x: x x)", FailureCheck),
      // This line below will create an infinite loop if evaluated!! Out simple function check will catch it
      CodeExpectation("val m: ReqMap(Any, Any) => 2 = (x: x x)", GeneralSuccessCheck)
    ))
  }

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

  "Versioned Counts " should " be initialized and updated for Count" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("n", SuccessCheck(ExpOnlyEnvironmentCommand(Index(3))))
    ))
  }

  it should " be allowed to have members" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("val p: n = 1", GeneralSuccessCheck),
    ))
  }

  "Versioned Objects " should " be initialized and updated for Map" in {
    testCodeScript(Vector(
      CodeExpectation("ver m = new Map(Identifier, Count)", GeneralSuccessCheck),
      CodeExpectation("update m (hello, ())", GeneralSuccessCheck),
      CodeExpectation("update m (world, ())", GeneralSuccessCheck),
      CodeExpectation("update m (world, ())", GeneralSuccessCheck),
      CodeExpectation("m hello", SuccessCheck(ExpOnlyEnvironmentCommand(Index(1)))),
      CodeExpectation("m world", SuccessCheck(ExpOnlyEnvironmentCommand(Index(2)))),
      CodeExpectation("m yo", SuccessCheck(ExpOnlyEnvironmentCommand(Index(0))))
    ))
  }

  it should " be convertable to values and stop updating" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("val x: Count = n", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("x", SuccessCheck(ExpOnlyEnvironmentCommand(Index(1)))),
      CodeExpectation("n", SuccessCheck(ExpOnlyEnvironmentCommand(Index(3))))
    ))
  }

  it should " be forked correctly" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("ver nForked = fork n", GeneralSuccessCheck),
      CodeExpectation("update nForked ()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update nForked ()", GeneralSuccessCheck),
      CodeExpectation("update nForked ()", GeneralSuccessCheck),
      CodeExpectation("update nForked ()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update nForked ()", GeneralSuccessCheck),
      CodeExpectation("n", SuccessCheck(ExpOnlyEnvironmentCommand(Index(5)))),
      CodeExpectation("nForked", SuccessCheck(ExpOnlyEnvironmentCommand(Index(8))))
    ))
  }

  it should " fail when forked off a non-versioned object" in {
    testCodeScript(Vector(
      CodeExpectation("val x: Count = 10", GeneralSuccessCheck),
      CodeExpectation("ver n = fork x", FailureCheck)
    ))
  }

  "Defining Bool as a Case " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("ver Bool = new Type", GeneralSuccessCheck),
      CodeExpectation("update Bool (False, ())", GeneralSuccessCheck),
      CodeExpectation("update Bool (True, ())", GeneralSuccessCheck),
      CodeExpectation("Bool.True ()", GeneralSuccessCheck),
      CodeExpectation("Bool.False ()", GeneralSuccessCheck)
    ))
  }

  "A case pattern " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("val MyCase = Case(First: 5, Second: Identifier)", GeneralSuccessCheck),
      // Note that x must be declares as an identifier here because otherwise it's taken as the literal identifier x
      // TODO - we need to make sure there are some GOOD ERROR MESSAGES associated with that
      CodeExpectation("val MyCaseTo5: ReqMap(MyCase, 5) = ((First x): x, (Second x): 2)", GeneralSuccessCheck),
      CodeExpectation("MyCaseTo5 (MyCase.First 4)", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(4, Index(5))))),
      CodeExpectation("MyCaseTo5 (MyCase.Second hello)", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, Index(5)))))
    ))
  }

  it should " be seen to the type if all cases are accounted for" in {
    testCodeScript(Vector(
      CodeExpectation("val MyCase = Case(First: 5, Second: Identifier)", GeneralSuccessCheck),
      CodeExpectation("val MyCaseTo5: ReqMap(MyCase, 5) = ((First x): x, (Second x): 2)", GeneralSuccessCheck),
    ))
  }

  "Defining a non-generic linked list " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("ver ListOfCounts = new Type", GeneralSuccessCheck),
      CodeExpectation("update ListOfCounts (Nil, ())", GeneralSuccessCheck),
      CodeExpectation("update ListOfCounts (Node, (head: Count, tail: ListOfCounts))", GeneralSuccessCheck),
      CodeExpectation("val a: ListOfCounts = ListOfCounts.Nil()", GeneralSuccessCheck),
      CodeExpectation("val b: ListOfCounts = ListOfCounts.Node(5, a)", GeneralSuccessCheck),
      CodeExpectation("val c: ListOfCounts = ListOfCounts.Node(1, b)", GeneralSuccessCheck),
      CodeExpectation("val b2: ListOfCounts = ListOfCounts.Node(3, a)", GeneralSuccessCheck)
    ))
  }

  "Map conversions " should " have a contravariant input type" in {
    testCodeScript(Vector(
      CodeExpectation("val mySubset: Map(7, 2) = (0: 1, 1:1, 4:1, 6: 1)", GeneralSuccessCheck),
      CodeExpectation("val testInput1: ReqMap(7, Count) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val testInput2: ReqMap(Subtype(mySubset), Count) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val supertypeInputFunc: ReqMap(7, Count) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput1", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput2", FailureCheck),
      CodeExpectation("val subtypeInputFunc: ReqMap(Subtype(mySubset), Count) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("subtypeInputFunc testInput1", GeneralSuccessCheck),
      CodeExpectation("subtypeInputFunc testInput2", GeneralSuccessCheck)
    ))
  }

  it should " have a covariant output type" in {
    testCodeScript(Vector(
      CodeExpectation("val mySubset: Map(7, 2) = (0: 1, 1:1, 4:1, 6: 1)", GeneralSuccessCheck),
      CodeExpectation("val testInput1: ReqMap(Count, 7) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val testInput2: ReqMap(Count, Subtype(mySubset)) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val supertypeInputFunc: ReqMap(Count, 7) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput1", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput2", GeneralSuccessCheck),
      CodeExpectation("val subtypeInputFunc: ReqMap(Count, Subtype(mySubset)) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("subtypeInputFunc testInput1", FailureCheck),
      CodeExpectation("subtypeInputFunc testInput2", GeneralSuccessCheck)
    ))
  }

  "ReqMaps " should " not be allowed to have an updatable key type" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("update n()", GeneralSuccessCheck),
      CodeExpectation("val m: ReqMap(n, 5) = (0: 3, 1: 4)", FailureCheck)
    ))
  }

  "Expanding Subsets " should " work for indecies" in {
    testCodeScript(Vector(
      CodeExpectation("ver ID = new ExpandingSubset(10)", GeneralSuccessCheck),
      CodeExpectation("update ID 2", GeneralSuccessCheck),
      CodeExpectation("update ID 7", GeneralSuccessCheck),
      CodeExpectation("update ID 5", GeneralSuccessCheck),
      CodeExpectation("val x: ID = 2", GeneralSuccessCheck),
      CodeExpectation("val y: ID = 3", FailureCheck)
    ))
  }

  it should " work for counts" in {
    testCodeScript(Vector(
      CodeExpectation("ver ID = new ExpandingSubset(Count)", GeneralSuccessCheck),
      CodeExpectation("update ID 2", GeneralSuccessCheck),
      CodeExpectation("update ID 73", GeneralSuccessCheck),
      CodeExpectation("update ID 5555", GeneralSuccessCheck),
      CodeExpectation("val x: ID = 73", GeneralSuccessCheck),
      CodeExpectation("val y: ID = 1", FailureCheck)
    ))
  }

  it should " work for identifiers" in {
    testCodeScript(Vector(
      CodeExpectation("ver ID = new ExpandingSubset(Identifier)", GeneralSuccessCheck),
      CodeExpectation("update ID hello", GeneralSuccessCheck),
      CodeExpectation("update ID world", GeneralSuccessCheck),
      CodeExpectation("val x: ID = hello", GeneralSuccessCheck),
      CodeExpectation("val y: ID = hi", FailureCheck)
    ))
  }

  "Tables " should " be allowed to be created" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Table(Count, Identifier)", GeneralSuccessCheck),
      CodeExpectation("update n hello", GeneralSuccessCheck),
      CodeExpectation("update n world", GeneralSuccessCheck),
      CodeExpectation("n", GeneralSuccessCheck),
      CodeExpectation("n 1", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("world")))),
    ))
  }

  it should " have a Sequence shortcut if they are based on a count" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Sequence(10)", GeneralSuccessCheck),
      CodeExpectation("update n 2", GeneralSuccessCheck),
      CodeExpectation("update n 7", GeneralSuccessCheck),
      CodeExpectation("update n 5", GeneralSuccessCheck),
      CodeExpectation("n 2", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(5, Index(10))))),
      CodeExpectation("n 3", FailureCheck)
    ))
  }

  it should " world for an expanding subset" in {
    val tuesdayResponse = TaggedObject(
      UIdentifier("TUE"),
      IdentifierT
    )

    testCodeScript(Vector(
      CodeExpectation("ver idChange = new Table(ExpandingSubset(Identifier), Identifier)", GeneralSuccessCheck),
      CodeExpectation("update idChange (Monday, MON)", GeneralSuccessCheck),
      CodeExpectation("update idChange (Tuesday, TUE)", GeneralSuccessCheck),
      CodeExpectation("update idChange (Wednesday, WED)", GeneralSuccessCheck),
      CodeExpectation("idChange Tuesday", SuccessCheck(ExpOnlyEnvironmentCommand(tuesdayResponse))),
      CodeExpectation("idChange Thursday", FailureCheck)
    ))
  }

  /**
   * 
   * 
   * 

  /* Canonical way to define boolean type */
  ver Bool = new CaseType
  update Bool case False
  update Bool case True
  

  More about DataType:
  Starts with Case()
  The inputs are keys, n
  This means there is 1 ReqMap:
    ReqMap(n, Identifier)
  (It also should preserve equality)

  Then theres a ReqMap(n, Type) which gives an input type
  - The input type could be anything
  - Starts out as unit
    - Maybe new Struct (where you can add a field)

  How does Bool get saved?
  - Just have direct names as entries (makes storing these things less dependant on uuid choice)
  - It's stored in a standard "prelude" document that everything should be based upon
  - True and False constructors also stored in the prelude.

  Equality preserving Maps.. can these exist?
  -> First there needs to be an equality function (a == b)
  -> Can equality function this be built?
     - First, a and b must have the same construction type (So how do we define that)
     - Then, equality for cases and structs are easy
     - Equality for basic maps is clear
     - No need to go further yet!
  -> How do we know if a map preserves equality?
     - Write this function ad-hoc

  Equals function (a == b) requires some form of generics?
  UGHH!!!
  We can skip this for now?


  */
}