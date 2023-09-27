package ai.newmap.interpreter

import org.scalatest._
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

// Expect a success from this line of code, with output given as this string
case class SuccessCheckStr(s: String) extends ResultExpectation


class TestFullEnvironmentInterpreter extends FlatSpec {
  def Index(i: Long): NewMapObject = NewMapObject(UIndex(i), CountT)
  def IndexValue(i: Long, nType: NewMapType): NewMapObject = NewMapObject(UIndex(i), nType)
  def IndexTN(i: Long): NewMapType = IndexT(UIndex(i))

  /**
   * test a bunch of lines of newmap code
   * on each line, you can check that it succeeds, fails, or 
   */
  def testCodeScript(expectations: Vector[CodeExpectation]): Unit = {
    val interpreter = new EnvironmentInterpreter(
      printInitCommandErrors = false,
      suppressStdout = true
    )

    expectations.foreach(expectation => {
      val interpretation = interpreter(expectation.line)
      expectation.resultExpectation match {
        case FailureCheck => assert(interpretation.isFailure)
        case GeneralSuccessCheck => {
          interpretation match {
            case Success(_) => ()
            case Failure(msg) => fail(msg)
          }
        }
        case SuccessCheck(com) => {
          interpretation match {
            case Success(msg) => assert(msg == com.displayString(interpreter.env))
            case Failure(msg) => fail(msg)
          }
        }
        case SuccessCheckStr(s) => {
          interpretation match {
            case Success(msg) => assert(msg == s)
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

  def bind(key: UntaggedObject, value: UntaggedObject): (UntaggedObject, UntaggedObject) = {
    key -> value
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

  "A Character " should " be creatable" in testLineWorks("val x: Char = `a")
  it should " not work for multiple letters" in testLineFails("val x: Char = `ax")
  it should " work for numbers" in testLineWorks("val x: Char = `3")
  it should " not be convertible to Count" in testLineFails("val x: Count = `c")

  "A static map " should " be creatable" in {
    val code = "val m: Map (3: 100) = (0: 20, 1: 43, 2: 67)"

    val correctCommand = Environment.eCommand(
      "m",
      NewMapObject(
        UMap(Vector(
          bind(UIndex(0), UIndex(20)),
          bind(UIndex(1), UIndex(43)),
          bind(UIndex(2), UIndex(67))
        )),
        MapT(
          TypeTransform(IndexTN(3), IndexTN(100)),
          MapConfig(CommandOutput, BasicMap)
        )
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be creatable as a type" in {
    val code = "val m: Type = Map (3: 100)"

    val correctCommand = Environment.eCommand(
      "m",
      Environment.typeAsObject(MapT(TypeTransform(IndexTN(3), IndexTN(100)), MapConfig(CommandOutput, BasicMap)))
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  it should " be applyable to a key" in {
    val correctCommand = Environment.eCommand("result", NewMapObject(UIndex(43), IndexTN(100)))
    testCodeScript(Vector(
      CodeExpectation("val m: Map (3: 100) = (0: 20, 1: 43, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
    ))
  }

  it should " be applyable to a key not specified and use the default" in {
    val correctCommand = Environment.eCommand("result", NewMapObject(UIndex(0), IndexTN(100)))

    testCodeScript(Vector(
      CodeExpectation("val m: Map (3: 100) = (0: 20, 2: 67)", GeneralSuccessCheck),
      CodeExpectation("val result: 100 = m 1", SuccessCheck(correctCommand))
      // TODO - check equality between result and 0 here
    ))
  }


  it should " be creatable as a type object pair" in {
    val code = "val m: Map(3: 100) = (0: 10, 2: 3)"

    val correctCommand = Environment.eCommand(
      "m",
      NewMapObject(
        UMap(Vector(
          bind(UIndex(0), UIndex(10)),
          bind(UIndex(2), UIndex(3))
        )),
        MapT(TypeTransform(IndexTN(3), IndexTN(100)), MapConfig(CommandOutput, BasicMap))
      )
    )

    testCodeLine(CodeExpectation(code, SuccessCheck(correctCommand)))
  }

  "A struct " should " be created" in {
    val structType = Environment.structTypeFromParams(Vector(("a",IndexTN(2)), ("b",IndexTN(3))))
    val correctCommand = Environment.eCommand(
      "s",
      NewMapObject(
        UMap(Vector(
          bind(UIdentifier("a"), UIndex(0)), 
          bind(UIdentifier("b"), UIndex(0))
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
      IndexValue(1, IndexTN(3))
    )

    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s b", SuccessCheck(correctCommand))
    ))
  }

  it should " allow its fields to be accessed in expresions that can be evaluated to literals " in {
    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val fieldMap: ReqMap(2: Identifier) = (0: z, 1: b)", GeneralSuccessCheck),
      CodeExpectation("val q: 3 = s (fieldMap(1))", GeneralSuccessCheck)
    ))
  }

  it should " not allow fields that cannnot be evaluated to literals " in {
    testCodeScript(Vector(
      CodeExpectation("val s: (a: 2, b: 3) = (a:0, b:1)", GeneralSuccessCheck),
      CodeExpectation("val q: Field => 3 = (x: s|x)", FailureCheck)
    ))
  }

  it should " be considered a type in a type input" in {
    val correctCommand = Environment.eCommand(
      "testType",
      Environment.typeAsObject(MapT(
        TypeTransform(
          Environment.structTypeFromParams(
            Vector(
              "a" -> IndexTN(3),
              "b" -> IndexTN(3)
            )
          ),
          IndexTN(100)
        ),
        MapConfig(CommandOutput, BasicMap)
      ))
    )

    testCodeScript(Vector(
      CodeExpectation("val myStruct: Type = (a: 3, b: 3)", GeneralSuccessCheck),
      CodeExpectation("val testType: Type = Map (myStruct: 100)", SuccessCheck(correctCommand))
    ))
  }

  it should " be creatable with numerical indecies" in {
    testCodeScript(Vector(
      CodeExpectation("val s: (Type, 10) = (Count, 6)", GeneralSuccessCheck)
    ))
  }

  it should " be creatable as a versioned object" in {
    testCodeScript(Vector(
      CodeExpectation("ver x = new ()", GeneralSuccessCheck),
      CodeExpectation("update x: (myField, Count|3)", GeneralSuccessCheck),
    ))
  }

  it should " be updatable with a pre-defined tagged object" in {
    testCodeScript(Vector(
      CodeExpectation("ver x = new ()", GeneralSuccessCheck),
      CodeExpectation("val o: Object = Count|3", GeneralSuccessCheck),
      CodeExpectation("update x: (myField, o)", GeneralSuccessCheck),
    ))
  }

  it should " be recognized as complete when in a pattern " in {
    testCodeScript(Vector(
      CodeExpectation("val myStructSimple: Type = (first: 1, second: 1)", GeneralSuccessCheck),
      CodeExpectation("val myMap: ReqMap(myStructSimple: Identifier) = ((0, 0): x)", GeneralSuccessCheck),
      CodeExpectation("val myMap: ReqMap((first: 1, second: 1): Identifier) = ((0, 0): x)", GeneralSuccessCheck),
      CodeExpectation("val myStruct: Type = (first: 2, second: 3)", GeneralSuccessCheck),
      CodeExpectation("val myMap: ReqMap(myStruct: Identifier) = ((0, 0): a, (0, 1): b, (0, 2): c, (1, 0): d, (1, 1): e, (1, 2): f)", GeneralSuccessCheck),
      CodeExpectation("val myMap: ReqMap(myStruct: Identifier) = ((0, 0): a, (0, 1): b, (1, 0): d, (1, 1): e, (1, 2): f)", FailureCheck),
    ))
  }

  "A Map to a non-command type " should " not be creatable" in {
    testLineFails("val m: Map (10, Identifier) = (6: a)")
  }

  "A case " should " be created and instantiated" in {
    val correctCommand = Environment.eCommand(
      "x",
      NewMapObject(UCase(UIdentifier("a"), UIndex(0)), CustomT("MyCase", UArray(Array.empty)))
    )

    testCodeScript(Vector(
      CodeExpectation("data MyCase = CaseType", GeneralSuccessCheck),
      CodeExpectation("update MyCase: (a, 2)", GeneralSuccessCheck),
      CodeExpectation("update MyCase: (b, 3)", GeneralSuccessCheck),
      // TODO - allow this check without knowing mycase id!
      CodeExpectation("val x: MyCase = a|0", SuccessCheck(correctCommand)),
      CodeExpectation("val y: MyCase = a|b", FailureCheck),
      CodeExpectation("val x: MyCase = c", FailureCheck),
    ))
  }

  "Lambda expressions" should " be creatable as a type, object and applied" in {
    val correctCommandCreateFunc = Environment.eCommand(
      "f",
      NewMapObject(
        UMap(Vector(
          UWildcardPattern("a") ->
          ApplyFunction(
            UMap(Vector(
              bind(UIndex(0), UIndex(2)),
              bind(UIndex(1), UIndex(3)),
              bind(UIndex(2), UIndex(1))
            )),
            ParamId("a"),
            StandardMatcher
          )
        )),
        MapT(TypeTransform(IndexTN(3), IndexTN(4)), MapConfig(RequireCompleteness, FullFunction))
      )
    )

    val correctCommandUseFunc = Environment.eCommand(
      "result",
      IndexValue(3, IndexTN(4))
    )

    val correctCommandUseSimpleFunc = Environment.eCommand(
      "resultSimple",
      IndexValue(1, IndexTN(4))
    )

    val correctCommandUseParenFunc = Environment.eCommand(
      "resultParen",
      IndexValue(2, IndexTN(4))
    )

    testCodeScript(Vector(
      CodeExpectation("val fSig: Type = (3 => 4)", SuccessCheck(Environment.eCommand(
        "fSig",
        Environment.typeAsObject(Environment.fullFuncT(TypeTransform(IndexTN(3), IndexTN(4))))
      ))),
      CodeExpectation("val m: Map(3: 4) = (0: 2, 1: 3, 2: 1)", GeneralSuccessCheck),
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
      CodeExpectation("val m: Map(5: 10) = (0: 0, 1: 2, 2: 4, 3: 6, 4: 8)", GeneralSuccessCheck),
      CodeExpectation("f (1, m)", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, IndexTN(10)))))
    ))
  }

  /*
  // This "might" be possible with generics
  it should " be creatable without a type given" in {
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

  "Generic Identity Function " should " have a valid type signature" in {
    testCodeScript(Vector(
      CodeExpectation("val GenericIdType: Type = GenericMap(t: t)", GeneralSuccessCheck)
    ))
  }

  it should " be creatable from its type" in {
    testCodeScript(Vector(
      CodeExpectation("val GenericIdType: Type = GenericMap(t: t)", GeneralSuccessCheck),
      CodeExpectation("val gid: GenericIdType = (t: t)", GeneralSuccessCheck)
    ))
  }

  it should " work" in {
    testCodeScript(Vector(
      CodeExpectation("val GenericId: GenericMap(t: t) = (t: t)", GeneralSuccessCheck),
      CodeExpectation("GenericId ~hi", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIdentifier("hi"), IdentifierT)))),
      CodeExpectation("GenericId 5", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
      CodeExpectation("val m: Map(5: 2) = (0: 1, 3: 1)", GeneralSuccessCheck),
      // This line is needed because 3 will get converted into a count
      // - in the future, when we're working with type classes, maybe we can call (m (GenericId 3)) directly
      CodeExpectation("val x: 5 = 3", GeneralSuccessCheck),
      CodeExpectation("m (GenericId x)", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(1), IndexTN(2)))))
    ))
  }

  "Generic Constant Function " should " be creatable" in {
    testCodeScript(Vector(
      CodeExpectation("val constantCount: GenericMap(t: Count) = (x: 5)", GeneralSuccessCheck),
      CodeExpectation("constantCount 10", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
      CodeExpectation("constantCount ~hello", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT))))
    ))
  }

  // TODO: Think about this - maybe this shouldn't be allowed because things like "0" and "~yo" have no type interpretation
  // - Maybe require this to be composed with maps that have an exact input type
  it should " be able to include exceptions" in {
    testCodeScript(Vector(
      CodeExpectation("val constantCount: GenericMap(t: Count) = (0: 100, ~yo: 1, x: 5)", GeneralSuccessCheck),
      CodeExpectation("constantCount 10", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
      CodeExpectation("constantCount ~hello", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
      CodeExpectation("constantCount 0", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(100), CountT)))),
      CodeExpectation("constantCount ~yo", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(1), CountT))))
    ))
  }

  "Variable Substitution " should " work" in {
    testCodeScript(Vector(
      CodeExpectation("val id5: ReqMap(5: 5) = (t: t)", GeneralSuccessCheck),
      CodeExpectation("val gimmeWhatIWant: ReqMap(5: ReqMap(5: 5)) = (t: (s: t))", GeneralSuccessCheck),
      CodeExpectation("gimmeWhatIWant 2 4", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, IndexTN(5)))))
    ))
  }

  it should " respect inner-scoped variables" in {
    testCodeScript(Vector(
      CodeExpectation("val id5: ReqMap(5: 5) = (t: t)", GeneralSuccessCheck),
      CodeExpectation("val gimmeId5: ReqMap(Count: ReqMap(5: 5)) = (t: id5)", GeneralSuccessCheck),
      CodeExpectation("gimmeId5 10 1", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(1, IndexTN(5)))))
    ))
  }

  it should " avoid variable capture" in {
    testCodeScript(Vector(
      // This works because the map doesn't attempt to evaluate when it is defined
      // But it might break when it does!
      CodeExpectation("val f: ReqMap(5: ReqMap(5: 5)) = (x: (z: x))", GeneralSuccessCheck),      
      CodeExpectation("val g: ReqMap(5: ReqMap(5: 5)) = (z: (f z))", GeneralSuccessCheck),
      CodeExpectation("g 1 3", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(1, IndexTN(5)))))
    ))
  }

  "ReqMaps " should " require an input type" in {
    testCodeScript(Vector(
      CodeExpectation("val x = (0: 1, 1: 3, 4: 5)", FailureCheck)
    ))
  }

  it should " require all the values" in {
    testCodeScript(Vector(
      CodeExpectation("val x: ReqMap(5: 10) = (0: 1, 1: 3, 4: 5)", FailureCheck),
      CodeExpectation("val x: ReqMap(5: 10) = (0: 1, 1: 3, 2: 3, 3: 9, 4: 5)", GeneralSuccessCheck),
      CodeExpectation("x 0", GeneralSuccessCheck),
      CodeExpectation("x 3", GeneralSuccessCheck)
    ))
  }

  it should " be able to handle the most basic pattern matching" in {
    testCodeScript(Vector(
      CodeExpectation("val x: ReqMap(4: 4) = (0: 3, t: t)", GeneralSuccessCheck),
      CodeExpectation("x 0", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(3, IndexTN(4))))),
      CodeExpectation("x 3", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(3, IndexTN(4))))),
      CodeExpectation("x 2", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, IndexTN(4)))))
    ))
  }

  // This test exists because of a bug which got this wrong
  // The solution to this was avoiding variable capture
  it should " be able to handle a nested ReqMap properly" in {
    val expectedResult = Environment.typeAsObject(
      MapT(
        TypeTransform(
          IndexTN(2),
          MapT(
            TypeTransform(IndexTN(4), CountT),
            MapConfig(RequireCompleteness, SimpleFunction)
          )
        ),
        MapConfig(RequireCompleteness, SimpleFunction)
      )
    )

    testCodeScript(Vector(
      CodeExpectation(
        "ReqMap(2: ReqMap(4: Count))",
        SuccessCheck(ExpOnlyEnvironmentCommand(expectedResult))
      )
    ))
  }

  it should " keep constant and updatable key type" in {
    testCodeScript(Vector(
      CodeExpectation("data n = 0", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("val m: ReqMap(n: 5) = (0: 3, 1: 4)", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("val newVersioned: n = 2", GeneralSuccessCheck),
      CodeExpectation("m newVersioned", FailureCheck), // m still takes inputs 0, 1 even though n was updated
      CodeExpectation("val nv: n = 0", GeneralSuccessCheck),
      CodeExpectation("m nv", FailureCheck) // even though nv has a key in m, it still isn't allowed because it's part of the larger type
    ))
  }

  "A SimpleMap " should " be allowed to call other simple maps" in {
    testCodeScript(Vector(
      CodeExpectation("val m1: ReqMap(3: Identifier) = (0: Zero, 1: One, 2: Two)", GeneralSuccessCheck),
      CodeExpectation("val m2: ReqMap(3: Identifier) = (0: Nil, x: m1 x)", GeneralSuccessCheck),
      CodeExpectation("m2 0", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("Nil")))),
      CodeExpectation("m2 2", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("Two"))))
    ))
  }

  // These should be reserved for recursive maps
  it should " not be allowed to call a full function map" in {
    testCodeScript(Vector(
      CodeExpectation("data Funny = CaseType", GeneralSuccessCheck),
      CodeExpectation("update Funny: (FunMap, (Funny => 2))", GeneralSuccessCheck),
      CodeExpectation("val funnyMap: ReqMap(Funny: 2) = (_: 1)", GeneralSuccessCheck),  
      CodeExpectation("val funnyItem: Funny = FunMap|funnyMap", GeneralSuccessCheck),      
      CodeExpectation("val m: ReqMap(Funny: 2) = (f: funnyMap f)", GeneralSuccessCheck),
      CodeExpectation("val m: ReqMap(Funny: 2) = (FunMap|f: f funnyItem)", FailureCheck)
    ))
  }

  // TODO - the same thing must be caught for a map that's being updated with commands
  // TODO - must be upgraded with generic types
  it should " disallow self referential function calls" in {
    testCodeScript(Vector(
      CodeExpectation("data Funny = CaseType", GeneralSuccessCheck),
      CodeExpectation("update Funny: (FunMap, ReqMap(Funny: 2))", GeneralSuccessCheck),
      CodeExpectation("val m: Funny = FunMap|(_: 1)", GeneralSuccessCheck),
      // Preventing Russell's paradox!
      CodeExpectation("val f: ReqMap(Funny: 2) = (FunMap|x: x FunMap|x)", FailureCheck),
      // This line below will create an infinite loop if evaluated on itself!! (f m) should work. Out simple function check will catch it
      CodeExpectation("val f: Funny => 2 = (FunMap|x: x FunMap|x)", GeneralSuccessCheck),
    ))
  }

  "A struct pattern " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("val m1: ReqMap(4: Count) = (0: 100, 1: 101, 2: 102, 3: 103)", GeneralSuccessCheck),
      CodeExpectation("val m2: ReqMap(4: Count) = (0: 200, 1: 201, 2: 202, 3: 203)", GeneralSuccessCheck),
      CodeExpectation("val ifStatement: ReqMap(2: ReqMap(4: Count)) = (0: m1, 1: m2)", GeneralSuccessCheck),

      // TODO - clean up by building a shortcut
      CodeExpectation("val f: (4, 2) => Count = ((first, second): ifStatement second first)", GeneralSuccessCheck),
      CodeExpectation("f(0, 0)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(100)))),
      CodeExpectation("f(1, 1)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(201)))),
      CodeExpectation("f(2, 0)", SuccessCheck(ExpOnlyEnvironmentCommand(Index(102))))
    ))
  }

  "A Subset Type " should "work" in {
    testCodeScript(Vector(
      CodeExpectation("data x = Subtype(8)", GeneralSuccessCheck),
      CodeExpectation("update x: 0", GeneralSuccessCheck),
      CodeExpectation("update x: 1", GeneralSuccessCheck),
      CodeExpectation("update x: 4", GeneralSuccessCheck),
      CodeExpectation("val y: x = 1", GeneralSuccessCheck),
      CodeExpectation("val y: x = 6", FailureCheck)
    ))
  }

  it should " have functions made from them" in {
    testCodeScript(Vector(
      // TODO: should be able to call Subtype(True: 1, False: 1) - how can we make this work?
      CodeExpectation("data Bool = Subtype(Identifier)", GeneralSuccessCheck),
      CodeExpectation("update Bool: True", GeneralSuccessCheck),
      CodeExpectation("update Bool: False", GeneralSuccessCheck),
      CodeExpectation("val shorten: Map(Bool: 2) = (True: 1, False: 0)", GeneralSuccessCheck),
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
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("n", SuccessCheck(ExpOnlyEnvironmentCommand(Index(3))))
    ))
  }

  it should " be allowed to have members" in {
    testCodeScript(Vector(
      CodeExpectation("data n = 0", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("val p: n = 1", GeneralSuccessCheck),
    ))
  }

  "Versioned Objects " should " be initialized and updated for Map" in {
    testCodeScript(Vector(
      CodeExpectation("ver m = new Map(Identifier: Count)", GeneralSuccessCheck),
      CodeExpectation("update m: (hello, ())", GeneralSuccessCheck),
      CodeExpectation("update m: (world, ())", GeneralSuccessCheck),
      CodeExpectation("update m: (world, ())", GeneralSuccessCheck),
      CodeExpectation("m hello", SuccessCheck(ExpOnlyEnvironmentCommand(Index(1)))),
      CodeExpectation("m world", SuccessCheck(ExpOnlyEnvironmentCommand(Index(2)))),
      CodeExpectation("m yo", SuccessCheck(ExpOnlyEnvironmentCommand(Index(0))))
    ))
  }

  it should " be convertable to values and stop updating" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("val x: Count = n", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("x", SuccessCheck(ExpOnlyEnvironmentCommand(Index(1)))),
      CodeExpectation("n", SuccessCheck(ExpOnlyEnvironmentCommand(Index(3))))
    ))
  }

  it should " be forked correctly" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Count", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("fork n as nForked", GeneralSuccessCheck),
      CodeExpectation("update nForked: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update nForked: ()", GeneralSuccessCheck),
      CodeExpectation("update nForked: ()", GeneralSuccessCheck),
      CodeExpectation("update nForked: ()", GeneralSuccessCheck),
      CodeExpectation("update n: ()", GeneralSuccessCheck),
      CodeExpectation("update nForked: ()", GeneralSuccessCheck),
      CodeExpectation("n", SuccessCheck(ExpOnlyEnvironmentCommand(Index(5)))),
      CodeExpectation("nForked", SuccessCheck(ExpOnlyEnvironmentCommand(Index(8))))
    ))
  }

  it should " fail when forked off a non-versioned object" in {
    testCodeScript(Vector(
      CodeExpectation("val x: Count = 10", GeneralSuccessCheck),
      CodeExpectation("fork n as x", FailureCheck)
    ))
  }

  "Defining Bool as a Case " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("data Bool = CaseType", GeneralSuccessCheck),
      CodeExpectation("update Bool: (False, ())", GeneralSuccessCheck),
      CodeExpectation("update Bool: (True, ())", GeneralSuccessCheck),
      CodeExpectation("val t: Bool = True", GeneralSuccessCheck),
      CodeExpectation("val f: Bool = False", GeneralSuccessCheck)
    ))
  }

  "A case pattern " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("data MyCase = CaseType", GeneralSuccessCheck),
      CodeExpectation("update MyCase: (First, 5)", GeneralSuccessCheck),
      CodeExpectation("update MyCase: (Second, Identifier)", GeneralSuccessCheck),
      CodeExpectation("val MyCaseTo5: ReqMap(MyCase: 5) = (First|x: x, Second|x: 2)", GeneralSuccessCheck),
      CodeExpectation("MyCaseTo5 First|4", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(4, IndexTN(5))))),
      CodeExpectation("MyCaseTo5 Second|hello", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, IndexTN(5)))))
    ))
  }

  it should " be seen to the type if all cases are accounted for" in {
    testCodeScript(Vector(
      CodeExpectation("data MyCase = CaseType", GeneralSuccessCheck),
      CodeExpectation("update MyCase: (First, 5)", GeneralSuccessCheck),
      CodeExpectation("update MyCase: (Second, Identifier)", GeneralSuccessCheck),
      CodeExpectation("val MyCaseTo5: ReqMap(MyCase: 5) = ((First|x): x, (Second|x): 2)", GeneralSuccessCheck),
      CodeExpectation("val IncompleteMyCaseTo5: ReqMap(MyCase: 5) = ((First|x): x)", FailureCheck),
    ))
  }

  "Defining a non-generic linked list " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("data ListOfCounts = CaseType", GeneralSuccessCheck),
      CodeExpectation("update ListOfCounts: (Empty, ())", GeneralSuccessCheck),
      CodeExpectation("update ListOfCounts: (Node, (head: Count, tail: ListOfCounts))", GeneralSuccessCheck),
      CodeExpectation("val a: ListOfCounts = Empty", GeneralSuccessCheck),
      CodeExpectation("val b: ListOfCounts = Node|(5, a)", GeneralSuccessCheck),
      CodeExpectation("val c: ListOfCounts = Node|(1, b)", GeneralSuccessCheck),
      CodeExpectation("val b2: ListOfCounts = Node|(3, a)", GeneralSuccessCheck)
    ))
  }

  it should " be allowed to be equipted with a recursive length function" in {
    testCodeScript(Vector(
      CodeExpectation("data ListOfCounts = CaseType", GeneralSuccessCheck),
      CodeExpectation("update ListOfCounts: (Empty, ())", GeneralSuccessCheck),
      CodeExpectation("update ListOfCounts: (Node, (head: Count, tail: ListOfCounts))", GeneralSuccessCheck),
      CodeExpectation("val length: (ListOfCounts => Count) = (Empty|_: 0, Node|(head, tail): Increment (length(tail)))", FailureCheck),
      CodeExpectation("def length: ReqMaq(ListOfCounts, Count) = (Empty|_: 0, Node|(head, tail): Increment (length(tail)))", FailureCheck),
      CodeExpectation("def length: (ListOfCounts => Count) = (Empty|_: 0, Node|(head, tail): Increment (length(tail)))", GeneralSuccessCheck),
      CodeExpectation("length Empty", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(0, CountT)))),
      CodeExpectation("length (Node|(6, Node|(2, Empty)))", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(2, CountT)))),
      // Include examples to test the length function
    ))
  }

  "A generic option case " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("data Option (T: Type)", GeneralSuccessCheck),
      CodeExpectation("update Option: (None, ())", GeneralSuccessCheck),
      CodeExpectation("update Option: (Some, T)", GeneralSuccessCheck),
      CodeExpectation("val maybeSix = Option|6", GeneralSuccessCheck),
      CodeExpectation("val x: maybeSix = None", GeneralSuccessCheck),
      CodeExpectation("val y: maybeSix = Some|1", GeneralSuccessCheck),
      CodeExpectation("val y: maybeSix = Some|10", FailureCheck),
      CodeExpectation("val z: maybeSix = None|3", FailureCheck),
    ))
  }

  it should " be callable without naming it" in {
    testCodeScript(Vector(
      CodeExpectation("data Option (T: Type)", GeneralSuccessCheck),
      CodeExpectation("update Option: (None, ())", GeneralSuccessCheck),
      CodeExpectation("update Option: (Some, T)", GeneralSuccessCheck),
      CodeExpectation("val x: Option|Count = None|()", GeneralSuccessCheck),
      CodeExpectation("val x: Option|Count = None", GeneralSuccessCheck),
      CodeExpectation("val y: Option|Count = Some|20", GeneralSuccessCheck),
      CodeExpectation("val z: Option|Count = None|0", FailureCheck),
    ))
  }

  it should " be ok to equip with a getOrElse function" in {
    testCodeScript(Vector(
      CodeExpectation("data Option (T: Type)", GeneralSuccessCheck),
      CodeExpectation("update Option: (None, ())", GeneralSuccessCheck),
      CodeExpectation("update Option: (Some, T)", GeneralSuccessCheck),
      CodeExpectation("val getOrElse: GenericMap (Option|T: ReqMap(T: T)) = (None|(): (t: t), Some|t: (_: t))", GeneralSuccessCheck),
      CodeExpectation("val x: Option|Count = None", GeneralSuccessCheck),
      CodeExpectation("val y: Option|Count = Some|20", GeneralSuccessCheck),
      CodeExpectation("getOrElse x 5", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(5, CountT)))),
      CodeExpectation("getOrElse y 5", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(20, CountT))))
    ))
  }

  it should " be covariant" in {
    testCodeScript(Vector(
      CodeExpectation("data Option (T: Type)", GeneralSuccessCheck),
      CodeExpectation("update Option: (None, ())", GeneralSuccessCheck),
      CodeExpectation("update Option: (Some, T)", GeneralSuccessCheck),
      CodeExpectation("data MyType = Subtype(Count)", GeneralSuccessCheck),
      CodeExpectation("update MyType: 3", GeneralSuccessCheck),
      CodeExpectation("val myValue: Option|MyType = Some|3", GeneralSuccessCheck),
      CodeExpectation(
        "val myFunction: ReqMap(Option|Count: Identifier) = (_: ~hello)",
        GeneralSuccessCheck
      ),
      // In order to get this working, we'll have to let pattern matching look at convertibility
      // not just equality!!!
      CodeExpectation("myFunction myValue", SuccessCheck(ExpOnlyEnvironmentCommand(
        NewMapObject(
          UIdentifier("hello"),
          IdentifierT
        )
      ))),
      CodeExpectation(
        "val myOtherFunction: ReqMap(Option|MyType: Identifier) = (_: ~hi)",
        GeneralSuccessCheck
      ),
      CodeExpectation("val myCount: Option|Count = Some|3", GeneralSuccessCheck),
      CodeExpectation("val myBadCount: Option|Count = Some|4", GeneralSuccessCheck),
      CodeExpectation("myOtherFunction myBadCount", FailureCheck),
      // TODO: Perhaps this next one should succeed because the object is convertible into the type
      CodeExpectation("myOtherFunction myCount", FailureCheck)
    ))
  }

  "A generic LinkedList case " should " be possible" in {
    testCodeScript(Vector(
      CodeExpectation("data List (T: Type)", GeneralSuccessCheck),
      CodeExpectation("update List: (Empty, ())", GeneralSuccessCheck),
      CodeExpectation("update List: (Node, (head: T, tail: List|T))", GeneralSuccessCheck),
      CodeExpectation("val a: List|Count = Empty", GeneralSuccessCheck),
      CodeExpectation("val b: List|Count = Node|(5, a)", GeneralSuccessCheck),
      CodeExpectation("val c: List|Count = Node|(1, b)", GeneralSuccessCheck),
      CodeExpectation("val b2: List|Count = Node|(3, a)", GeneralSuccessCheck)
    ))
  }

  "Map conversions " should " have a contravariant input type" in {
    testCodeScript(Vector(
      CodeExpectation("data mySubset = Subtype(7)", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 0", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 1", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 4", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 6", GeneralSuccessCheck),
      CodeExpectation("val testInput1: ReqMap(7: Count) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val testInput2: ReqMap(mySubset: Count) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val supertypeInputFunc: ReqMap(7: Count) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput1", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput2", FailureCheck),
      CodeExpectation("val subtypeInputFunc: ReqMap(mySubset: Count) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("subtypeInputFunc testInput1", GeneralSuccessCheck),
      CodeExpectation("subtypeInputFunc testInput2", GeneralSuccessCheck)
    ))
  }

  it should " have a covariant output type" in {
    testCodeScript(Vector(
      CodeExpectation("data mySubset = Subtype(7)", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 0", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 1", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 4", GeneralSuccessCheck),
      CodeExpectation("update mySubset: 6", GeneralSuccessCheck),
      CodeExpectation("val testInput1: ReqMap(Count: 7) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val testInput2: ReqMap(Count: mySubset) = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("val supertypeInputFunc: ReqMap(Count: 7) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput1", GeneralSuccessCheck),
      CodeExpectation("supertypeInputFunc testInput2", GeneralSuccessCheck),
      CodeExpectation("val subtypeInputFunc: ReqMap(Count: mySubset) => Count = (_: 0)", GeneralSuccessCheck),
      CodeExpectation("subtypeInputFunc testInput1", FailureCheck),
      CodeExpectation("subtypeInputFunc testInput2", GeneralSuccessCheck)
    ))
  }

  "Expanding Subsets " should " work for indecies" in {
    testCodeScript(Vector(
      CodeExpectation("data ID = Subtype(10)", GeneralSuccessCheck),
      CodeExpectation("update ID: 2", GeneralSuccessCheck),
      CodeExpectation("update ID: 7", GeneralSuccessCheck),
      CodeExpectation("update ID: 5", GeneralSuccessCheck),
      CodeExpectation("val x: ID = 2", GeneralSuccessCheck),
      CodeExpectation("val y: ID = 3", FailureCheck)
    ))
  }

  it should " work for counts" in {
    testCodeScript(Vector(
      CodeExpectation("data ID = Subtype(Count)", GeneralSuccessCheck),
      CodeExpectation("update ID: 2", GeneralSuccessCheck),
      CodeExpectation("update ID: 73", GeneralSuccessCheck),
      CodeExpectation("update ID: 5555", GeneralSuccessCheck),
      CodeExpectation("val x: ID = 73", GeneralSuccessCheck),
      CodeExpectation("val y: ID = 1", FailureCheck)
    ))
  }

  it should " work for identifiers" in {
    testCodeScript(Vector(
      CodeExpectation("data ID = Subtype(Identifier)", GeneralSuccessCheck),
      CodeExpectation("update ID: hello", GeneralSuccessCheck),
      CodeExpectation("update ID: world", GeneralSuccessCheck),
      CodeExpectation("val x: ID = hello", GeneralSuccessCheck),
      CodeExpectation("val y: ID = hi", FailureCheck)
    ))
  }

  "Tables " should " be allowed to be created" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Table(0: Identifier)", GeneralSuccessCheck),
      CodeExpectation("update n: hello", GeneralSuccessCheck),
      CodeExpectation("update n: world", GeneralSuccessCheck),
      CodeExpectation("n", GeneralSuccessCheck),
      CodeExpectation("n 1", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapO.identifier("world")))),
    ))
  }

  it should " have a Sequence shortcut if they are based on a count" in {
    testCodeScript(Vector(
      CodeExpectation("ver n = new Sequence(10)", GeneralSuccessCheck),
      CodeExpectation("update n: 2", GeneralSuccessCheck),
      CodeExpectation("update n: 7", GeneralSuccessCheck),
      CodeExpectation("update n: 5", GeneralSuccessCheck),
      CodeExpectation("n 2", SuccessCheck(ExpOnlyEnvironmentCommand(IndexValue(5, IndexTN(10))))),
      CodeExpectation("n 3", FailureCheck)
    ))
  }

  it should " work for an expanding subset" in {
    val tuesdayResponse = NewMapObject(
      UIdentifier("TUE"),
      IdentifierT
    )

    testCodeScript(Vector(
      CodeExpectation("ver idChange = new Table(Subtype(Identifier): Identifier)", GeneralSuccessCheck),
      CodeExpectation("update idChange: (Monday, MON)", GeneralSuccessCheck),
      CodeExpectation("update idChange: (Tuesday, TUE)", GeneralSuccessCheck),
      CodeExpectation("update idChange: (Wednesday, WED)", GeneralSuccessCheck),
      CodeExpectation("idChange Tuesday", SuccessCheck(ExpOnlyEnvironmentCommand(tuesdayResponse))),
      CodeExpectation("idChange Thursday", FailureCheck)
    ))
  }

  "A typeOf function " should " be creatable as a type class" in {
    testCodeScript(Vector(
      CodeExpectation("val x: 9 = 4", GeneralSuccessCheck),
      CodeExpectation("val y: Identifier = ~hi", GeneralSuccessCheck),
      CodeExpectation("_typeOf x", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(Environment.typeAsUntaggedObject(IndexTN(9)), TypeT)))),
      CodeExpectation("_typeOf y", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(Environment.typeAsUntaggedObject(IdentifierT), TypeT)))),
      CodeExpectation("_typeOf 4", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(Environment.typeAsUntaggedObject(CountT), TypeT)))),
    ))
  }

  "An Array " should " be creatable" in {
    testCodeScript(Vector(
      CodeExpectation("val x: Array|Identifier = 3|(0: ~zero, 1: ~one, 2: ~two)", GeneralSuccessCheck),
    ))
  }

  it should " come with a length function" in {
    testCodeScript(Vector(
      CodeExpectation("val x: Array|Identifier = 5|(0: ~zero, 1: ~one, 2: ~two, 3: ~three, 4: ~four)", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len x", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
    ))
  }

  it should " be updatable with new values" in {
    testCodeScript(Vector(
      CodeExpectation("ver myArray = new Array|10", GeneralSuccessCheck),
      CodeExpectation("update myArray: 1", GeneralSuccessCheck),
      CodeExpectation("update myArray: 3", GeneralSuccessCheck),
      CodeExpectation("update myArray: 1", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len myArray", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(3), CountT)))),
      CodeExpectation("myArray", SuccessCheck(ExpOnlyEnvironmentCommand(
        NewMapObject(
          UCase(UIndex(3), UArray(Array(UIndex(1), UIndex(3), UIndex(1)))),
          CustomT("Array", UCase(UIdentifier("Index"), UIndex(10)))
        )
      ))),
    )) 
  }

  it should " be creatable with brackets" in {
    testCodeScript(Vector(
      CodeExpectation("val y: Array|Count = 5|[2, 3, 5, 7, 11]", GeneralSuccessCheck),
    ))
  }

  it should " be creatable with empty brackets" in {
    testCodeScript(Vector(
      CodeExpectation("val y: Array|Count = 0|[]", GeneralSuccessCheck),
    ))
  }

  it should " be creatable with empty brackets with ReqMap" in {
    testCodeScript(Vector(
      CodeExpectation("val y: ReqMap(0: Count) = []", GeneralSuccessCheck),
    ))
  }

  it should " come with a length function that also works with brackets" in {
    testCodeScript(Vector(
      CodeExpectation("val y: Array|Count = 5|[2, 3, 5, 7, 11]", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len y", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
    ))
  }

  "Iterations " should " work from an array into a histogram" in {
    testCodeScript(Vector(
      CodeExpectation("val myArray: Array|Count = 6|(0: 5, 1: 5, 2: 10, 3: 10, 4: 10, 5: 1)", GeneralSuccessCheck),
      CodeExpectation("ver hist = new Map(Count: Count)", GeneralSuccessCheck),
      CodeExpectation("iterate myArray into hist", GeneralSuccessCheck),
      CodeExpectation("hist 5", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(2), CountT)))),
    ))
  }

  it should " also work for the bracketed notation" in {
    testCodeScript(Vector(
      CodeExpectation("val myArray: Array|Count = 6|[5, 5, 10, 10, 10, 1]", GeneralSuccessCheck),
      CodeExpectation("ver hist = new Map(Count: Count)", GeneralSuccessCheck),
      CodeExpectation("iterate myArray into hist", GeneralSuccessCheck),
      CodeExpectation("hist 5", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(2), CountT)))),
    ))
  }

  it should " be usable to append arrays" in {
    testCodeScript(Vector(
      CodeExpectation("val myArray1: Array|Identifier = 3|(0: a, 1: b, 2: c)", GeneralSuccessCheck),
      CodeExpectation("val myArray2: Array|Identifier = 2|(0: d, 1: e)", GeneralSuccessCheck),
      CodeExpectation("ver appendedArray = new Array|Identifier", GeneralSuccessCheck),
      CodeExpectation("iterate myArray1 into appendedArray", GeneralSuccessCheck),
      CodeExpectation("iterate myArray2 into appendedArray", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len appendedArray", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(5), CountT)))),
    ))
  }

  it should " be usable on types" in {
    testCodeScript(Vector(
      CodeExpectation("ver appendedArray = new Array|10", GeneralSuccessCheck),
      CodeExpectation("val t: Type = 10", GeneralSuccessCheck),
      CodeExpectation("iterate t into appendedArray", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len appendedArray", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(10), CountT)))),
    ))
  }

  it should " be usable on types with a literal" in {
    testCodeScript(Vector(
      CodeExpectation("ver appendedArray = new Array|10", GeneralSuccessCheck),
      CodeExpectation("iterate 10 into appendedArray", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len appendedArray", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(10), CountT)))),
    ))
  }

  it should " work on subtypes " in {
    testCodeScript(Vector(
      CodeExpectation("data x = Subtype(Count)", GeneralSuccessCheck),
      CodeExpectation("update x: 10", GeneralSuccessCheck),
      CodeExpectation("update x: 3", GeneralSuccessCheck),
      CodeExpectation("update x: 10", GeneralSuccessCheck),
      CodeExpectation("update x: 2", GeneralSuccessCheck),
      CodeExpectation("ver a = new Array|Count", GeneralSuccessCheck),
      CodeExpectation("iterate x into a", GeneralSuccessCheck),
      CodeExpectation("val len: GenericMap(Array|T: Count) = (i|_: i)", GeneralSuccessCheck),
      CodeExpectation("len a", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(3), CountT)))),
    ))
  }

  "Strings " should " be creatable and updatable" in {
    testCodeScript(Vector(
      CodeExpectation("val s: String = \"asdf\"", GeneralSuccessCheck),
      CodeExpectation("val emptyString: String = \"\"", GeneralSuccessCheck),
      CodeExpectation("ver updatedString = new String", GeneralSuccessCheck),
      CodeExpectation("update updatedString: `a", GeneralSuccessCheck),
      CodeExpectation("update updatedString: `b", GeneralSuccessCheck)
    ))
  }

  it should " be iterable" in {
    testCodeScript(Vector(
      CodeExpectation("ver x = new String", GeneralSuccessCheck),
      CodeExpectation("val s: String = \"asdf\"", GeneralSuccessCheck),
      CodeExpectation("iterate s into x", GeneralSuccessCheck)
    ))
  }

  it should " be printed nicely" in {
    testCodeScript(Vector(
      CodeExpectation("val s: String = \"asdf\"", GeneralSuccessCheck),
      CodeExpectation("s", SuccessCheckStr("asdf"))
    ))
  }

  "The stdout channel " should " work with strings" in {
    testCodeScript(Vector(
      CodeExpectation("ver stdoutRecorder = new String", GeneralSuccessCheck),
      CodeExpectation("connectChannel stdout stdoutRecorder", GeneralSuccessCheck),
      CodeExpectation("val myString = \"hello world\"", GeneralSuccessCheck),
      CodeExpectation("write stdout myString", GeneralSuccessCheck),
      CodeExpectation("val yo: String = \" yo \"", GeneralSuccessCheck),
      CodeExpectation("write stdout yo", GeneralSuccessCheck),
      CodeExpectation("write stdout myString", GeneralSuccessCheck),
      CodeExpectation("stdoutRecorder", SuccessCheckStr("hello world yo hello world"))
    ))
  }

  "HelloWorld.nm script " should " behave as expected" in {
    testCodeScript(Vector(
      CodeExpectation("ver stdoutRecorder = new String", GeneralSuccessCheck),
      CodeExpectation("connectChannel stdout stdoutRecorder", GeneralSuccessCheck),
      CodeExpectation(":load TestScripts/HelloWorld.nm", GeneralSuccessCheck),
      CodeExpectation("stdoutRecorder", SuccessCheckStr("hello world !!\n"))
    ))
  }

  "Multiline commands " should " work as expected" in {
    testCodeScript(Vector(
      CodeExpectation(":load TestScripts/MultilineTest.nm", GeneralSuccessCheck),
      CodeExpectation("x", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(3), CountT)))),
    ))
  }

  "Addition " should " work for counts" in {
    testCodeScript(Vector(
      CodeExpectation("1 + 1", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(2), CountT)))),
      CodeExpectation("8 + 0", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(8), CountT)))),
      CodeExpectation("4 + 7", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(11), CountT)))),
      CodeExpectation("1 + 1 + 5", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIndex(7), CountT))))
    ))
  }

  it should " work for doubles" ignore {
    testCodeScript(Vector(
      CodeExpectation("1.0 + 1.", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UDouble(2.0), CountT)))),
      CodeExpectation("0.0 + 4.5", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UDouble(4.5), CountT)))),
   ))
  }

  "Field Maps " should " work" in {
    testCodeScript(Vector(
      CodeExpectation(
        """new basic map on 3 as numName returning Identifier = (0: ~zero, 1: ~one, 2: ~two)""",
        GeneralSuccessCheck
      ),
      CodeExpectation("val x: 3 = 1", GeneralSuccessCheck),
      CodeExpectation(
        "x.numName",
        SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UIdentifier("one"), IdentifierT)))
      ),
      CodeExpectation("numName(1)", FailureCheck),
      CodeExpectation("x.numName(1)", FailureCheck),
      CodeExpectation("3.f()", FailureCheck),
    ))
  }

  it should " work for shape area" in {
    testCodeScript(Vector(
      CodeExpectation("data Shape = CaseType", GeneralSuccessCheck),
      CodeExpectation("update Shape: (Square, Double)", GeneralSuccessCheck),
      CodeExpectation("update Shape: (Rect, [Double, Double])", GeneralSuccessCheck),
      CodeExpectation("new simple map on Shape as area returning Double = (Square|x: x * x, Rect|[x, y]: x * y)", GeneralSuccessCheck),
      CodeExpectation("val s: Shape = Square|2.0", GeneralSuccessCheck),
      CodeExpectation(
        "s.area",
        SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UDouble(4.0), DoubleT)))
      ),
      CodeExpectation("val t: Shape = Rect|[2.0, 5.0]", GeneralSuccessCheck),
      CodeExpectation(
        "t.area",
        SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UDouble(10.0), DoubleT)))
      ),
    ))
  }

  "The mean machine " should " work for getting the mean" in {
    testCodeScript(Vector(
      CodeExpectation(":load TestScripts/MeanMachine.nm", GeneralSuccessCheck),
      CodeExpectation("val mm: MeanMachine = (6.0, 3.0)", GeneralSuccessCheck),
      CodeExpectation("mm.mean", SuccessCheck(ExpOnlyEnvironmentCommand(NewMapObject(UDouble(2.0), DoubleT)))),
    ))
  }

  /*
  TODOs with the arrays:
  - startsWith function (or general regular expression?)
  - Commands (replace sequence)
  - append arrays
  - mapValue function (could this be done with maps too?)
  - Convert to LinkedList?

  Regular Expressions?
  - These are going to objects that carry "state", and the array is iterated into them
  */

  /*
  Code proposed for side effects, channels.

  A "pattern" is a regular expression with a state (which is a regular expression)
  When you feed it data, it changes state

  What happens when it finds a pattern?
  - it emits a side effect, pattern found "result"
  - this includes some environment updates (new variables?)
  - What does this look like?

  val pattern: RegularExpression = r"hello"
  
  connect pattern result to found
  s.listChars to pattern
  ???
  */

  /**
   * 
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