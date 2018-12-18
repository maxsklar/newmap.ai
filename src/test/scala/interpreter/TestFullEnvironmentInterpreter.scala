package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.interpreter._
import ai.newmap.interpreter.Lexer._
import ai.newmap.interpreter.NewMapParser._
import ai.newmap.model._
import ai.newmap.util.{Failure, Success}

class TestFullEnvironmentInterpreter extends FlatSpec {
  "A number " should " be allowed if it's one less than the type" in {
    val interp = new EnvironmentInterpreter()
    assert(interp("val x: 5 = 4").isSuccess)
  }

  it should " be allowed if it's less than the type" in {
    val interp = new EnvironmentInterpreter()
    assert(interp("val x: 10 = 4").isSuccess)
  }

  it should " fail when equal to the type" in {
    val interp = new EnvironmentInterpreter()
    assert(interp("val x: 9 = 9").isFailure)
  }

  it should " fail when greater than the type" in {
    val interp = new EnvironmentInterpreter()
    assert(interp("val x: 2 = 13").isFailure)
  }

  "A static map " should " be creatable" in {
    val interp = new EnvironmentInterpreter()
    val code = "val m: Map (key: 3, value: 100, default: 0) = (0: 20, 1: 43, 2: 67)"

    interp(code) match {
      case Failure(msg) => fail(msg)
      case Success(msg) => {
        val correctCommand = EnvironmentCommand(
          "m",
          MapT(IndexT(3), IndexT(100), Index(0)),
          MapInstance(Vector(
            Index(0) -> Index(20),
            Index(1) -> Index(43),
            Index(2) -> Index(67)
          ), Index(0))
        )
        assert(msg == correctCommand.toString)
      }
    }
  }

  it should " be creatable as a type" in {
    val interp = new EnvironmentInterpreter()
    val code = "val m: Type = Map (key: 3, value: 100, default: 0)"

    interp(code) match {
      case Failure(msg) => fail(msg)
      case Success(msg) => {
        val correctCommand = EnvironmentCommand(
          "m",
          TypeT,
          MapType(Index(3), Index(100), Index(0))
        )
        assert(msg == correctCommand.toString)
      }
    }
  }

  it should " be applyable to a key" in {
    val interp = new EnvironmentInterpreter()

    val result = for {
      _ <- interp("val m: Map (key: 3, value: 100, default: 0) = (0: 20, 1: 43, 2: 67)")
      r <- interp("val result: 100 = m 1")
    } yield {
      val correctCommand = EnvironmentCommand("result", IndexT(100), Index(43))
      assert(r == correctCommand.toString)
    }
    
    result match {
      case Failure(msg) => fail(msg)
      case Success(msg) => ()
    }
  }

  it should " be applyable to a key not specified and use the default" in {
    val interp = new EnvironmentInterpreter()

    val result = for {
      _ <- interp("val m: Map (key: 3, value: 100, default: 0) = (0: 20, 2: 67)")
      r <- interp("val result: 100 = m 1")
    } yield {
      val correctCommand = EnvironmentCommand("result", IndexT(100), Index(0))
      assert(r == correctCommand.toString)
    }
    
    result match {
      case Failure(msg) => fail(msg)
      case Success(msg) => ()
    }
  }

  it should " be creatable without explicit parameters" in {
    val interp = new EnvironmentInterpreter()

    // This is the first use of automatic
    val code = "val m: Type = Map (3, 100, 0)"

    interp(code) match {
      case Failure(msg) => fail(msg)
      case Success(msg) => {
        val correctCommand = EnvironmentCommand(
          "m",
          TypeT,
          MapType(Index(3), Index(100), Index(0))
        )
        assert(msg == correctCommand.toString)
      }
    }
  }

  "A struct " should " be created" in {
    val interp = new EnvironmentInterpreter()

    val result = for {
      r <- interp("val s: Struct (params: (a: 2, b: 3)) = (a:0, b:0)")
    } yield {
      val correctCommand = EnvironmentCommand(
        "s",
        StructT(Vector(("a",IndexT(2)), ("b",IndexT(3)))),
        StructInstance(Vector(("a",Index(0)), ("b",Index(0)))))
      assert(r == correctCommand.toString)
    }
    
    result match {
      case Failure(msg) => fail(msg)
      case Success(msg) => ()
    }
  }

  it should " be callable " in {
    val interp = new EnvironmentInterpreter()

    val result = for {
      _ <- interp("val s: Struct (params: (a: 2, b: 3)) = (a:0, b:1)")
      r <- interp("val q: 3 = s b")
    } yield {
      val correctCommand = EnvironmentCommand(
        "q",
        IndexT(3),
        Index(1)
      )
      assert(r == correctCommand.toString)
    }
    
    result match {
      case Failure(msg) => fail(msg)
      case Success(msg) => ()
    }
  }

  it should " be considered a type in a type input" in {
    val interp = new EnvironmentInterpreter()

    val result = for {
      r <- interp("val testType: Type = Map (key: Struct (params: (a: 3, b: 3)), value: 100, default: 0)")
    } yield {
      val correctCommand = EnvironmentCommand(
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
      assert(r == correctCommand.toString)
    }
    
    result match {
      case Failure(msg) => fail(msg)
      case Success(msg) => ()
    }
  }

  "Lambda expressions" should " be creatable as a type" in {
    val interp = new EnvironmentInterpreter()

    val result = for {
      r <- interp("val f: Type = ((a: 3) => 4)")
    } yield {
      val correctCommand = EnvironmentCommand(
        "f",
        TypeT,
        LambdaInstance(
          Vector("a" -> Index(3)),
          Index(4)
        )
      )
      assert(r == correctCommand.toString)
    }

    result match {
      case Failure(msg) => fail(msg)
      case Success(msg) => ()
    }
  }
}