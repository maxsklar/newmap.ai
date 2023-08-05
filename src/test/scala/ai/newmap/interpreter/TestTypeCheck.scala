package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestTypeCheck extends FlatSpec {
  def Index(i: Long): NewMapObject = NewMapObject(UIndex(i), CountT)
  val env = (new EnvironmentInterpreter(false)).env
  
  "A number" should " be interpreted correctly" in {
  	TypeChecker(NaturalNumberParse(4)) match {
      case Success(objectFound) => {
        assert(objectFound.nExpression == UIndex(4))
      }
      case Failure(reason) => fail(reason)
    }
  }

  "A variable" should " be interpreted correctly is forced as an identifier" in {
    TypeChecker(IdentifierParse("x", true)) match {
      case Success(result) => {
        assert(result.nExpression == UIdentifier("x"))
      }
  	  case Failure(reason) => fail(reason)
  	}
  }

  it should " fail if it's an unbound identifier identifier" in {
    TypeChecker(IdentifierParse("x", false)) match {
      case Success(result) => fail("Failed to detect unbound identifier x")
      case Failure(reason) => ()
    }
  }

  "A keyword " should " be interpreted as that keyword" in {
  	TypeChecker(IdentifierParse("Type")) match {
  	  case Success(nObject) => {
  	  	assert(nObject.nExpression == Environment.typeAsUntaggedObject(TypeT))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  it should " be interpreted as an identifier if forced" in {
    TypeChecker(IdentifierParse("Type", true)) match {
      case Success(result) => {
        assert(result.nExpression== UIdentifier("Type"))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A simple function expression " should " work" in {
    val expression = LambdaParse(
      NaturalNumberParse(12),
      NaturalNumberParse(100)
    )

    TypeChecker(expression) match {
      case Success(result) => ()
      case Failure(reason) => fail(reason)
    }
  } 

  it should " fail when the order is reversed" in {
    val expression = LambdaParse(
      LiteralListParse(Vector(
        KeyValueBinding(IdentifierParse("y"), IdentifierParse("x")),
        KeyValueBinding(IdentifierParse("x"), IdentifierParse("Type"))
      ), MapType),
      IdentifierParse("y"),
    )

    TypeChecker(expression) match {
      case Success(result) => {
        fail("It wrongfully succeeded with " + result.toString)
      }
      case Failure(reason) => {
        // TODO: turn reason into a case class, and check that the right error occurred
      }
    }
  }

   it should " fail if another type is extraced" in {
    val expression = LambdaParse(
      LiteralListParse(Vector(
        KeyValueBinding(IdentifierParse("x"), IdentifierParse("Type")),
        KeyValueBinding(IdentifierParse("y"), IdentifierParse("x")),
        KeyValueBinding(IdentifierParse("z"), IdentifierParse("y"))
      ), MapType),
      IdentifierParse("z"),
    )

    TypeChecker(expression) match {
      case Success(result) => {
        fail("It wrongfully succeeded with " + result)
      }
      case Failure(reason) => {
        //println(reason)
        // TODO: turn reason into a case class, and check that the right error occurred
      }
    }
  }

  "An index type " should " not allow subtypes" in {
    val expression = LambdaParse(
      LiteralListParse(Vector(
        KeyValueBinding(IdentifierParse("a"), NaturalNumberParse(100)),
        KeyValueBinding(IdentifierParse("b"), IdentifierParse("a"))
      ), MapType),
      IdentifierParse("g"),
    )

    TypeChecker(expression) match {
      case Success(result) => {
        fail("Should not be allowed but was")
      }
      case Failure(reason) => ()
    }
  }

  "The zero type " should " be recognized as a type even though no object exists" in {
    val expression = LambdaParse(
      NaturalNumberParse(0),
      NaturalNumberParse(4)
    )

    TypeChecker(expression) match {
      case Success(result) => ()
      case Failure(reason) => fail(reason)
    }
  }


  "A boolean map " should " be interpreted correctly" in {
  	val booleanMap = ApplyParse(
      IdentifierParse("Map"),
      LiteralListParse(Vector(KeyValueBinding(NaturalNumberParse(2), NaturalNumberParse(2))), MapType)
    )

    TypeChecker(booleanMap) match {
      case Success(result) => {
      	// TODO - the type checker should be figuring out a type here - so maybe we can see what it returns.
        ()
  	  }
  	  case Failure(reason) => fail(reason)
    }
  }

  // TODO - should be in a separate file
  "A statement " should " be readable" in {
    val statement = FullStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(1), NaturalNumberParse(0))
    StatementInterpreter(statement, env) match {
      case Success(command) => {
        assert(command == Environment.eCommand("x",NewMapObject(UIndex(0), IndexT(UIndex(1)))))
      }
      case Failure(reason) => fail(reason)
    }
  }

  it should " be readable if there's an inferred type" in {
    val statement = InferredTypeStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(10))
    StatementInterpreter(statement, env) match {
      case Success(command) => {
        assert(command == FullEnvironmentCommand("x", Index(10)))
      }
      case Failure(reason) => fail(reason)
    }
  }
}