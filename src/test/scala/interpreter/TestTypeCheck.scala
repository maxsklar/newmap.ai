package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestTypeCheck extends FlatSpec {
  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)
  
  "A number" should " be interpreted correctly" in {
  	TypeChecker(NaturalNumberParse(4)) match {
      case Success(objectFound) => {
        assert(objectFound == ObjectExpression(Index(4)))
      }
      case Failure(reason) => fail(reason)
    }
  }

  "A variable" should " be interpreted correctly is forced as an identifier" in {
    TypeChecker(IdentifierParse("x", true)) match {
      case Success(result) => {
        assert(result == ObjectExpression(NewMapO.identifier("x")))
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
  	  	assert(nObject == ObjectExpression(TypeT))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  it should " be interpreted as an identifier if forced" in {
    TypeChecker(IdentifierParse("Type", true)) match {
      case Success(result) => {
        assert(result == ObjectExpression(NewMapO.identifier("Type")))
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
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("y"), IdentifierParse("x")),
        BindingCommandItem(IdentifierParse("x"), IdentifierParse("Type"))
      )),
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
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("x"), IdentifierParse("Type")),
        BindingCommandItem(IdentifierParse("y"), IdentifierParse("x")),
        BindingCommandItem(IdentifierParse("z"), IdentifierParse("y"))
      )),
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
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("a"), NaturalNumberParse(100)),
        BindingCommandItem(IdentifierParse("b"), IdentifierParse("a"))
      )),
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
      CommandList(Vector(NaturalNumberParse(2), NaturalNumberParse(2)))
    )

    TypeChecker(booleanMap) match {
      case Success(result) => {
      	assert(RetrieveType(result, Environment.Base) == TypeT)
        //println(objectFound)

      	// TODO: what should we do with this?
      	//assert(objectFound == NewMapO.identifier("Type"))
  	  }
  	  case Failure(reason) => fail(reason)
    }
  }

  // TODO - should be in a separate file
  "A statement " should " be readable" in {
    val statement = FullStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(1), NaturalNumberParse(0))
    StatementInterpreter(statement, Environment.Base) match {
      case Success(result) => {
        assert(result.commands.length == 1)
        val com = result.commands(0)
        assert(com == Environment.eCommand("x",TaggedObject(UIndex(0), Index(1))))
      }
      case Failure(reason) => fail(reason)
    }
  }

  it should " be readable if there's an inferred type" in {
    val statement = InferredTypeStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(10))
    StatementInterpreter(statement, Environment.Base) match {
      case Success(result) => {
        assert(result.commands == Vector(FullEnvironmentCommand("x", Index(10))))
      }
      case Failure(reason) => fail(reason)
    }
  }
}