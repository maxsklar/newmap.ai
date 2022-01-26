package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestTypeCheck extends FlatSpec {
  "A number" should " be interpreted correctly" in {
  	TypeChecker(NaturalNumberParse(4)) match {
      case Success(objectFound) => {
        assert(objectFound == Index(4))
      }
      case Failure(reason) => fail(reason)
    }
  }

  "A variable" should " be interpreted correctly" in {
    TypeChecker(IdentifierParse("x")) match {
      case Success(result) => {
        assert(result == IdentifierInstance("x"))
      }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A keyword " should " be interpreted as that keyword" in {
  	TypeChecker(IdentifierParse("Type")) match {
  	  case Success(nObject) => {
  	  	assert(nObject == TypeT(0))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  it should " be interpreted as an identifier if forced" in {
    TypeChecker(IdentifierParse("Type", true)) match {
      case Success(result) => {
        assert(result == IdentifierInstance("Type"))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A simple lambda expression " should " work" in {
    val expression = LambdaParse(
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("x"), NaturalNumberParse(12))
      )),
      IdentifierParse("x")
    )

    TypeChecker(expression) match {
      case Success(result) => ()
      case Failure(reason) => fail(reason)
    }
  } 

  "If x is declared as a type, y " should " be able to be declared as type x" in {
    val expression = LambdaParse(
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("x"), IdentifierParse("Type")),
        BindingCommandItem(IdentifierParse("y"), IdentifierParse("x"))
      )),
      IdentifierParse("y"),
    )

    TypeChecker(expression) match {
      case Success(result) => ()
        // TODO: check these are right
        //println(objectFound)
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
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("a"), NaturalNumberParse(0))
      )),
      IdentifierParse("a"),
    )

    TypeChecker(expression) match {
      case Success(result) => ()
      case Failure(reason) => fail(reason)
    }
  }


  "A boolean map " should " be interpreted correctly" in {
  	val booleanMap = ApplyParse(
      IdentifierParse("Map"),
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("key"), NaturalNumberParse(2)),
        BindingCommandItem(IdentifierParse("value"), NaturalNumberParse(2))
      ))
    )

    TypeChecker(booleanMap) match {
      case Success(result) => {
      	assert(RetrieveType(result) == TypeT(0))
        //println(objectFound)

      	// TODO: what should we do with this?
      	//assert(objectFound == IdentifierInstance("Type"))
  	  }
  	  case Failure(reason) => fail(reason)
    }
  }

  // TODO - should be in a separate file
  "A statement " should " be readable" in {
    val statement = FullStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(1), NaturalNumberParse(0))
    StatementInterpreter(statement, Environment.Base) match {
      case Success(envCommands) => {
        assert(envCommands.length == 1)
        val com = envCommands(0)
        assert(com == Environment.eCommand("x",Index(0)))
      }
      case Failure(reason) => fail(reason)
    }
  }

  it should " be readable if there's an inferred type" in {
    val statement = InferredTypeStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(10))
    StatementInterpreter(statement, Environment.Base) match {
      case Success(envCommands) => {
        assert(envCommands == Vector(FullEnvironmentCommand("x", Index(10))))
      }
      case Failure(reason) => fail(reason)
    }
  }

  "Applying a Function in a lambda " should " work if the types are right" in {
    val expression = LambdaParse(
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("inputType"), IdentifierParse("Type")),
        BindingCommandItem(IdentifierParse("outputType"), IdentifierParse("Type")),
        BindingCommandItem(IdentifierParse("f"), LambdaParse(
          CommandList(Vector(
            BindingCommandItem(IdentifierParse("input"), IdentifierParse("inputType"))
          )),
          IdentifierParse("outputType")
        )),
        BindingCommandItem(IdentifierParse("x"), IdentifierParse("inputType"))
      )),
      ApplyParse(
        IdentifierParse("f"),
        CommandList(Vector(
          BindingCommandItem(IdentifierParse("input"), IdentifierParse("x"))
        ))
      )
    )

    TypeChecker(expression) match {
      case Success(result) => {
        ()
      }
      case Failure(reason) => {
        fail(reason)
      }
    }
  }
}