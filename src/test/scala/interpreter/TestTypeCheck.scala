package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestTypeCheck extends FlatSpec {
  "A number" should " be interpreted correctly" in {
  	TypeChecker(NaturalNumberParse(4)) match {
  	  case Success(TypeChecked(typeFound, objectFound)) => {
  	  	assert(typeFound == IndexT(5))
  	  	assert(objectFound == Index(4))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A variable" should " be interpreted correctly" in {
  	TypeChecker(IdentifierParse("x")) match {
  	  case Success(TypeChecked(typeFound, objectFound)) => {
        assert(typeFound == IdentifierT)
        assert(objectFound == IdentifierInstance("x"))
      }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A keyword " should " be interpreted as that keyword" in {
  	TypeChecker(IdentifierParse("Type")) match {
  	  case Success(TypeChecked(typeFound, objectFound)) => {
  	  	assert(typeFound == TypeT)
  	  	assert(objectFound == TypeType)
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  it should " be interpreted as an identifier if forced" in {
    TypeChecker(IdentifierParse("Type", true)) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
      	assert(typeFound == IdentifierT)
      	assert(objectFound == IdentifierInstance("Type"))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A simple lambda expression " should " work" in {
    val expression = LambdaParse(
      Vector(IdentifierParse("x") -> IdentifierParse("Object")),
      IdentifierParse("x")
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        ()
      }
      case Failure(reason) => fail(reason)
    }
  } 

  "If x is declared as a type, y " should " be able to be declared as type x" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("x") -> IdentifierParse("Type"),
        IdentifierParse("y") -> IdentifierParse("x")
      ),
      IdentifierParse("y"),
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        // TODO: check these are right
        //println(typeFound)
        //println(objectFound)
      }
      case Failure(reason) => fail(reason)
    }
  }

  it should " fail when the order is reversed" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("y") -> IdentifierParse("x"),
        IdentifierParse("x") -> IdentifierParse("Type")
      ),
      IdentifierParse("y"),
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        fail("It wrongfully succeeded, " + objectFound.toString + ": " + typeFound.toString)
      }
      case Failure(reason) => {
        // TODO: turn reason into a case class, and check that the right error occurred
      }
    }
  }

   it should " fail if another type is extraced" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("x") -> IdentifierParse("Type"),
        IdentifierParse("y") -> IdentifierParse("x"),
        IdentifierParse("z") -> IdentifierParse("y")
      ),
      IdentifierParse("z"),
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        fail("It wrongfully succeeded, " + objectFound.toString + ": " + typeFound.toString)
      }
      case Failure(reason) => {
        //println(reason)
        // TODO: turn reason into a case class, and check that the right error occurred
      }
    }
  }

  "A numerical type " should " beget lots of subtypes" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("a") -> NaturalNumberParse(100),
        IdentifierParse("b") -> IdentifierParse("a"),
        IdentifierParse("c") -> IdentifierParse("b"),
        IdentifierParse("d") -> IdentifierParse("c"),
        IdentifierParse("e") -> IdentifierParse("d"),
        IdentifierParse("f") -> IdentifierParse("e"),
        IdentifierParse("g") -> IdentifierParse("f"),
      ),
      IdentifierParse("g"),
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        // TODO: Check these are right
        //println(typeFound)
        //println(objectFound)
      }
      case Failure(reason) => fail(reason)
    }
  }

  "A numerical type " should " not get infinite subtyping" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("a") -> NaturalNumberParse(3),
        IdentifierParse("b") -> IdentifierParse("a"),
        IdentifierParse("c") -> IdentifierParse("b"),
        IdentifierParse("d") -> IdentifierParse("c"),
        IdentifierParse("e") -> IdentifierParse("d"),
        IdentifierParse("f") -> IdentifierParse("e"),
        IdentifierParse("g") -> IdentifierParse("f"),
      ),
      IdentifierParse("g"),
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        fail("It wrongfully succeeded, " + objectFound.toString + ": " + typeFound.toString)
      }
      case Failure(reason) => {
        //println(reason)
        // TODO: turn reason into a case class, and check that the right error occurred
      }
    }
  }

  "The zero type " should " be recognized as a type even though no object exists" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("a") -> NaturalNumberParse(0),
        IdentifierParse("b") -> IdentifierParse("a")
      ),
      IdentifierParse("a"),
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        fail("It wrongfully succeeded, " + objectFound.toString + ": " + typeFound.toString)
      }
      case Failure(reason) => {
        //println(reason)
        // TODO: turn reason into a case class, and check that the right error occurred
      }
    }
  }


  "A boolean map " should " be interpreted correctly" in {
  	val booleanMap = ApplyParse(
      IdentifierParse("Map"),
      Enclosure(Paren, Vector(
        IdentifierParse("key") -> NaturalNumberParse(2),
        IdentifierParse("value") -> NaturalNumberParse(2),
        IdentifierParse("default") -> NaturalNumberParse(0)
    )))

    TypeChecker(booleanMap) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
      	assert(typeFound == TypeT)
        //println(objectFound)

      	// TODO: what should we do with this?
      	//assert(objectFound == IdentifierInstance("Type"))
  	  }
  	  case Failure(reason) => fail(reason)
    }
  }

  // TODO - should be in a separate file
  "A statement " should " be readable" in {
    val statement = StatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(1), NaturalNumberParse(0))
    StatementInterpreter(statement, Environment.Base) match {
      case Success(envCommands) => {
        assert(envCommands == Vector(EnvironmentCommand("x",IndexT(1),Index(0))))
      }
      case Failure(reason) => fail(reason)
    }
  }

  // TODO - this fails because structs fail!
  "Applying a Function in a lambda " should " work if the types are right" in {
    val expression = LambdaParse(
      Vector(
        IdentifierParse("inputType") -> IdentifierParse("Type"),
        IdentifierParse("outputType") -> IdentifierParse("Type"),
        IdentifierParse("f") -> LambdaParse(
          Vector(IdentifierParse("input") -> IdentifierParse("inputType")),
          IdentifierParse("outputType")
        ),
        IdentifierParse("x") -> IdentifierParse("inputType")
      ),
      ApplyParse(
        IdentifierParse("f"),
        Enclosure(
          Paren,
          Vector(IdentifierParse("input") -> IdentifierParse("x"))
        )
      )
    )

    TypeChecker(expression) match {
      case Success(TypeChecked(typeFound, objectFound)) => {
        ()
      }
      case Failure(reason) => {
        fail(reason)
      }
    }
  }
}