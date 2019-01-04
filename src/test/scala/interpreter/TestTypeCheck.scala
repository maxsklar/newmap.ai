package ai.newmap.interpreter

import org.scalatest._
import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

class TestTypeCheck extends FlatSpec {
  "A number" should " be interpreted correctly" in {
  	TypeChecker(NaturalNumberParse(4)) match {
  	  case Success(NewMapObjectWithType(objectFound, typeInfo)) => {
  	  	assert(typeInfo == NewMapTypeInfo.init)
  	  	assert(objectFound == Index(4))
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A variable" should " be interpreted correctly" in {
  	TypeChecker(IdentifierParse("x")) match {
  	  case Success(result) => {
        assert(result.nObject == IdentifierInstance("x"))
        assert(result.nTypeInfo == NewMapTypeInfo.init)
      }
  	  case Failure(reason) => fail(reason)
  	}
  }

  "A keyword " should " be interpreted as that keyword" in {
  	TypeChecker(IdentifierParse("Type")) match {
  	  case Success(result) => {
  	  	assert(result.nTypeInfo == ExplicitlyTyped(TypeT))
  	  	assert(result.nObject == TypeType)
  	  }
  	  case Failure(reason) => fail(reason)
  	}
  }

  it should " be interpreted as an identifier if forced" in {
    TypeChecker(IdentifierParse("Type", true)) match {
      case Success(result) => {
        assert(result.nObject == IdentifierInstance("Type"))
        assert(result.nTypeInfo == NewMapTypeInfo.init)
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
      case Success(result) => {
        ()
      }
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
      case Success(result) => {
        result.nTypeInfo match {
          case ImplicitlyTyped(_) => fail(result + " should be explicit")
          case _ => ()
        }

        // TODO: check these are right
        //println(typeFound)
        //println(objectFound)
      }
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

  "A numerical type " should " beget lots of subtypes" in {
    val expression = LambdaParse(
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("a"), NaturalNumberParse(100)),
        BindingCommandItem(IdentifierParse("b"), IdentifierParse("a")),
        BindingCommandItem(IdentifierParse("c"), IdentifierParse("b")),
        BindingCommandItem(IdentifierParse("d"), IdentifierParse("c")),
        BindingCommandItem(IdentifierParse("e"), IdentifierParse("d")),
        BindingCommandItem(IdentifierParse("f"), IdentifierParse("e")),
        BindingCommandItem(IdentifierParse("g"), IdentifierParse("f")),
      )),
      IdentifierParse("g"),
    )

    TypeChecker(expression) match {
      case Success(result) => {
        result.nTypeInfo match {
          case ImplicitlyTyped(_) => fail(result + " should be explicit")
          case _ => ()
        }

        // TODO: Check these are right
        //println(typeFound)
        //println(objectFound)
      }
      case Failure(reason) => fail(reason)
    }
  }

  "A numerical type " should " get infinite subtyping" in {
    val expression = LambdaParse(
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("a"), NaturalNumberParse(3)),
        BindingCommandItem(IdentifierParse("b"), IdentifierParse("a")),
        BindingCommandItem(IdentifierParse("c"), IdentifierParse("b")),
        BindingCommandItem(IdentifierParse("d"), IdentifierParse("c")),
        BindingCommandItem(IdentifierParse("e"), IdentifierParse("d")),
        BindingCommandItem(IdentifierParse("f"), IdentifierParse("e")),
        BindingCommandItem(IdentifierParse("g"), IdentifierParse("f")),
      )),
      IdentifierParse("g"),
    )

    TypeChecker(expression) match {
      case Success(result) => ()
      case Failure(reason) => fail(reason)
    }
  }

  "The zero type " should " be recognized as a type even though no object exists" in {
    val expression = LambdaParse(
      CommandList(Vector(
        BindingCommandItem(IdentifierParse("a"), NaturalNumberParse(0)),
        BindingCommandItem(IdentifierParse("b"), IdentifierParse("a"))
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
      Vector(CommandList(Vector(
        BindingCommandItem(IdentifierParse("key"), NaturalNumberParse(2)),
        BindingCommandItem(IdentifierParse("value"), NaturalNumberParse(2)),
        BindingCommandItem(IdentifierParse("default"), NaturalNumberParse(0))
    ))))

    TypeChecker(booleanMap) match {
      case Success(result) => {
      	assert(result.nTypeInfo == ExplicitlyTyped(TypeT))
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
        assert(envCommands == Vector(FullEnvironmentCommand("x",IndexT(1),Index(0))))
      }
      case Failure(reason) => fail(reason)
    }
  }

  it should " be readable if there's an inferred type" in {
    val statement = InferredTypeStatementParse(ValStatement, IdentifierParse("x"), NaturalNumberParse(10))
    StatementInterpreter(statement, Environment.Base) match {
      case Success(envCommands) => {
        assert(envCommands == Vector(TypeInferredEnvironmentCommand("x",Index(10))))
      }
      case Failure(reason) => fail(reason)
    }
  }

  // TODO - this fails because structs fail!
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
        Vector(CommandList(Vector(
          BindingCommandItem(IdentifierParse("input"), IdentifierParse("x"))
        )))
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