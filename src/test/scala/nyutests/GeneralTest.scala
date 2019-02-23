package ai.newmap.interpreter

import org.scalatest.FunSuite
import ai.newmap.util.Success
import ai.newmap.interpreter._
import ai.newmap.interpreter.Lexer._

import ai.newmap.model._

class GeneralTest extends FunSuite {
    // lexer test
    test("A map statement should be lexed correctly"){
		val code = "val a : Map(3, 100, 0) = (0:20, 1:43, 2:67)"
		assert(Lexer(code) == Success(List(
		    Identifier("val"),
		    Identifier("a"),
		    Colon(),
		    Identifier("Map"),
		    Enc(Paren, isOpen = true),
		    Number(3),
		    Comma(),
		    Number(100),
		    Comma(),
		    Number(0),
		    Enc(Paren, isOpen = false),
		    Equals(),
	            Enc(Paren, isOpen = true),
		    Number(0),
		    Colon(),
		    Number(20),
		    Comma(),
		    Number(1),
		    Colon(),
		    Number(43),
		    Comma(),
		    Number(2),
		    Colon(),
		    Number(67),
		    Enc(Paren, isOpen = false)
		)))
    }
    // parser test
    test("A List of tokens of map statement should be parsed correctly"){
		val tokens = List(
		 	Identifier("val"), 
		 	Identifier("a"), 
		 	Colon(), 
		 	Identifier("Map"), 
		 	Enc(Paren,true), 
		 	Number(3), 
		 	Comma(), 
		 	Number(100), 
		 	Comma(), 
		 	Number(0), 
		 	Enc(Paren,false), 
		 	Equals(), 
		 	Enc(Paren,true), 
		 	Number(0), 
		 	Colon(), 
		 	Number(20), 
		 	Comma(), 
		 	Number(1), 
		 	Colon(), 
		 	Number(43), 
		 	Comma(), 
		 	Number(2), 
		 	Colon(), 
		 	Number(67), 
		 	Enc(Paren,false)
		)
	    assert(NewMapParser.statementParse(tokens) == 
	    	Success(
		        FullStatementParse(ValStatement,
		        	IdentifierParse("a",false),
		        	ApplyParse(IdentifierParse("Map",false),
		        		Vector(CommandList(Vector(NaturalNumberParse(3), 
		        			NaturalNumberParse(100), 
		        			NaturalNumberParse(0))))
		        	),
		        	CommandList(Vector(BindingCommandItem(NaturalNumberParse(0),
		        		NaturalNumberParse(20)), 
		        		BindingCommandItem(NaturalNumberParse(1),
		        			NaturalNumberParse(43)), 
		        		 	BindingCommandItem(NaturalNumberParse(2),NaturalNumberParse(67)))
		        	)
		        )
			)
	    )
    }
	// lexer test2
    test("A count statement should be lexed correctly"){
		val code = "val a : Count = 5"
		assert(Lexer(code) == Success(List(
		    Identifier("val"), 
    		Identifier("a"), 
    		Colon(), 
    		Identifier("Count"), 
    		Equals(), 
    		Number(5)
		)))
    }
    // parser test 2 
    test("A List of tokens of count statement should be parsed correctly"){
    	val tokens = List(
    		Identifier("val"), 
    		Identifier("a"), 
    		Colon(), 
    		Identifier("Count"), 
    		Equals(), 
    		Number(5)
    	)
		assert(NewMapParser.statementParse(tokens) == 
			Success(
				FullStatementParse(ValStatement,
					IdentifierParse("a",false),
					IdentifierParse("Count",false),
					NaturalNumberParse(5)
				)
			)
    	)
    }
}

