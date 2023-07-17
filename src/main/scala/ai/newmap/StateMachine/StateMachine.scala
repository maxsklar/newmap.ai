package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Parser.StateMachineConfig.ParserConfig
import ai.newmap.model.EnvStatementParse
import ai.newmap.util.{Failure, Success, Outcome};

class StateMachine (val depth: Integer = 0){
  private val MAX_DEPTH = 5

  def run(tokens: Seq[Lexer.Token]): Outcome[EnvStatementParse, String] = {
    if(depth > MAX_DEPTH) {
      Failure("Max Depth Reached")
    }
    
    val parserConfig = new ParserConfig()
    var curState = parserConfig.initState
    //println("Current State: " + curState.name)
    tokens.foreach(token => {
      //println("Found Token: " + token)
      curState = curState.changeState(token, tokens)
      //println("Current State: " + curState.name)
    })

    curState = curState.changeState(null, tokens)

    for {
      _ <- Outcome.failWhen(!curState.isEndState, "Unimplemented")
    } yield {
      curState.generateParseTree
    }
  }
}