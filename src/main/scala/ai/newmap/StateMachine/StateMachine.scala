package ai.newmap.StateMachine

import ai.newmap.interpreter.Lexer
import ai.newmap.interpreter.Parser.StateMachineConfig.ParserConfig
import ai.newmap.util.Outcome;

class StateMachine (val depth: Integer = 0, val expectation: Class[_] = null){
  private val MAX_DEPTH = 5

  def run(tokens: Seq[Lexer.Token]): Outcome[Any, String] = {
    if(depth > MAX_DEPTH){
      ai.newmap.util.Failure("Max Depth Reached")
    }
    val parserConfig = new ParserConfig()
    var curState = parserConfig.initState
    tokens.foreach(token => {
      curState = curState.changeState(token, tokens)
    })
    curState = curState.changeState(null, tokens)
    if (curState.endState) {
      ai.newmap.util.Success(curState.generateParseTree)
    }
    ai.newmap.util.Failure("Unimplemented")
  }

}