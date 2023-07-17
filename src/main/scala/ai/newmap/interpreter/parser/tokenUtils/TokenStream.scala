package ai.newmap.interpreter.parser.tokenUtils

import ai.newmap.interpreter.Lexer

class TokenStream(var tokens: Seq[Lexer.Token], removeTokens: Boolean) {

  if(removeTokens) tokens = tokens.filter(!isComment(_))

  def isEmpty: Boolean ={
    tokens.isEmpty
  }
  def getReader: TokenReader = {
    new TokenReader(tokens)
  }
  private def isComment(token: Lexer.Token): Boolean = token match {
    case Lexer.Comment(_) => true
    case _ => false
  }
}
