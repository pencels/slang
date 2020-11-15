package slang.parse

import slang.lex.Token

case class ParserException(token: Token, message: String)
    extends Exception(message)
