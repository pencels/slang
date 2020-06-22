package slang.parse

import slang.lex.Token

class ParseException(val token: Token, val reason: String) extends RuntimeException(reason) {
    override def getMessage(): String = {
        String.format("[line %d, col %d] ", token.loc.line, token.loc.col) + super.getMessage();
    }
}
