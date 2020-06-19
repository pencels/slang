package slang.parse

import slang.lex.Token

class IdParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = Expr.Id(token.lexeme)
}
