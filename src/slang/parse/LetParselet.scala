package slang.parse

import slang.lex._
import slang.parse._
import slang.parse.Expr.Let

class LetParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        val pattern = parser.pattern()
        parser.expect(TokenType.Eq, "Expect `=` after let pattern.");
        val expr = parser.expression(Precedence.SEQUENCE)
        Let(pattern, expr)
    }
}
