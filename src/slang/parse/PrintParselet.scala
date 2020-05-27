package slang.parse

import slang.lex._
import slang.parse._
import slang.parse.Expr.Print

class PrintParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        val expr = parser.expression(Precedence.SEQUENCE)
        Print(expr)
    }
}