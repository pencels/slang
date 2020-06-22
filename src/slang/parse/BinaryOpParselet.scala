package slang.parse

import slang.lex.Token

class BinaryOpParselet(val precedence: Int, val isRight: Boolean) extends InfixParselet {
    override def parse(parser: Parser, left: Expr, token: Token): Expr = {
        parser.skipNewlines
        val right = parser.expression(precedence - (if (isRight) 1 else 0))
        Expr.Binary(left, token, right);
    }

    override def getPrecedence: Int = precedence
}
