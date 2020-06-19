package slang.parse

import slang.lex.Token

class BinaryOpParselet(val precedence: Int, val isRight: Boolean) extends InfixParselet {
    override def parse(parser: Parser, left: Expr, token: Token): Expr = {
        // TODO(chris): Skip newlines here???
        val right = parser.expression(precedence - (if (isRight) 1 else 0))
        Expr.Binary(left, token, right);
    }

    override def getPrecedence: Int = precedence
}
