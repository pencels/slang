package slang.parse

import slang.lex.Token

class PostfixOpParselet extends InfixParselet {
    override def parse(parser: Parser, left: Expr, token: Token): Expr = Expr.Postfix(left, token) 
    override def getPrecedence: Int = Precedence.POSTFIX
}
