package slang.parse

import slang.lex.Token

/**
 * Just do nothing and return null. Lowest precedence.
 */
class SkipParselet extends PrefixParselet with InfixParselet {
    override def parse(parser: Parser, token: Token): Expr = null
    override def parse(parser: Parser, left: Expr, token: Token): Expr = null
    override def getPrecedence: Int = -1
}
