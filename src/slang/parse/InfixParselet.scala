package slang.parse

import slang.lex.Token

trait InfixParselet {
    def parse(parser: Parser, left: Expr, token: Token): Expr
    def getPrecedence: Int
}
