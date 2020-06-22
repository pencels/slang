package slang.parse

import slang.lex.Token

trait PrefixParselet {
    def parse(parser: Parser, token: Token): Expr
}
