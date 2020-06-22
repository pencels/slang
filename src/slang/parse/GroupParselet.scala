package slang.parse

import slang.lex.Token
import slang.lex.TokenType

class GroupParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        parser.skipNewlines
        val exprs = parser.multilineSequenceExpr(TokenType.RParen, "')'")
        Expr.Grouping(exprs)
    }
}
