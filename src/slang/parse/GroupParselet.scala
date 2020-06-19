package slang.parse

import slang.lex.Token
import slang.lex.TokenType

class GroupParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        parser.skipNewlines
        val exprs = parser.parseExprLines(TokenType.RParen)
        parser.consume(TokenType.RParen, "Expect `)` to close group or sequence of expressions.")

        exprs match {
            // If there's one expr, treat as a simple group.
            case List(expr) => Expr.Grouping(expr)

            // If there are 0 or multiple exprs, treat like a sequence of exprs.
            case _ => Expr.Grouping(Expr.Seq(exprs));
        }
    }
}
