package slang.parse

import scala.collection._

import slang.lex.Token
import slang.lex.TokenType

class ListParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        val elements = new mutable.ListBuffer[Expr]

        while (!parser.check(TokenType.RBracket)) {
            parser.skipNewlines // Allow newlines before elements.
            elements.addOne(parser.expression())
            parser.skipNewlines // Allow newlines after elements.
            parser.expect(TokenType.Comma, TokenType.Newline)
        }

        parser.consume(TokenType.RBracket, "Expect ']' to end list.");
        Expr.SlangList(elements.toList)
    }
}
