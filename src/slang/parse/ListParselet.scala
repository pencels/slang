package slang.parse

import scala.collection._

import slang.lex.Token
import slang.lex.TokenType

class ListParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        val elements = parser.parseDelimited(_.expression(), TokenType.Comma, TokenType.RBracket, "list element", "comma", "]")
        Expr.SlangList(elements)
    }
}
