package slang.parse

import slang.lex.{Token, TokenType}

class ListParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        val elements = parser.parseDelimited(_.expression(), TokenType.Comma, TokenType.RBracket, "list element", "comma", "]")
        Expr.List(elements)
    }
}
