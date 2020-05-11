package slang.parse;

import slang.lex.Token;
import slang.lex.TokenType;

public class GroupParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        parser.skipNewlines();
        Expr expr = parser.expression();
        parser.skipNewlines();
        parser.consume(TokenType.RIGHT_PAREN, "Expect `)` to close group.");
        return new Expr.Grouping(expr);
    }
}
