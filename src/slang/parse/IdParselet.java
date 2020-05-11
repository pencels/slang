package slang.parse;

import slang.lex.Token;

public class IdParselet implements PrefixParselet {
    public Expr parse(Parser parser, Token token) {
        return new Expr.Id(token.lexeme);
    }
}
