package slang.parse;

import slang.lex.Token;

/**
 * Just do nothing and return null. Lowest precedence.
 */
public class SkipParselet implements PrefixParselet, InfixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        return null;
    }

    @Override
    public Expr parse(Parser parser, Expr left, Token token) {
        return null;
    }

    @Override
    public int getPrecedence() {
        return 0;
    }
}
