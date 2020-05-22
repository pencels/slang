package slang.parse;

import slang.lex.Token;

public class PostfixOpParselet implements InfixParselet {
    @Override
    public Expr parse(Parser parser, Expr left, Token token) {
        return new Expr.Postfix(left, token);
    }

    @Override
    public int getPrecedence() {
        return Precedence.POSTFIX;
    }
}
