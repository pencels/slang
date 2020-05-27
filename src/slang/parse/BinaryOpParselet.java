package slang.parse;

import slang.lex.Token;

public class BinaryOpParselet implements InfixParselet {
    private int precedence;
    private boolean isRight;

    BinaryOpParselet(int precedence, boolean isRight) {
        this.precedence = precedence;
        this.isRight = isRight;
    }

    @Override
    public Expr parse(Parser parser, Expr left, Token token) {
        // TODO(chris): Skip newlines here???
        Expr right = parser.expression(precedence - (isRight ? 1 : 0));
        return new Expr.Binary(left, token, right);
    }

    @Override
    public int getPrecedence() {
        return precedence;
    }
}
