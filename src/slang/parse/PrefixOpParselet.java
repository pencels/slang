package slang.parse;

import slang.lex.Token;

public class PrefixOpParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        Expr operand = parser.expression(0);
        return new Expr.Unary(token, operand);
    }
}
