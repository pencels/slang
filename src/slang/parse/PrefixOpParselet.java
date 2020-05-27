package slang.parse;

import slang.lex.Token;

public class PrefixOpParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        Expr operand = parser.expression(Precedence.CALL);
        return new Expr.Prefix(token, operand);
    }
}
