package slang.parse;

import slang.lex.Token;
import slang.lex.TokenType;

import java.util.ArrayList;
import java.util.List;

public class CallParselet implements InfixParselet {
    @Override
    public Expr parse(Parser parser, Expr left, Token token) {
        List<Expr> args = new ArrayList<Expr>();

        // There may not be any arguments.
        do {
            args.add(parser.expression(getPrecedence()));
        } while (parser.getPrecedence() >= this.getPrecedence());

        return new Expr.Call(left, args);
    }

    @Override
    public int getPrecedence() {
        return Precedence.CALL;
    }
}
