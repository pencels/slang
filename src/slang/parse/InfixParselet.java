package slang.parse;

import slang.lex.Token;

public interface InfixParselet {
    Expr parse(Parser parser, Expr left, Token token);

    int getPrecedence();
}
