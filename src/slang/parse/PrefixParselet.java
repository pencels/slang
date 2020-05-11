package slang.parse;

import slang.lex.Token;

public interface PrefixParselet {
    Expr parse(Parser parser, Token token);
}
