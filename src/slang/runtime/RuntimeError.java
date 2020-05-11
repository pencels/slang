package slang.runtime;

import slang.lex.Token;

public class RuntimeError extends RuntimeException {
    final Token token;

    public RuntimeError(Token token, String reason) {
        super(reason);
        this.token = token;
    }
}
