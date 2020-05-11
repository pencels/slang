package slang.parse;

import slang.lex.Token;

public class ParseException extends RuntimeException {
    public final Token token;
    ParseException(Token token, String reason) {
        super(reason);
        this.token = token;
    }
}
