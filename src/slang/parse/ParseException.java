package slang.parse;

import slang.lex.Token;

public class ParseException extends RuntimeException {
    public final Token token;

    ParseException(Token token, String reason) {
        super(reason);
        this.token = token;
    }

    @Override
    public String getMessage() {
        return String.format("[line %d, col %d] ", token.line, token.col) + super.getMessage();
    }
}
