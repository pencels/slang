package slang.lex;

public class Token {
    public final TokenType opType;
    public final String lexeme;
    public final Object value;
    public final int line, col;

    public Token(TokenType opType, String lexeme, Object value, int line, int col) {
        this.opType = opType;
        this.lexeme = lexeme;
        this.value = value;
        this.line = line;
        this.col = col;
    }

    public String toString() {
        return String.format("slang.lexer.Token(%s, %s, %s, %d, %d)", opType, lexeme, value, line, col);
    }
}
