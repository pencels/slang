package slang.lex;

public class Token {
    public final TokenType type;
    public final String lexeme;
    public final Object value;
    public final int line, col;
    
    public Token(TokenType type, String lexeme, Object value, int line, int col) {
        this.type = type;
        this.lexeme = lexeme;
        this.value = value;
        this.line = line;
        this.col = col;
    }
    
    public String toString() {
        return String.format("slang.lexer.Token(%s, %s, %s, %d, %d)", type, lexeme, value, line, col);
    }
}
