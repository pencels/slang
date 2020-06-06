package slang.lex;

import slang.Slang;

import java.util.*;

import static slang.lex.TokenType.*;

public class Lexer {
    private static final Map<String, TokenType> KEYWORDS = new HashMap<>();

    // Number of interpolation scopes that have been opened.
    //private Stack<Integer> interpScopes = new Stack<>();

    private Stack<Integer> parens = new Stack<>();

    {
        parens.push(0);
    }

    private int numInterpolationScopes = 0;

    static {
        KEYWORDS.put("let", LET);
        KEYWORDS.put("obj", OBJ);
        KEYWORDS.put("print", PRINT);
        KEYWORDS.put("nothing", NOTHING);
        KEYWORDS.put("true", TRUE);
        KEYWORDS.put("false", FALSE);
    }

    private final String source;
    private int start, current;
    private int line = 1, col;
    private final List<Token> tokens = new ArrayList<>();

    public Lexer(String source) {
        this.source = source;
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    private char advance() {
        col++;
        current++;
        return source.charAt(current - 1);
    }

    private char peek() {
        if (isAtEnd()) {
            return '\0';
        }
        return source.charAt(current);
    }

    private char peekNext() {
        if (current + 1 >= source.length()) {
            return '\0';
        }
        return source.charAt(current + 1);
    }

    private boolean match(char expected) {
        if (isAtEnd()) {
            return false;
        }
        if (source.charAt(current) != expected) {
            return false;
        }

        advance();
        return true;
    }

    private void newline() {
        line++;
        col = 1;
    }

    private void nextToken() {
        start = current;

        char c = advance();

        switch (c) {
            case '(': {
                parens.set(parens.size() - 1, parens.peek() + 1);
                addToken(LEFT_PAREN);
                break;
            }
            case ')': {
                // This might be closing a string interpolation.
                if (parens.peek() == 0 && numInterpolationScopes > 0) {
                    addToken(STRING_INTERP_END); // Note the end of this interpolation.
                    string(); // Read the rest of the stream as a string.
                    numInterpolationScopes--; // Closed a scope.
                    parens.pop();
                } else {
                    parens.set(parens.size() - 1, parens.peek() - 1);
                    addToken(RIGHT_PAREN);
                }
                break;
            }
            case '{':
                addToken(LEFT_CURLY);
                break;
            case '}':
                addToken(RIGHT_CURLY);
                break;
            case '[':
                addToken(LEFT_BRACKET);
                break;
            case ']':
                addToken(RIGHT_BRACKET);
                break;
            case ';':
                addToken(SEMI);
                break;
            case '!':
                if (match('=')) {
                    addToken(NE);
                } else {
                    addToken(BANG);
                }
                break;
            case '&':
                addToken(AMPERSAND);
                break;
            case '|':
                addToken(PIPE);
                break;
            case '@':
                addToken(AT);
                break;
            case ',':
                addToken(COMMA);
                break;
            case '<':
                if (match('=')) {
                    addToken(LE);
                } else {
                    addToken(LT);
                }
                break;
            case '>':
                if (match('=')) {
                    addToken(GE);
                } else {
                    addToken(GT);
                }
                break;
            case '.':
                if (match('.')) {
                    addToken(DOTDOT);
                } else {
                    addToken(DOT);
                }
                break;
            case '-':
                if (match('>')) {
                    addToken(ARROW);
                } else if (match('-')) {
                    // It's a comment!
                    while (peek() != '\n' && !isAtEnd()) advance(); // Comments go until the end of the line.
                } else {
                    addToken(MINUS);
                }
                break;
            case '+':
                addToken(PLUS);
                break;
            case '*':
                addToken(STAR);
                break;
            case '/':
                addToken(SLASH);
                break;
            case '=':
                if (match('=')) {
                    addToken(EQEQ);
                } else {
                    addToken(EQ);
                }
                break;
            case '"':
                string();
                break;
            case ':':
                atom();
                break;

            case '\n':
                addToken(NEWLINE);
                newline();
                break;
            case ' ':
            case '\t':
                break; // Skip space

            default:
                if (isAlpha(c)) {
                    identifier();
                } else if (isNum(c)) {
                    number();
                } else {
                    Slang.lexerError(c);
                }
        }

    }

    private void atom() {
        while (isAtom(peek())) {
            advance();
        }

        String value = source.substring(start + 1, current);
        addToken(ATOM, value);
    }

    private void identifier() {
        while (isAlpha(peek()) || isNum(peek())) {
            advance();
        }

        // Check for keywords.
        String text = source.substring(start, current);
        if (KEYWORDS.containsKey(text)) {
            addToken(KEYWORDS.get(text));
            return;
        }

        addToken(IDENTIFIER);
    }

    private void number() {
        while (isNum(peek())) {
            advance();
        }

        // Check for fractional part
        if (peek() == '.' && isNum(peekNext())) {
            advance();
            while (isNum(peek())) {
                advance();
            }
        }

        addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private void string() {
        while (peek() != '"') {
            // Catch string interpolation.
            if (peek() == '\\' && peekNext() == '(') {
                addToken(STRING_INTERP_START, source.substring(start + 1, current));

                // Skip "\(".
                advance();
                advance();

                // Opened an interpolation scope.
                parens.push(0);
                numInterpolationScopes++;
                return; // Cut it short and keep tokenizing.
            }
            advance();
        }

        if (isAtEnd()) {
            // TODO(chris): error EOF while parsing string literal
            return;
        }

        advance(); // Eat the ".

        String value = source.substring(start + 1, current - 1);
        addToken(STRING, value);
    }

    private boolean isAlpha(char c) {
        return 'A' <= c && c <= 'Z'
                || 'a' <= c && c <= 'z'
                || c == '_';
    }

    private boolean isNum(char c) {
        return '0' <= c && c <= '9';
    }

    private boolean isAtom(char c) {
        return isAlpha(c) || isNum(c) || c == '!' || c == '?';
    }

    private void addToken(TokenType type, Object value) {
        String lexeme = source.substring(start, current);
        tokens.add(new Token(type, lexeme, value, line, col));
    }

    private void addToken(TokenType type) {
        addToken(type, null);
    }

    public List<Token> lex() {
        while (!isAtEnd()) {
            nextToken();
        }

        tokens.add(new Token(EOF, "", null, line, col));

        return tokens;
    }
}
