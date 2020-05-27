package slang.parse;

import slang.lex.Token;
import slang.lex.TokenType;
import slang.runtime.Value;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static slang.lex.TokenType.*;

public class Parser {
    private final Map<TokenType, PrefixParselet> prefixParselets = new HashMap<>();
    private final Map<TokenType, InfixParselet> infixParselets = new HashMap<>();

    private final List<Token> tokens;
    private int current;

    private int bookmark; // For backtracking on a poor parsing decision.

    {
        registerAtom(IDENTIFIER, new IdParselet());
        registerAtom(LEFT_PAREN, new GroupParselet());
        registerAtom(NUMBER, new LiteralParselet());
        registerAtom(STRING, new LiteralParselet());
        registerAtom(STRING_INTERP_START, new StringInterpolationParselet());
        registerAtom(ATOM, new LiteralParselet());
        registerAtom(LEFT_CURLY, new BlockParselet());
        registerAtom(LEFT_BRACKET, new ListParselet());
        registerAtom(NOTHING, new LiteralParselet());
        registerAtom(TRUE, new LiteralParselet());
        registerAtom(FALSE, new LiteralParselet());

        prefix(MINUS);
        prefix(PLUS);
        prefix(BANG);

        binary(PLUS, Precedence.SUM, false);
        binary(MINUS, Precedence.SUM, false);
        binary(STAR, Precedence.PRODUCT, false);
        binary(SLASH, Precedence.PRODUCT, false);
        binary(DOT, Precedence.CALL, false);
        binary(DOTDOT, Precedence.CONDITIONAL, false);
        binary(EQEQ, Precedence.CONDITIONAL, false);
        binary(NE, Precedence.CONDITIONAL, false);
        binary(LT, Precedence.CONDITIONAL, false);
        binary(LE, Precedence.CONDITIONAL, false);
        binary(GT, Precedence.CONDITIONAL, false);
        binary(GE, Precedence.CONDITIONAL, false);
        binary(AT, Precedence.APPLY, false);
        binary(SEMI, Precedence.SEQUENCE, false);

        postfix(BANG);

        register(PRINT, new PrintParselet());
        register(LET, new LetParselet());

        register(NEWLINE, (PrefixParselet) new SkipParselet());
        register(NEWLINE, (InfixParselet) new SkipParselet());
    }

    public void register(TokenType token, PrefixParselet parselet) {
        prefixParselets.put(token, parselet);
    }

    public void register(TokenType token, InfixParselet parselet) {
        infixParselets.put(token, parselet);
    }

    private void registerAtom(TokenType token, PrefixParselet parselet) {
        register(token, parselet);
        register(token, new CallParselet());
    }

    public Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    private void prefix(TokenType token) {
        register(token, new PrefixOpParselet());
    }

    private void binary(TokenType token, int precedence, boolean isRight) {
        register(token, new BinaryOpParselet(precedence, isRight));
    }

    private void postfix(TokenType token) {
        register(token, new PostfixOpParselet());
    }

    public List<Expr> parse() {
        List<Expr> exprs = new ArrayList<>();
        while (!isAtEnd()) {
            skipNewlines(); // Consume any empty lines before trying to parse an expr.
            if (isAtEnd()) break;
            exprs.add(expression());
            if (isAtEnd()) break;
            if (!match(NEWLINE)) {
                throw new ParseException(peek(), "Expect newline after expr.");
            }
        }
        return exprs;
    }

    void skipNewlines() {
        while (match(NEWLINE)) ;
    }

    Token peek() {
        return tokens.get(current);
    }

    Token previous() {
        return tokens.get(current - 1);
    }

    Token advance() {
        if (!isAtEnd()) current++;
        return previous();
    }

    boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    boolean check(TokenType type) {
        if (isAtEnd()) return false;
        return peek().opType == type;
    }

    boolean isAtEnd() {
        return peek().opType == TokenType.EOF;
    }

    Token consume(TokenType expected, String errorMessage) {
        Token token = peek();
        if (token.opType != expected) {
            throw new ParseException(previous(), errorMessage);
        }

        return advance();
    }

    Expr expression(int precedence) {
        // Try with pattern.
        setBookmark();
        try {
            Pattern pat = pattern();
            consume(EQ, "Expect '=' after pattern.");
            Expr right = expression();
            return new Expr.Assign(pat, right);
        } catch (ParseException e) {
            goToBookmark();
        }

        Token token = advance();
        PrefixParselet prefix = prefixParselets.get(token.opType);

        if (prefix == null) throw new ParseException(token, "Could not parse token: " + token + ".");

        Expr left = prefix.parse(this, token);

        while (precedence < getPrecedence()) {
            TokenType nextType = peek().opType;
            if (!(infixParselets.get(nextType) instanceof CallParselet)) {
                token = advance();
            }
            InfixParselet infix = infixParselets.get(nextType);
            left = infix.parse(this, left, token);
        }

        return left;
    }

    Expr expression() {
        return expression(0);
    }

    int getPrecedence() {
        InfixParselet parser = infixParselets.get(peek().opType);
        if (parser != null) return parser.getPrecedence();
        return 0;
    }

    void setBookmark() {
        bookmark = current;
    }

    void goToBookmark() {
        current = bookmark;
    }

    public Pattern pattern() {
        if (match(NOTHING, NUMBER, STRING, ATOM, TRUE, FALSE)) {
            Token lit = previous();
            Value value = LiteralParselet.valueFromToken(lit);
            return new Pattern.Literal(value);
        }
        if (match(LEFT_BRACKET)) return listPattern();
        if (match(LEFT_CURLY)) return lazyPattern();
        if (match(IDENTIFIER)) {
            Token id = previous();
            if ("_".equals(id.lexeme)) return new Pattern.Ignore(id);
            if (match(DOTDOT)) {
                return new Pattern.Spread(id);
            } else {
                return new Pattern.Id(id);
            }
        }

        throw new ParseException(peek(), "Encountered non-pattern token.");
    }

    private Pattern lazyPattern() {
        Token token = previous();
        Pattern inner = pattern();
        if (!(inner instanceof Pattern.Ignore || inner instanceof Pattern.Id)) {
            throw new ParseException(token, "Lazy pattern must be _ or identifier.");
        }
        consume(RIGHT_CURLY, "Expect '}' to close lazy pattern.");
        return new Pattern.Strict(inner);
    }

    public Pattern listPattern() {
        List<Pattern> patterns = new ArrayList<>();
        while (!check(RIGHT_BRACKET)) {
            patterns.add(pattern());
            if (check(RIGHT_BRACKET)) break;
            consume(COMMA, "Expect ',' between list patterns.");
        }
        consume(RIGHT_BRACKET, "Expect ']' to end list pattern.");
        return new Pattern.SlangList(scala.jdk.CollectionConverters.ListHasAsScala(patterns).asScala().toList());
    }
}
