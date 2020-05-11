package slang.parse;

import slang.lex.Token;

import javax.swing.text.ElementIterator;

import static slang.lex.TokenType.*;

public class StringInterpolationParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        Expr left = new Expr.Literal(LiteralParselet.valueFromToken(token));
        Expr inner = parser.expression();
        parser.consume(STRING_INTERP_END, "Expect end of string interpolation expression.");

        // Transform into string concatenation.
        Token concatOp = new Token(PLUS, "+", null, 0, 0);
        Expr concat = new Expr.Binary(left, concatOp, inner);

        if (parser.match(STRING)) {
            // End of string, attach.
            Token end = parser.previous();
            Expr right = new Expr.Literal(LiteralParselet.valueFromToken(end));
            return new Expr.Binary(concat, concatOp, right);
        } else if (parser.match(STRING_INTERP_START)) {
            Expr right = parser.expression();
            return new Expr.Binary(concat, concatOp, right);
        } else {
            throw new ParseException(parser.peek(), "Expect end of string literal.");
        }
    }
}
