package slang.parse;

import slang.lex.Token;

import java.util.ArrayList;
import java.util.List;

import static slang.lex.TokenType.*;

public class SeqParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {

        List<Expr> elements = new ArrayList<>();

        while (!parser.check(RIGHT_BRACKET)) {
            parser.skipNewlines(); // Allow newlines before elements.
            elements.add(parser.expression());
            parser.skipNewlines(); // Allow newlines after elements.
            parser.match(COMMA, NEWLINE);
            //if (parser.check(RIGHT_BRACKET)) break;
            //parser.consume(COMMA, "Expect ',' between list elements.");
        }

        parser.consume(RIGHT_BRACKET, "Expect ']' to end list.");
        return new Expr.Seq(elements);
    }
}
