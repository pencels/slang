package slang.parse;

import java.util.List;

import slang.lex.Token;
import slang.lex.TokenType;

public class GroupParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        parser.skipNewlines();
        List<Expr> exprs = parser.parseExprLines(TokenType.RIGHT_PAREN);
        parser.consume(TokenType.RIGHT_PAREN, "Expect `)` to close group or sequence of expressions.");

        // If there's one expr, treat as a simple group.
        if (exprs.size() == 1) {
            return new Expr.Grouping(exprs.get(0));
        }

        // If there are 0 or multiple exprs, treat like a sequence of exprs.
        return new Expr.Seq(scala.jdk.CollectionConverters.ListHasAsScala(exprs).asScala().toList());
    }
}
