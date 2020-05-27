package slang.parse;

import slang.lex.Token;
import slang.util.Pair;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import static slang.lex.TokenType.*;

public class BlockParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        parser.skipNewlines();
        parser.setBookmark(); // Remember first token within block.

        // Try each possible thing, backtrack if it didn't work.
        Pair<Expr, ParseException> expr;
        List<ParseException> exceptions = new ArrayList<>();

        expr = tryParseMatchBlock(parser);
        if (expr.right == null) return expr.left;
        exceptions.add(expr.right);

        parser.goToBookmark();

        expr = tryParseBlock(parser);
        if (expr.right == null) return expr.left;
        exceptions.add(expr.right);

        // All failed in some way, we have no way to tell what's right -- report all errors.
        parser.goToBookmark();
        throw new AmbiguousParseException(token, scala.jdk.CollectionConverters.ListHasAsScala(exceptions).asScala().toList());
    }

    private Pair<Expr, ParseException> tryParseBlock(Parser parser) {
        try {
            List<Expr> exprs = parseExprLines(parser);
            parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");
            return new Pair(new Expr.Block(new Expr.Seq(scala.jdk.CollectionConverters.ListHasAsScala(exprs).asScala().toList())), null);
        } catch (ParseException e) {
            return new Pair(null, e);
        }
    }

    private Pair<Expr, ParseException> tryParseMatchBlock(Parser parser) {
        try {
            List<Expr.MatchRow> matches = parseMatchRows(parser);
            parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");

            // Sort by size so that shorter argument patterns go first.
            matches = matches
                    .stream()
                    .sorted(Comparator.comparingInt(match -> match.patterns().size()))
                    .collect(Collectors.toList());

            return new Pair(new Expr.Matchbox(scala.jdk.CollectionConverters.ListHasAsScala(matches).asScala().toList()), null);
        } catch (ParseException e) {
            return new Pair(null, e);
        }
    }

    private List<Pattern> parseMatchParams(Parser parser) {
        List<Pattern> patterns = new ArrayList<>();

        // Read up all the patterns.
        while (!parser.check(ARROW) && !parser.check(PIPE)) {
            patterns.add(parser.pattern());
        }

        return patterns;
    }

    private Expr.MatchRow parseMatch(Parser parser) {
        List<Pattern> patterns = parseMatchParams(parser);

        parser.consume(ARROW, "Expect -> after pattern.");

        Expr expr = parser.expression();
        return new Expr.MatchRow(scala.jdk.CollectionConverters.ListHasAsScala(patterns).asScala().toList(), expr);
    }

    private List<Expr.MatchRow> parseMatchRows(Parser parser) {
        List<Expr.MatchRow> matches = new ArrayList<>();
        while (!parser.check(RIGHT_CURLY)) {
            matches.add(parseMatch(parser));
            if (parser.check(RIGHT_CURLY)) break;
            if (!parser.match(NEWLINE, COMMA)) {
                throw new ParseException(parser.peek(), "Expect newline or comma to separate matchbox clauses.");
            }
            parser.skipNewlines();
        }
        return matches;
    }

    private List<Expr> parseExprLines(Parser parser) {
        List<Expr> exprs = new ArrayList<>();
        while (!parser.check(RIGHT_CURLY)) {
            exprs.add(parser.expression());
            if (parser.check(RIGHT_CURLY)) break;
            if (!parser.match(NEWLINE)) {
                throw new ParseException(parser.peek(), "Expect newline to separate block exprs.");
            }
            parser.skipNewlines();
        }
        return exprs;
    }
}
