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

        // Try each possible thing, backtrack if it didn't work.
        Pair<Expr, ParseException> expr;
        List<ParseException> exceptions = new ArrayList<>();

        expr = parser.tryParse(this::parseBlock);
        if (expr.right == null) return expr.left;
        exceptions.add(expr.right);

        expr = parser.tryParse(this::parseMatchBlock);
        if (expr.right == null) return expr.left;
        exceptions.add(expr.right);

        // All failed in some way, we have no way to tell what's right -- report all errors.
        var scalaList = scala.jdk.CollectionConverters.ListHasAsScala(exceptions).asScala().toList();
        throw new AmbiguousParseException(token, scalaList);
    }

    private Expr parseBlock(Parser parser) {
        List<Expr> exprs = parser.parseExprLines(RIGHT_CURLY);
        parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");
        var scalaList = scala.jdk.CollectionConverters.ListHasAsScala(exprs).asScala().toList();
        return new Expr.Block(new Expr.Seq(scalaList));
    }

    private Expr parseMatchBlock(Parser parser) {
        List<Expr.MatchRow> matches = parseMatchRows(parser);
        parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");

        // Sort by size so that shorter argument patterns go first.
        matches = matches
                .stream()
                .sorted(Comparator.comparingInt(match -> match.patterns().size()))
                .collect(Collectors.toList());

        var scalaList = scala.jdk.CollectionConverters.ListHasAsScala(matches).asScala().toList();
        return new Expr.Matchbox(scalaList);
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
}
