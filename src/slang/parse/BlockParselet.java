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
            List<Stmt> statements = parseStatements(parser);
            parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");
            return new Pair(new Expr.Block(scala.jdk.CollectionConverters.ListHasAsScala(statements).asScala().toList()), null);
        } catch (ParseException e) {
            return new Pair(null, e);
        }
    }

    private Pair<Expr, ParseException> tryParseMatchBlock(Parser parser) {
        try {
            List<Stmt.Match> matches = parseMatchStatements(parser);
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

    private Stmt.Match parseMatch(Parser parser) {
        List<Pattern> patterns = parseMatchParams(parser);

        parser.consume(ARROW, "Expect -> after pattern.");

        Expr expr = parser.expression();
        return new Stmt.Match(scala.jdk.CollectionConverters.ListHasAsScala(patterns).asScala().toList(), expr);
    }

    private List<Stmt.Match> parseMatchStatements(Parser parser) {
        List<Stmt.Match> matches = new ArrayList<>();
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

    private List<Stmt> parseStatements(Parser parser) {
        List<Stmt> statements = new ArrayList<>();
        while (!parser.check(RIGHT_CURLY) && !parser.isAtEnd()) {
            parser.skipNewlines();
            statements.add(parser.statement());
            if (parser.check(RIGHT_CURLY)) break;
            if (!parser.match(NEWLINE, SEMI)) {
                throw new ParseException(parser.peek(), "Expect newline or semicolon after statement.");
            }
        }
        return statements;
    }
}
