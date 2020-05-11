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

        expr = tryParseMatchBlock(parser);
        if (expr.right == null) return expr.left;

        parser.goToBookmark();

        expr = tryParseBlock(parser);
        if (expr.right == null) return expr.left;

        parser.goToBookmark();

        expr = tryParsePipeBlock(parser);
        if (expr.right == null) return expr.left;

        // All failed in some way, we have no way to tell what's right -- report all errors.
        throw new ParseException(token, "Ambiguous block expression.");
    }

    private Pair<Expr, ParseException> tryParseBlock(Parser parser) {
        try {
            List<Stmt> statements = parseStatements(parser);
            parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");
            return new Pair(new Expr.Block(statements), null);
        } catch (ParseException e) {
            return new Pair(null, e);
        }
    }

    private Pair<Expr, ParseException> tryParsePipeBlock(Parser parser) {
        try {
            List<Pattern> params = parseMatchParams(parser);
            parser.skipNewlines(); // TODO: ??? Allow putting '|' on a line after the parameter list???
            parser.consume(PIPE, "Expect '|' to end parameter list.");
            parser.skipNewlines(); // Body of block should be able to start on a different line from '|' or the open curly brace.

            List<Stmt> statements = parseStatements(parser);
            parser.consume(RIGHT_CURLY, "Expect `}` at end of block.");

            Expr expr = new Expr.Block(statements);

            if (statements.size() == 1) {
                Stmt first = statements.get(0);
                if (first instanceof Stmt.Expression) {
                    expr = ((Stmt.Expression) first).expr;
                }
            }

            List<Stmt.Match> matches = List.of(new Stmt.Match(params, expr));
            return new Pair(new Expr.MatchBlock(matches), null);
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
                    .sorted(Comparator.comparingInt(match -> match.patterns.size()))
                    .collect(Collectors.toList());

            return new Pair(new Expr.MatchBlock(matches), null);
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
        return new Stmt.Match(patterns, expr);
    }

    private List<Stmt.Match> parseMatchStatements(Parser parser) {
        List<Stmt.Match> matches = new ArrayList<>();
        while (!parser.check(RIGHT_CURLY)) {
            matches.add(parseMatch(parser));
            if (parser.check(RIGHT_CURLY)) break;
            if (!parser.match(NEWLINE, COMMA)) {
                throw new ParseException(parser.peek(), "Expect newline or comma after match.");
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
