package slang.parse

import scala.collection._

import slang.lex.Token
import slang.lex.TokenType

class BlockParselet extends PrefixParselet {
    override def parse(parser: Parser, token: Token): Expr = {
        parser.skipNewlines

        // Try each possible thing, backtrack if it didn't work.
        val exceptions: mutable.ListBuffer[ParseException] = new mutable.ListBuffer[ParseException]

        parser.tryParse(parseBlock(_)) match {
            case Left(expr) => return expr
            case Right(exception) => exceptions.addOne(exception)
        }

        parser.tryParse(parseMatchBlock(_)) match {
            case Left(expr) => return expr
            case Right(exception) => exceptions.addOne(exception)
        }

        // All failed in some way, we have no way to tell what's right -- report all errors.
        throw new AmbiguousParseException(token, exceptions.toList);
    }

    private def parseBlock(parser: Parser): Expr = {
        val exprs = parser.parseExprLines(TokenType.RCurly)
        parser.consume(TokenType.RCurly, "Expect `}` at end of block.")
        Expr.Block(Expr.Seq(exprs))
    }

    private def parseMatchBlock(parser: Parser): Expr = {
        val matches = parseMatchRows(parser)
        parser.consume(TokenType.RCurly, "Expect `}` at end of block.")
        Expr.Matchbox(matches);
    }

    private def parseMatchParams(parser: Parser): List[Pattern] = {
        var patterns: List[Pattern] = Nil

        // Read up all the patterns.
        while (!parser.check(TokenType.Arrow)) {
            patterns = parser.pattern() :: patterns
        }

        patterns.reverse
    }

    private def parseMatchRow(parser: Parser): Expr.MatchRow = {
        val patterns = parseMatchParams(parser)
        parser.consume(TokenType.Arrow, "Expect -> after pattern.");

        val expr = parser.expression()
        Expr.MatchRow(patterns, expr);
    }

    private def parseMatchRows(parser: Parser): List[Expr.MatchRow] = {
        var matches: List[Expr.MatchRow] = Nil
        while (!parser.check(TokenType.RCurly)) {
            matches = parseMatchRow(parser) :: matches
            if (!parser.check(TokenType.RCurly)) {
                if (!parser.expect(TokenType.Newline, TokenType.Comma)) {
                    throw new ParseException(parser.peek, "Expect newline or comma to separate matchbox clauses.");
                }
            }
            parser.skipNewlines
        }
        matches.reverse
    }
}
