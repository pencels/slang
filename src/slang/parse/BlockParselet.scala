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
        val exprs = parser.multilineSequenceExpr(TokenType.RCurly, "'}'")
        Expr.Block(exprs)
    }

    private def parseMatchBlock(parser: Parser): Expr = {
        val matches = parseMatchRows(parser)
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
        parser.expect(TokenType.Arrow, "Expect -> after pattern.");

        val seq = parser.sequenceExpr
        Expr.MatchRow(patterns, seq);
    }

    private def parseMatchRows(parser: Parser): List[Expr.MatchRow] = {
        parser.parseDelimited(parseMatchRow, TokenType.Comma, TokenType.RCurly, "matchbox row", "comma", "}")
    }
}
