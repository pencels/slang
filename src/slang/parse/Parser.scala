package slang.parse

import scala.collection._
import scala.jdk.CollectionConverters._

import slang.lex._
import slang.lex.TokenType
import slang.parse._
import scala.annotation.tailrec

class Parser(val tokens: List[Token]) {
    private val prefixParselets: mutable.HashMap[TokenType, PrefixParselet] = new mutable.HashMap[TokenType, PrefixParselet]
    private val infixParselets: mutable.HashMap[TokenType, InfixParselet] = new mutable.HashMap[TokenType, InfixParselet]

    private var current = 0

    private def register(ty: TokenType, parselet: PrefixParselet) = {
        prefixParselets += (ty -> parselet)
    }

    private def register(ty: TokenType, parselet: InfixParselet) = {
        infixParselets += (ty -> parselet)
    }

    private def registerAtom(ty: TokenType, parselet: PrefixParselet) = {
        register(ty, parselet)
        register(ty, new CallParselet)
    }

    private def prefix(ty: TokenType) = {
        registerAtom(ty, new PrefixOpParselet)
    }

    private def binary(ty: TokenType, precedence: Int, isRight: Boolean) = {
        register(ty, new BinaryOpParselet(precedence, isRight))
    }

    private def postfix(ty: TokenType) = {
        register(ty, new PostfixOpParselet)
    }

    def parse: List[Expr] = {
        var exprs: List[Expr] = Nil
        while (!isAtEnd) {
            skipNewlines // Consume any empty lines before trying to parse an expr.
            if (!isAtEnd) {
                exprs = expression() :: exprs
            }
            skipComments
            if (!isAtEnd && !consume(TokenType.Newline)) {
                throw new ParseException(peek, "Expect newline after an expression.")
            }
        }
        exprs.reverse
    }

    def skipComments: Unit = peek.ty match {
        case TokenType.Comment(_) =>
            advance
        case _ =>
    }

    @tailrec
    final def skipNewlines: Unit = peek.ty match {
        case TokenType.Newline | TokenType.Comment(_) =>
            advance
            skipNewlines
        case _ =>
    }

    def peek: Token = tokens(current)
    def previous: Token = tokens(current - 1)

    private def advance: Token = {
        if (!isAtEnd) {
            current += 1
        }
        previous
    }

    def consume(ty: TokenType with PlainToken*): Boolean = {
        if (ty.exists(_ == peek.ty)) {
            advance
            true
        } else {
            false
        }
    }

    def check(ty: TokenType with PlainToken): Boolean = {
        if (isAtEnd) false else peek.ty == ty
    }

    private def isAtEnd: Boolean = peek.ty == TokenType.Eof

    def expect(expected: TokenType, errorMessage: String) = {
        if (peek.ty != expected) {
            throw new ParseException(previous, errorMessage)
        }

        advance
    }

    def tryParse[T](action: Function[Parser, T]): Either[T, ParseException] = {
        var cursor = current
        try {
            Left(action.apply(this))
        } catch {
            case e: ParseException => 
                current = cursor
                Right(e)
        }
    }

    def getInfixParselet(token: Token): InfixParselet = token.ty match {
        case TokenType.Star | TokenType.Slash | TokenType.Percent => new BinaryOpParselet(Precedence.PRODUCT, false)
        case TokenType.Plus | TokenType.Minus => new BinaryOpParselet(Precedence.SUM, false)
        case TokenType.EqEq |
             TokenType.Ne |
             TokenType.Lt |
             TokenType.Le |
             TokenType.Gt |
             TokenType.Ge => new BinaryOpParselet(Precedence.CONDITIONAL, false)
        case TokenType.At => new BinaryOpParselet(Precedence.APPLY, false)
        case TokenType.Semicolon => new BinaryOpParselet(Precedence.SEQUENCE, false)

        case TokenType.Bang => new PostfixOpParselet

        case TokenType.Newline => new SkipParselet

        case TokenType.LParen |
             TokenType.LCurly |
             TokenType.LBracket |
             TokenType.Nothing |
             _: TokenType.Id |
             _: TokenType.Atom |
             _: TokenType.Number |
             _: TokenType.String => new CallParselet

        case _ => null
    }

    def getPrefixParselet(token: Token): PrefixParselet = token.ty match {
        case TokenType.Let => new LetParselet
        case TokenType.Print => new PrintParselet
        case TokenType.Newline => new SkipParselet

        case TokenType.LParen => new GroupParselet
        case TokenType.LCurly => new BlockParselet
        case TokenType.LBracket => new ListParselet
        case _: TokenType.Id => new IdParselet
        case TokenType.Nothing |
             _: TokenType.Atom |
             _: TokenType.Number |
             _: TokenType.String => new LiteralParselet

        case TokenType.Minus |
             TokenType.Plus |
             TokenType.Bang |
             TokenType.Ampersand |
             TokenType.Star |
             TokenType.StarBang => new PrefixOpParselet

        case _ => null
    }

    def expression(precedence: Int = 0): Expr = {
        tryParse(parser => {
            val pat = parser.pattern
            parser.expect(TokenType.Eq, "Expect '=' after pattern.")
            val right = parser.expression()
            Expr.Assign(pat, right)
        }) match {
            case Left(expr) => return expr
            case _ =>
        }

        var token = advance
        val prefix = getPrefixParselet(token)

        if (prefix == null) {
            throw new ParseException(token, s"Could not parse token: ${token}")
        }

        var left = prefix.parse(this, token)

        while (precedence < currentPrecedence) {
            val nextToken = peek
            if (!getInfixParselet(nextToken).isInstanceOf[CallParselet]) {
                token = advance
            }
            val infix = getInfixParselet(nextToken)
            left = infix.parse(this, left, token)
        }

        left
    }

    def currentPrecedence: Int = {
        val parser = getInfixParselet(peek)
        if (parser != null) {
            parser.getPrecedence
        } else {
            0
        }
    }

    def parseExprLines(end: TokenType with PlainToken): List[Expr] = {
        var exprs: List[Expr] = Nil

        while (!check(end)) {
            exprs = expression() :: exprs
            if (check(end)) return exprs.reverse
            if (!consume(TokenType.Newline)) {
                throw new ParseException(peek, "Expected newline to separate expressions.")
            }
            skipNewlines
        }

        exprs.reverse
    }

    def consumeNumber() = peek.ty match {
        case _: TokenType.Number => advance; true
        case _ => false
    }
    
    def consumeAtom() = peek.ty match {
        case _: TokenType.Atom => advance; true
        case _ => false
    }

    def consumeString() = peek.ty match {
        case _: TokenType.String => advance; true
        case _ => false
    }

    def expect(ty: TokenType with PlainToken, message: String): Unit = {
        if (!consume(ty)) {
            throw new ParseException(peek, message)
        }
    }

    def pattern(): Pattern = {
        if (consume(TokenType.Nothing) || consumeNumber || consumeAtom || consumeString) {
            val value = LiteralParselet.valueFromToken(previous)
            return Pattern.Literal(value)
        }

        if (consume(TokenType.Ampersand)) {
            return strictPattern(false)
        }
        if (consume(TokenType.AmpersandBang)) {
            return strictPattern(true)
        }

        if (consume(TokenType.Bang)) {
            if (consume(TokenType.LCurly)) {
                return strictBlockPattern(true)
            }
            throw new ParseException(peek, "Expected strict block pattern { ... } after '!'.")
        }

        if (consume(TokenType.LCurly)) {
            return strictBlockPattern(false)
        }

        if (consume(TokenType.LBracket)) {
            return listPattern
        }

        peek match {
            case id @ Token(TokenType.Id(name), _, _) =>
            advance
                if (name == "_") {
                    return Pattern.Ignore(id)
                }
                if (consume(TokenType.DotDot)) {
                    return Pattern.Spread(id)
                } else {
                    return Pattern.Id(id)
                }
            case _ =>
        }

        throw new ParseException(peek, "Encountered non-pattern token.")
    }

    private def strictBlockPattern(full: Boolean): Pattern = {
        val inner = strictPattern(full)
        expect(TokenType.RCurly, "Expect '}' to close block pattern.")
        inner
    }

    private def strictPattern(full: Boolean): Pattern = {
        val token = previous
        val inner = pattern
        inner match {
            case Pattern.Ignore(_) | Pattern.Id(_) => Pattern.Strict(inner, full)
            case _ => throw new ParseException(token, "Lazy pattern must be _ or identifier.")
        }
    }

    private def listPattern(): Pattern = {
        var patterns: List[Pattern] = Nil
        while (!check(TokenType.RBracket)) {
            patterns = pattern :: patterns
            if (!check(TokenType.RBracket)) {
                expect(TokenType.Comma, "Expect ',' between list patterns.")
            }
        }
        expect(TokenType.RBracket, "Expect ']' to end list pattern.")
        Pattern.SlangList(patterns.reverse)
    }
}