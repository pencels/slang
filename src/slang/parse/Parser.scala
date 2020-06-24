package slang.parse

import scala.collection._
import scala.jdk.CollectionConverters._
import scala.annotation.tailrec
import scala.util.control.Breaks._

import slang.lex._
import slang.lex.TokenType
import slang.parse._
import slang.runtime.SlangNothing

class Parser(var lexer: Lexer) extends Iterator[Expr] {

    private val prefixParselets: mutable.HashMap[TokenType, PrefixParselet] = new mutable.HashMap[TokenType, PrefixParselet]
    private val infixParselets: mutable.HashMap[TokenType, InfixParselet] = new mutable.HashMap[TokenType, InfixParselet]

    private var previousOption: Option[Token] = None
    private var currentOption: Option[Token] = None

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

    def parse: List[Expr] = toList

    override def hasNext: Boolean = {
        skipNewlines
        !isAtEnd
    }

    override def next(): Expr = {
        val expr = sequenceExpr
        skipNewlines
        expr
    }

    @tailrec
    final def skipNewlines: Unit = peek.ty match {
        case TokenType.Newline | TokenType.Comment(_) =>
            advance
            skipNewlines
        case _ =>
    }

    def peek: Token = currentOption getOrElse {
        // Pump a token from the lexer if the lookahead isn't filled.
        val token = lexer.next
        currentOption = Some(token)
        token
    }

    def previous: Token = previousOption.get

    def advance: Token = {
        if (!isAtEnd) {
            previousOption = currentOption
            currentOption = lexer.nextOption
        }
        previousOption.get
    }

    def check(ty: TokenType with PlainToken): Boolean = {
        if (isAtEnd) false else peek.ty == ty
    }

    private def isAtEnd: Boolean = peek.ty == TokenType.Eof

    def expect(expected: TokenType with PlainToken, errorMessage: String) = {
        if (peek.ty != expected) {
            throw new ParseException(previous, errorMessage)
        }

        advance
    }

    def tryParse[T](action: Function[Parser, T]): Either[T, ParseException] = {
        val savedCurrent = currentOption
        val savedPrevious = previousOption
        val savedLexer = lexer.clone

        try {
            val result = action.apply(this)
            Left(result)
        } catch {
            case e: ParseException =>
                currentOption = savedCurrent
                previousOption = savedPrevious
                lexer = savedLexer
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

        case TokenType.Bang => new PostfixOpParselet

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

    def parseDelimited[T](
        parseElement: Function[Parser, T],
        delim: TokenType with PlainToken,
        end: TokenType with PlainToken,
        elementEnglish: String,
        delimEnglish: String,
        endEnglish: String,
    ): List[T] = {
        val inners = new mutable.ListBuffer[T]

        while (peek.ty != end) {
            skipNewlines
            if (peek.ty != end) {
                inners.addOne(parseElement(this))
            }
            peek.ty match {
                case ty if ty == end =>
                case ty if ty == delim => advance
                case TokenType.Newline | TokenType.Comment(_) => advance
                case _ => throw new ParseException(peek, s"Expected $delimEnglish or $endEnglish.")
            }
        }
        expect(end, s"Expected $endEnglish.")

        inners.toList
    }
    
    def sequenceExpr() = {
        val exprs = new mutable.ListBuffer[Expr]

        breakable {
            while (true) {
                skipNewlines
                exprs.addOne(expression())
                peek.ty match {
                    case TokenType.Semicolon => advance
                    case _ => break
                }
            }
        }

        if (exprs.length == 1) {
            exprs.head
        } else {
            Expr.Seq(exprs.toList)
        }
    }

    def multilineSequenceExpr(end: TokenType with PlainToken, endEnglish: String) = {
        val exprs = parseDelimited(_.expression(), TokenType.Semicolon, end, "expression", "';'", endEnglish)

        if (exprs.length == 1) {
            exprs.head
        } else {
            Expr.Seq(exprs)
        }
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

    def valueFromTokenType(ty: TokenType) = ty match {
        case TokenType.Nothing => SlangNothing
        case TokenType.Number(num) => slang.runtime.Number(num)
        case TokenType.Atom(name) => slang.runtime.Atom(name)
        case TokenType.String(value) => slang.runtime.SlangString(value)
        case _ => throw new Exception(s"Cant get a value from ${ty}")
    }

    def pattern(): Pattern = {
        peek.ty match {
            case TokenType.Nothing |
                 _: TokenType.Number |
                 _: TokenType.Atom |
                 _: TokenType.String =>
                 Pattern.Literal(valueFromTokenType(advance.ty))
            case TokenType.Ampersand =>
                advance
                strictPattern(false)
            case TokenType.AmpersandBang =>
                advance
                strictPattern(true)
            case TokenType.Bang =>
                advance
                peek.ty match {
                    case TokenType.LCurly =>
                        advance
                        strictBlockPattern(true)
                    case _ => throw new ParseException(peek, "Expected strict block pattern { ... } after '!'.")
                }
            case TokenType.LCurly =>
                advance
                strictBlockPattern(false)
            case TokenType.LBracket =>
                advance
                listPattern
            case TokenType.Id(name) =>
                val id = advance
                if (name == "_") {
                    Pattern.Ignore(id)
                } else {
                    peek.ty match {
                        case TokenType.DotDot =>
                            advance
                            Pattern.Spread(id)
                        case _ => Pattern.Id(id)
                    }
                }
            case _ => throw new ParseException(peek, "Encountered non-pattern token.")
        }
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