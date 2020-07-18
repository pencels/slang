package slang.parse

import scala.collection._
import scala.annotation.tailrec
import scala.util.control.Breaks._

import slang.lex._
import slang.parse._
import slang.runtime._

object Parser {
    private val namedPrefixParselets = new mutable.HashMap[String, PrefixParselet]
    private val namedInfixParselets = new mutable.HashMap[String, InfixParselet]

    prefix("&") // Add & in core language as a prefix parselet.
    infix(".", Associativity.Right, 450)

    def prefix(name: String) = {
        Parser.namedPrefixParselets += (name -> new PrefixOpParselet)
        Parser.namedInfixParselets.getOrElseUpdate(name, new CallParselet)
        Lexer.operatorTrie.add(name)
    }

    def postfix(name: String) = {
        Parser.namedInfixParselets += (name -> new PostfixOpParselet)
        Lexer.operatorTrie.add(name)
    }

    def infix(name: String, assoc: Associativity, prec: Int) = {
        Parser.namedInfixParselets += (name -> new BinaryOpParselet(prec, assoc == Associativity.Right))
        Lexer.operatorTrie.add(name)
    }
}

class Parser(var lexer: Lexer) extends Iterator[Expr] {
    private var previousOption: Option[Token] = None
    private var currentOption: Option[Token] = None

    def parse: List[Expr] = toList

    override def hasNext: Boolean = {
        skipNewlines
        !isAtEnd
    }

    override def next(): Expr = {
        peek.ty match {
            case TokenType.KwOperator =>
                operatorDecl
                skipNewlines
                return Expr.Literal(Value.Nothing)
            case _ =>
        }

        val expr = sequenceExpr
        skipNewlines
        expr
    }

    def operatorDecl() = {
        val token = advance // Eat 'operator'
        peek.ty match {
            case TokenType.Id("prefix") =>
                advance
                val ops = operators
                for (op <- ops) {
                    if (Parser.namedPrefixParselets contains op) {
                        throw new ParseException(previous, s"Prefix operator `$op` is already defined.")
                    }
                    Parser.prefix(op)
                }
            case TokenType.Id("postfix") =>
                advance
                val ops = operators
                for (op <- ops) {
                    if (Parser.namedInfixParselets get op exists { !_.isInstanceOf[CallParselet] }) {
                        throw new ParseException(previous, s"Operator `$op` is already defined as postfix or infix.")
                    }
                    Parser.postfix(op)
                }
            case TokenType.Id("infix") =>
                val (ops, assoc, prec) = infixDecl
                for (op <- ops) {
                    if (Parser.namedInfixParselets get op exists { !_.isInstanceOf[CallParselet] }) {
                        throw new ParseException(previous, s"Operator `$op` is already defined as postfix or infix.")
                    }
                    Parser.infix(op, assoc, prec)
                }
            case _ => throw new ParseException(token, "Expected 'prefix', 'postfix', or 'infix' operator type.")
        }
    }

    def operator() = {
        peek.ty match {
            case TokenType.UnknownOperator(op) =>
                advance // Eat operator
                op
            case TokenType.Operator(op) =>
                advance // Eat operator
                op
            case _ => throw new ParseException(peek, "Expected operator literal.")
        }
    }

    def operators() = {
        var continue = true
        val ops = new mutable.ListBuffer[String]
        while (continue) {
            peek.ty match {
                case TokenType.UnknownOperator(_) => ops.addOne(operator)
                case TokenType.Operator(_) => ops.addOne(operator)
                case _ => continue = false
            }
        }
        ops.toList
    }

    def infixDecl() = {
        advance // Eat 'infix' token

        peek.ty match {
            case TokenType.Id(assocStr @ ("left" | "right")) =>
                advance // Eat associativity
                val assoc = assocStr match {
                    case "left" => Associativity.Left
                    case "right" => Associativity.Right
                }
                peek.ty match {
                    case TokenType.Number(prec) if prec.isWhole =>
                        if (prec >= Precedence.CALL) {
                            throw new ParseException(peek, s"Precedence of $prec too high.")
                        }
                        advance
                        val ops = operators
                        (ops, assoc, prec.toInt)
                    case _ => throw new ParseException(peek, "Expected integer precedence value.")
                }
            case _ => throw new ParseException(peek, "Expected 'left' or 'right' associativity declaration.")
        }
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

    def getInfixParselet(token: Token): Option[InfixParselet] = token.ty match {
        case TokenType.LParen |
             TokenType.LCurly |
             TokenType.LBracket |
             TokenType.Nothing |
             _: TokenType.Id |
             _: TokenType.Atom |
             _: TokenType.Number |
             _: TokenType.String => Some(new CallParselet)

        case TokenType.Operator(op) =>
            Some(Parser.namedInfixParselets.get(op).getOrElse {
                throw new ParseException(token, s"Operator `$op` is not infix")
            })
        case TokenType.UnknownOperator(op) =>
            throw new ParseException(token, s"Unknown infix operator `$op`")

        case _ => None
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

        case TokenType.Operator(op) =>
            Parser.namedPrefixParselets.get(op).getOrElse {
                throw new ParseException(token, s"Operator `$op` is not prefix")
            }
        case TokenType.UnknownOperator(op) => throw new ParseException(token, s"Unknown prefix operator `$op`")

        case _ => throw new ParseException(token, s"Could not parse token $token as prefix.")
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

        var left = prefix.parse(this, token)

        parseInfix(left, token, precedence)
    }

    @tailrec
    final def parseInfix(left: Expr, infixToken: Token, precedence: Int): Expr = {
        var token = infixToken
        // If there is a non-infix operator token, skip infix operator parsing.
        getInfixParselet(peek) match {
            case Some(infix) if precedence < infix.getPrecedence =>
                // Don't eat a token for call parselet, since it needs to preserve
                // tokens for expression() calls.
                if (!infix.isInstanceOf[CallParselet]) {
                    token = advance
                }
                val newLeft = infix.parse(this, left, token)
                parseInfix(newLeft, token, precedence)
            case _ => left
        }
    }

    def currentPrecedence: Int = {
        getInfixParselet(peek) match {
            case Some(parselet) => parselet.getPrecedence
            case None => 0
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
        case TokenType.Nothing => Value.Nothing
        case TokenType.Number(num) => Value.Number(num)
        case TokenType.Atom(name) => Value.Atom(name)
        case TokenType.String(value) => Value.String(value)
        case _ => throw new Exception(s"Cant get a value from ${ty}")
    }

    def pattern(): Pattern = {
        var pat = simplePattern()
        var continue = true
        while (continue) {
            peek.ty match {
                case TokenType.Operator(".") => 
                    advance // Eat '.'
                    pat = Pattern.Cons(pat, pattern())
                case _ => continue = false
            }
        }
        pat
    }

    def simplePattern() = {
        peek.ty match {
            case TokenType.Nothing |
                 _: TokenType.Number |
                 _: TokenType.Atom |
                 _: TokenType.String =>
                 Pattern.Literal(valueFromTokenType(advance.ty))
            case TokenType.LParen =>
                advance // Eat '('
                val pat = pattern
                expect(TokenType.RParen, "Expected ')' to close pattern.")
                pat
            case TokenType.Operator("&") =>
                advance
                strictPattern(false)
            case TokenType.Operator("&!") =>
                advance
                strictPattern(true)
            case TokenType.Operator("!") =>
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
                        case TokenType.Operator("..") =>
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
        Pattern.List(patterns.reverse)
    }
}