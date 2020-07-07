package slang.lex

import slang.lex.TokenType
import scala.collection._
import scala.jdk.CollectionConverters._

object Lexer {
    val operatorTrie = OperatorTrie.empty

    // Add tokens to trie so that we can recognize them for patterns.
    operatorTrie.add("&!") 
    operatorTrie.add("..")

    val EOF = '\u0000'
    val operatorBoundaryChars = raw"""$EOF\s"`()\[\]{};,"""
    val atomPattern = raw"""[^$operatorBoundaryChars]""".r
    val operatorLikePattern = raw"""[^A-Za-z0-9_$operatorBoundaryChars]""".r
}

class Lexer(val source: java.lang.String) extends Iterator[Token] {

    private val KEYWORDS: Map[String, TokenType] = Map[String, TokenType](
        "let" -> TokenType.Let,
        "print" -> TokenType.Print,
        "nothing" -> TokenType.Nothing,
        "operator" -> TokenType.KwOperator,
    )

    private var start = 0
    private var current = 0

    private var line = 1
    private var col = 0

    private var consumed = false

    private var operatorDeclContext = false

    override def clone(): Lexer = {
        val lexer = new Lexer(source)
        lexer.start = start
        lexer.current = current
        lexer.line = line
        lexer.col = col
        lexer.consumed = consumed
        lexer.operatorDeclContext = operatorDeclContext
        lexer
    }

    def lex: List[Token] = toList

    override def hasNext: Boolean = !consumed

    override def next(): Token = {
        if (isAtEnd) {
            consumed = true
        }
        nextToken
    }

    private def isAtEnd: Boolean = current >= source.length

    private def advance: Char = {
        if (isAtEnd) {
            Lexer.EOF
        } else {
            col += 1
            current += 1
            source(current - 1)
        }
    }

    private def peek: Char = {
        if (isAtEnd) {
            Lexer.EOF
        } else {
            source(current)
        }
    }

    private def peekNext: Char = {
        if (current + 1 >= source.length) {
            Lexer.EOF
        } else {
            source(current + 1)
        }
    }

    private def matchChar(expected: Char): Boolean = {
        if (isAtEnd || source(current) != expected) {
            false
        } else {
            advance
            true
        }
    }

    private def newline: Unit = {
        line += 1
        col = 1
    }

    private def skipWhitespace: Unit = {
        while (matchChar(' ') || matchChar('\t')) {}
    }

    private def createToken(ty: TokenType): Token = {
        Token(ty, source.substring(start, current), Loc(line, col))
    }

    private def nextToken: Token = {
        skipWhitespace

        start = current

        val c = advance

        c match {
            case '(' => createToken(TokenType.LParen)
            case ')' => createToken(TokenType.RParen)
            case '{' => createToken(TokenType.LCurly)
            case '}' => createToken(TokenType.RCurly)
            case '[' => createToken(TokenType.LBracket)
            case ']' => createToken(TokenType.RBracket)
            case ';' => createToken(TokenType.Semicolon)
            case ',' => createToken(TokenType.Comma)
            case '-' => 
                if (matchChar('>')) {
                    createToken(TokenType.Arrow)
                } else if (matchChar('-')) {
                    comment
                } else {
                    customOrUnknownOperator(c)
                }
            case '=' => 
                if (isOp(peek)) {
                    customOrUnknownOperator(c)
                } else {
                    createToken(TokenType.Eq)
                }
            case '"' => string
            case ':' => atom
            case '`' => backtickOperator
            case '\n' =>
                operatorDeclContext = false // End operator declaration context at the end of an expr.
                val tok = createToken(TokenType.Newline)
                newline
                tok
            case Lexer.EOF => createToken(TokenType.Eof)
            case _ =>
                if (isAlpha(c)) {
                    identifier
                } else if (isNum(c)) {
                    number
                } else if (isOp(c)) {
                    customOrUnknownOperator(c)
                } else {
                    throw new LexException(c, Loc(line, col))
                }
        }
    }

    private def comment: Token = {
        while (peek != '\n' && !isAtEnd) {
            advance
        }
        createToken(TokenType.Comment(source.substring(start, current)))
    }

    private def string: Token = {
        while (peek != '"') {
            advance
        }

        if (isAtEnd) {
            throw new RuntimeException(s"EOF while parsing string literal.")
        }

        advance // Eat the closing '"'.

        val value = source.substring(start + 1, current - 1)
        createToken(TokenType.String(value))
    }

    private def isAlpha(c: Char): Boolean = {
        'A' <= c && c <= 'Z' ||
        'a' <= c && c <= 'z' ||
        c == '_'
    }

    private def isNum(c: Char): Boolean = '0' <= c && c <= '9'

    private def isOp(c: Char): Boolean = c match {
        case Lexer.operatorLikePattern() => true
        case _ => false
    }

    private def isAtom(c: Char): Boolean = c match {
        case Lexer.atomPattern() => true
        case _ => false
    }

    private def identifier: Token = {
        while (isAlpha(peek) || isNum(peek)) {
            advance
        }

        val name = source.substring(start, current)

        if (KEYWORDS.contains(name)) {
            if (name == "operator") {
                operatorDeclContext = true
            }
            createToken(KEYWORDS(name))
        } else {
            createToken(TokenType.Id(name))
        }
    }

    private def atom: Token = {
        if (peek == '`') {
            advance // Eat opening '`'
            val name = backtick
            createToken(TokenType.Atom(name.substring(1)))
        } else {
            while (!isAtEnd && isAtom(peek)) {
                advance
            }

            val name = source.substring(start + 1, current)
            createToken(TokenType.Atom(name))
        }
    }

    private def customOrUnknownOperator(ch: Char): Token = {
        if (operatorDeclContext) {
            return unknownOperator
        }

        var node = Lexer.operatorTrie.get(ch)
        var lastEnd: Option[Int] = node.flatMap { node => if (node.end) Some(current) else None }

        while (node.isDefined && !isAtEnd && isOp(peek)) {
            node = node.flatMap(_.get(advance))
            lastEnd = node.flatMap { node => if (node.end) Some(current) else None } orElse lastEnd
        }

        lastEnd match {
            case Some(ind) =>
                current = ind
                val op = source.substring(start, ind)
                createToken(TokenType.Operator(op))
            case _ => unknownOperator
        }
    }

    private def unknownOperator: Token = {
        while (!isAtEnd && isOp(peek)) {
            advance
        }

        val op = source.substring(start, current)
        createToken(TokenType.UnknownOperator(op))
    }

    private def backtick: String = {
        while (peek != '`') {
            advance
        }
        advance // Eat the closing '`'

        source.substring(start + 1, current - 1)
    }

    private def backtickOperator: Token = {
        val name = backtick
        createToken(TokenType.Operator(name))
    }

    private def number: Token = {
        while (isNum(peek)) {
            advance
        }

        if (peek == '.' && isNum(peekNext)) {
            advance
            while (isNum(peek)) {
                advance
            }
        }

        val text = source.substring(start, current)
        createToken(TokenType.Number(text.toDouble))
    }
}