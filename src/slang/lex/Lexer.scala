package slang.lex

import slang.lex.TokenType
import scala.collection._
import scala.jdk.CollectionConverters._

class Lexer(val source: java.lang.String) {
    private val KEYWORDS: Map[String, TokenType] = Map[String, TokenType](
        "let" -> TokenType.Let,
        "print" -> TokenType.Print,
        "nothing" -> TokenType.Nothing,
    )

    private var start = 0
    private var current = 0

    private var line = 1
    private var col = 0

    def lex: List[Token] = {
        var tokens = new mutable.ListBuffer[Token]

        while (!isAtEnd) {
            tokens = tokens.addOne(nextToken)
        }

        tokens.addOne(Token(TokenType.Eof, "", Loc(line, col)))

        tokens.toList
    }

    private def isAtEnd: Boolean = current >= source.length

    private def advance: Char = {
        col += 1
        current += 1
        source(current - 1)
    }

    private def peek: Char = {
        if (isAtEnd) {
            '\u0000'
        } else {
            source(current)
        }
    }

    private def peekNext: Char = {
        if (current + 1 >= source.length) {
            '\u0000'
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
            case '!' =>
                if (matchChar('=')) {
                    createToken(TokenType.Ne)
                } else {
                    createToken(TokenType.Bang)
                }
            case '&' =>
                if (matchChar('!')) {
                    createToken(TokenType.AmpersandBang)
                } else {
                    createToken(TokenType.Ampersand)
                }
            case '|' => createToken(TokenType.Pipe)
            case '@' => createToken(TokenType.At)
            case ',' => createToken(TokenType.Comma)
            case '<' =>
                if (matchChar('=')) {
                    createToken(TokenType.Le)
                } else {
                    createToken(TokenType.Lt)
                }
            case '>' =>
                if (matchChar('=')) {
                    createToken(TokenType.Ge)
                } else {
                    createToken(TokenType.Gt)
                }
            case '.' => 
                if (matchChar('.')) {
                    createToken(TokenType.DotDot)
                } else {
                    createToken(TokenType.Dot)
                }
            case '-' => 
                if (matchChar('>')) {
                    createToken(TokenType.Arrow)
                } else if (matchChar('-')) {
                    comment
                } else {
                    createToken(TokenType.Minus)
                }
            case '+' => createToken(TokenType.Plus)
            case '*' =>
                if (matchChar('!')) {
                    createToken(TokenType.StarBang)
                } else {
                    createToken(TokenType.Star)
                }
            case '/' => createToken(TokenType.Slash)
            case '%' => createToken(TokenType.Percent)
            case '=' =>
                if (matchChar('=')) {
                    createToken(TokenType.EqEq)
                } else {
                    createToken(TokenType.Eq)
                }
            case '"' => string
            case ':' => atom
            case '\n' =>
                val tok = createToken(TokenType.Newline)
                newline
                tok
            case _ =>
                if (isAlpha(c)) {
                    identifier
                } else if (isNum(c)) {
                    number
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

    private def isAtom(c: Char): Boolean = isAlpha(c) || isNum(c) || c == '!' || c == '?'

    private def identifier: Token = {
        while (isAlpha(peek) || isNum(peek)) {
            advance
        }

        val name = source.substring(start, current)

        if (KEYWORDS.contains(name)) {
            createToken(KEYWORDS(name))
        } else {
            createToken(TokenType.Id(name))
        }
    }

    private def atom: Token = {
        while (isAtom(peek)) {
            advance
        }

        val name = source.substring(start + 1, current)
        createToken(TokenType.Atom(name))
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