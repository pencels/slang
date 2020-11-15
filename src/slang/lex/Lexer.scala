package slang.lex

class Lexer(file: SourceFile, operatorTrie: OperatorTrie)
    extends Iterator[Token] {
  private[this] val inputIterator = file.source.iterator.buffered

  var start = 0
  var current = 0

  skipWhitespace() // Prime the lexer by skipping leading whitespace.

  override def next(): Token = {
    start = current

    val token = advance() match {
      case '\n' => makeToken(TokenType.Newline)
      case '('  => makeToken(TokenType.LParen)
      case ')'  => makeToken(TokenType.RParen)
      case '['  => makeToken(TokenType.LSquare)
      case ']'  => makeToken(TokenType.RSquare)
      case '{'  => makeToken(TokenType.LCurly)
      case '}'  => makeToken(TokenType.RCurly)
      case '='  => makeToken(TokenType.Eq)
      case '-' =>
        if (matchChar('-')) {
          advanceWhileMatch(_ != '\n')
          makeToken(TokenType.Newline)
        } else {
          makeOpToken('-')
        }
      case ':' =>
        if (matchChar(':')) {
          makeToken(TokenType.ColonColon)
        } else {
          makeAtomToken()
        }
      case ch =>
        if (Lexer.IDENTIFIER_START contains ch) {
          makeIdToken()
        } else if (ch.isDigit) {
          makeNumberToken()
        } else if (Lexer.OPERATOR_START contains ch) {
          makeOpToken(ch)
        } else {
          throw new LexerException(
            Span(start, current),
            s"Unexpected character: $ch"
          )
        }
    }

    skipWhitespace() // Skip whitespace after every token to maintain hasNext invariant.

    token
  }

  override def hasNext: Boolean = peek.isDefined

  def makeIdToken() = {
    advanceWhileMatch(Lexer.IDENTIFIER_CONT contains _)

    val name = file.source.slice(start, current)

    if (Lexer.KEYWORDS contains name) {
      makeToken(Lexer.KEYWORDS(name))
    } else if (name(0).isUpper) {
      makeToken(TokenType.TypeId(name))
    } else {
      makeToken(TokenType.Id(name))
    }
  }

  def makeNumberToken() = {
    advanceWhileMatch(_.isDigit)
    matchChar('.')
    advanceWhileMatch(_.isDigit)

    val number = file.source.slice(start, current).toDouble
    makeToken(TokenType.Number(number))
  }

  def makeAtomToken() = {
    advanceWhileMatch(Lexer.IDENTIFIER_CONT contains _)

    val name = file.source.slice(start, current)
    makeToken(TokenType.Atom(name))
  }

  def makeOpToken(ch: Char): Token = {
    if (!operatorTrie.active) {
      return makeUnknownOpToken()
    }

    var node = operatorTrie.get(ch)
    var lastEnd: Option[Int] = node.flatMap { node =>
      if (node.end) Some(current) else None
    }

    while (node.isDefined && peek.exists(Lexer.OPERATOR_CONT contains _)) {
      node = node.flatMap(_.get(advance()))
      lastEnd = node.flatMap { node =>
        if (node.end) Some(current) else None
      } orElse lastEnd
    }

    lastEnd match {
      case Some(ind) =>
        current = ind
        val op = file.source.substring(start, ind)
        makeToken(TokenType.Op(op))
      case _ => makeUnknownOpToken()
    }
  }

  def makeUnknownOpToken() = {
    advanceWhileMatch(Lexer.OPERATOR_CONT contains _)

    val op = file.source.slice(start, current)

    throw new LexerException(Span(start, current), s"Unknown operator '$op'")

    makeToken(TokenType.UnknownOp(op))
  }

  def makeToken(ty: TokenType) = Token(Span(start, current), ty)

  def skipWhitespace() = {
    while (matchChar(' ') || matchChar('\t')) {}

    // Skip comments, if they're there.
    if (matchChar('-') && matchChar('-')) {
      advanceWhileMatch(_ != '\n')
    }
  }

  def matchChar(ch: Char) = {
    if (peek.exists(ch == _)) {
      advance()
      true
    } else {
      false
    }
  }

  def check(ch: Char) = peek.exists(_ == ch)
  def advanceWhileMatch(cond: Char => Boolean) = {
    while (peek.exists(cond)) {
      advance()
    }
  }

  def peek = inputIterator.headOption
  def advance() = {
    inputIterator.nextOption() match {
      case Some(c) =>
        current += 1
        c
      case None =>
        throw new Exception("advanced when there was no input")
    }
  }
}

object Lexer {
  val IDENTIFIER_START = (('A' to 'Z') ++ ('a' to 'z') :+ '_').toSet
  val IDENTIFIER_CONT = IDENTIFIER_START ++ ('0' to '9')
  val KEYWORDS = Map(
    "operator" -> TokenType.Operator,
    "when" -> TokenType.When,
    "let" -> TokenType.Let,
    "fn" -> TokenType.Fn
  )

  val OPERATOR_START =
    Set('/', '=', '-', '+', '!', '*', '%', '<', '>', '&', '|', '^', '~', '?',
      '$', '#') |
      (('\u00a1' to '\u00a7') ++
        Seq('\u00a9', '\u00ab') ++
        Seq('\u00ac', '\u00ae') ++
        ('\u00b0' to '\u00b1') ++
        Seq('\u00b6', '\u00bb', '\u00bf', '\u00d7', '\u00f7') ++
        ('\u2016' to '\u2017') ++
        ('\u2020' to '\u2027') ++
        ('\u2030' to '\u203e') ++
        ('\u2041' to '\u2053') ++
        ('\u2055' to '\u205e') ++
        ('\u2190' to '\u23ff') ++
        ('\u2500' to '\u2775') ++
        ('\u2794' to '\u2bff') ++
        ('\u2e00' to '\u2e7f') ++
        ('\u3001' to '\u3003') ++
        ('\u3008' to '\u3020') :+
        ('\u3030')).toSet

  val OPERATOR_CONT =
    OPERATOR_START |
      (('\u0300' to '\u036F') ++
        ('\u1dc0' to '\u1dff') ++
        ('\u20d0' to '\u20ff') ++
        ('\ufe00' to '\ufe0f') ++
        ('\ufe20' to '\ufe2f')).toSet
}
