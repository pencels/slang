package slang.lex

case class LexerException(span: Span, message: String)
    extends Exception(message)
