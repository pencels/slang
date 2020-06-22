package slang.lex

case class Loc(line: Int, col: Int)
case class Span(start: Int, end: Int)