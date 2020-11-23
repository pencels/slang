package slang.lex

case class Span(start: Int, end: Int) {
  def merge(other: Span): Span =
    Span(start.min(other.start), end.max(other.end))
}

case class Loc(line: Int, col: Int)
