package slang.ux

import scala.io.AnsiColor._
import scala.collection.mutable

import slang.lex._
import slang.sourcemap._

sealed trait MessageType
object MessageType {
  case object Error extends MessageType
  case object Warning extends MessageType
}

case class SpanMessage(span: Span, message: String)

trait ErrorReporter {
  def error(span: Span, message: String): Unit
  def warning(span: Span, message: String): Unit
  def trace(spans: Seq[Span], message: String): Unit
  def errors: Iterable[SpanMessage]
  def warnings: Iterable[SpanMessage]
}

class ConsoleErrorReporter(sourceMap: SourceMap) extends ErrorReporter {
  private var _errors = mutable.ListBuffer[SpanMessage]()
  private var _warnings = mutable.ListBuffer[SpanMessage]()

  def errors = _errors
  def warnings = _warnings

  def error(span: Span, message: String) = {
    _errors.addOne(SpanMessage(span, message))
    print(MessageType.Error, span, message)
  }

  def warning(span: Span, message: String) = {
    _warnings.addOne(SpanMessage(span, message))
    print(MessageType.Warning, span, message)
  }

  def trace(spans: Seq[Span], message: String) = {
    System.err.println(s"$BOLD${RED}Runtime Error:$RESET $message")
    for (span <- spans) {
      val (file, Loc(line, col)) = sourceMap.location(span.start)
      val lineStr = file.getSourceAtLine(line).stripLineEnd

      System.err.println(s"$RED -> at ${file.path}:$line:$col$RESET")
      System.err.println(s"      $lineStr")
      val preHighlightStr = lineStr.slice(0, col - 1)
      val indent = preHighlightStr.replaceAll("[^\t]", " ")
      System.err.println(s"      " + indent + s"$RED^$RESET")
    }
  }

  def print(ty: MessageType, span: Span, message: String) = {
    val (color, heading) = ty match {
      case MessageType.Error   => (RED, "Syntax Error")
      case MessageType.Warning => (YELLOW, "Syntax Warning")
    }

    val (file, Loc(line, col)) = sourceMap.location(span.start)
    val lineStr = file.getSourceAtLine(line).stripLineEnd
    val spanLen = span.end - span.start

    // Highlight the span of the source text.
    val preHighlightStr = lineStr.slice(0, col - 1)
    val highlightStr = color + lineStr.slice(col - 1, col - 1 + spanLen) + RESET
    val postHighlightStr = lineStr.slice(col - 1 + spanLen, lineStr.length)

    // Generate the indent for the indicator line (e.g. ^^) so that
    // even input with \t should align.
    val indent = preHighlightStr.replaceAll("[^\t]", " ")

    val lineNumStrLen = line.toString.length
    val border = BOLD + BLUE + "  " + " " * lineNumStrLen + " | " + RESET

    System.err.println(
      s"$BOLD$color$heading:$RESET$BOLD $message$RESET"
    )
    System.err.println(
      s"$BOLD$BLUE  " + " " * lineNumStrLen + " = " + s"${file.path}:${line}:${col}$RESET"
    )
    System.err.println(border)
    System.err.println(
      s"$BOLD$BLUE  $line | $RESET$preHighlightStr$highlightStr$postHighlightStr"
    )
    System.err.println(border + indent + color + ("^" * spanLen.max(1)) + RESET)
  }
}
