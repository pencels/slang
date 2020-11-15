package slang.ux

import scala.io.AnsiColor._

import slang.lex._
import slang.ux.MessageType.Warning

sealed trait MessageType
object MessageType {
  case object Error extends MessageType
  case object Warning extends MessageType
}

object ErrorReporter {
  def error(file: SourceFile, span: Span, message: String) =
    print(MessageType.Error, file, span, message)

  def warning(file: SourceFile, span: Span, message: String) =
    print(MessageType.Warning, file, span, message)

  def print(ty: MessageType, file: SourceFile, span: Span, message: String) = {
    val (color, heading) = ty match {
      case MessageType.Error   => (RED, "Error")
      case MessageType.Warning => (YELLOW, "Warning")
    }

    val Loc(line, col) = file.locationOfCharPos(span.start)
    val lineStr = file.getSourceAtLine(line).stripLineEnd
    val spanLen = span.end - span.start

    // Highlight the span of the source text.
    val preHighlightStr = lineStr.slice(0, col - 1)
    val highlightStr = color + lineStr.slice(col - 1, col - 1 + spanLen) + RESET
    val postHighlightStr = lineStr.substring(col - 1 + spanLen)

    // Generate the indent for the indicator line (e.g. ^^) so that
    // even input with \t should align.
    val indent = preHighlightStr.replaceAll("[^\t]", " ")

    val lineNumStrLen = line.toString.length
    val border = BOLD + BLUE + "  " + " " * lineNumStrLen + " | " + RESET

    System.err.println(
      s"$BOLD$color$heading:$RESET$BOLD $message$RESET"
    )
    System.err.println(
      s"$BOLD$BLUE  " + " " * lineNumStrLen + " = " + s"${file.name}:${line}:${col}$RESET"
    )
    System.err.println(border)
    System.err.println(
      s"$BOLD$BLUE  $line | $RESET$preHighlightStr$highlightStr$postHighlightStr"
    )
    System.err.println(border + indent + color + ("^" * spanLen) + RESET)
  }
}
