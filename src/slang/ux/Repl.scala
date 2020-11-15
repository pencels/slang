package slang.ux

import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.reader.{
  LineReader,
  LineReaderBuilder,
  UserInterruptException,
  EndOfFileException
}

import org.jline.reader.impl.DefaultParser

import slang.Slang
import slang.parse._
import slang.lex._
import org.jline.builtins.Completers.Completer
import org.jline.reader.Expander
import org.jline.reader.History

class ReplExpander extends Expander {
  override def expandHistory(history: History, str: String): String = str
  override def expandVar(x: String): String = x
}

/**
  * A REPL opened on the system terminal.
  */
class Repl {
  val prompt = "~ "

  val terminal: Terminal =
    TerminalBuilder
      .builder()
      .system(true)
      .build()

  val parser = new DefaultParser
  parser.setEscapeChars(Array.empty)

  val reader: LineReader =
    LineReaderBuilder
      .builder()
      .terminal(terminal)
      .expander(new ReplExpander)
      .parser(parser)
      .variable(LineReader.SECONDARY_PROMPT_PATTERN, "| ")
      .build()

  /**
    * Runs the REPL.
    */
  def run(): Unit = {
    val operatorTrie = new OperatorTrie

    operatorTrie.add("-")

    while (true) {
      val input =
        try {
          reader.readLine(prompt)
        } catch {
          case _: UserInterruptException =>
            System.err.println("Keyboard interrupt!")
            ""
          case _: EndOfFileException => return
        }

      val file = new SourceFile("<stdin>", input)
      val lexer = new Lexer(file, operatorTrie)

      try {
        for (token <- lexer) {
          pprint.pprintln(token)
        }
      } catch {
        case LexerException(span, msg) => ErrorReporter.error(file, span, msg)
        case ParserException(tok, msg) =>
          ErrorReporter.error(file, tok.span, msg)
      }
    }
  }
}
