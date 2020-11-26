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
import slang.sourcemap.SourceFile
import org.jline.builtins.Completers.Completer
import org.jline.reader.Expander
import org.jline.reader.History
import slang.runtime.Interpreter
import slang.sourcemap.SourceMap

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
    val sourceMap = new SourceMap

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

      var lastPos = sourceMap.lastPos

      sourceMap.addVirtualFile("<stdin>", input)

      val reporter = new ConsoleErrorReporter(sourceMap)
      val context =
        new ParseContext(sourceMap, lastPos, reporter, operatorTrie)
      val lexer = new Lexer(context)
      var parser = new Parser(lexer, context)

      val exprs = parser.toList.flatten

      if (reporter.errors.isEmpty) {
        Interpreter.interpret(exprs, reporter)
      }
    }
  }
}
