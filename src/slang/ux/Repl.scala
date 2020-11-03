package slang.ux

import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.reader.{
  LineReader,
  LineReaderBuilder,
  UserInterruptException,
  EndOfFileException
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

  val reader: LineReader =
    LineReaderBuilder
      .builder()
      .terminal(terminal)
      .build()

  /**
    * Runs the REPL.
    */
  def run(): Unit = {
    while (true) {
      try {
        val line = reader.readLine(prompt)
        println(s"You said: $line")
      } catch {
        case _: UserInterruptException =>
          println("Keyboard interrupt!")
        case _: EndOfFileException => return
      }
    }
  }
}
