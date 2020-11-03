package slang

import slang.ux.Repl

object Slang {
  def main(args: Array[String]): Unit = {
    val repl = new Repl
    repl.run()
  }
}
