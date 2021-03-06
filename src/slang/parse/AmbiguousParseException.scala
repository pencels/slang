package slang.parse

import slang.lex.Token

class AmbiguousParseException(token: Token, exceptions: List[ParseException]) extends RuntimeException {
  override def getMessage: String = {
    val line = token.loc.line
    val col = token.loc.col

    s"""
       |[line $line, col $col] Ambiguous parse results:
       |${exceptions.map(_.getMessage).mkString(" * ", "\n * ", "\n")}
       |""".stripMargin
  }
}
