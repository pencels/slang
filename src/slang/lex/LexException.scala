package slang.lex

class LexException(char: Char, loc: Loc) extends RuntimeException {
    override def getMessage(): String = {
        s"[line ${loc.line}, col ${loc.col}] Unexpected character: '${char}'"
    }
}