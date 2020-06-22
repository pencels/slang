package slang.runtime

import slang.lex.Token

class RuntimeError(val token: Token, val reason: String) extends RuntimeException(reason)
