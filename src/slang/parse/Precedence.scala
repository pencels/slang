package slang.parse

object Precedence extends Enumeration {
    type Precedence = Int

    val SEQUENCE = 1
    val CALL = 998
    val PREFIX = 999
    val POSTFIX = 1000
}

sealed trait Associativity
object Associativity {
    case object Left extends Associativity
    case object Right extends Associativity
    case object NonAssoc extends Associativity
}