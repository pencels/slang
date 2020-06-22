package slang.parse

object Precedence extends Enumeration {
    type Precedence = Int

    val SEQUENCE = 1
    val ASSIGNMENT = 2
    val APPLY = 3
    val CONDITIONAL = 4
    val SUM = 5
    val PRODUCT = 6
    val EXPONENT = 7
    val PREFIX = 8
    val POSTFIX = 9
    val CALL = 10
}
