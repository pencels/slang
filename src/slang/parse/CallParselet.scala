package slang.parse

import scala.collection._

import slang.lex.Token
import slang.lex.TokenType

class CallParselet extends InfixParselet {
    override def parse(parser: Parser, left: Expr, token: Token): Expr = {
        val args: mutable.ListBuffer[Expr] = new mutable.ListBuffer[Expr]

        // There may not be any arguments.
        do {
            args.addOne(parser.expression(getPrecedence))
        } while (parser.currentPrecedence >= getPrecedence)

        Expr.Call(left, args.toList)
    }

    override def getPrecedence: Int = Precedence.CALL
}
