package slang.runtime


import slang.parse.Stmt

case class Lazy(statements: List[Stmt], environment: Environment) extends Value {
    private var value: Value = _

    def getValue(interpreter: Interpreter): Value = {
        value = interpreter.interpret(environment, statements)
        value
    }

    def getCachedValue(interpreter: Interpreter): Value = {
        if (value == null) {
            value = getValue(interpreter)
        }

        value
    }

    override def toString: String = {
        var repr = environment.collapsedString
        repr += " { ... }"
        repr
    }

    override def getType: String = "Lazy"
    override def asString: String = toString()
    override def isElemental: Boolean = false
}
