package slang.runtime

import java.util.List

import slang.parse.Stmt

class Lazy(val statements: List[Stmt], val environment: Environment) extends Value {
    private var value: Value = null

    def getValue(interpreter: Interpreter): Value = {
        value = interpreter interpret(statements, environment)
        value
    }

    def getCachedValue(interpreter: Interpreter): Value = {
        if (value == null) {
            value = getValue(interpreter)
        }

        value
    }

    override def toString() = {
        var repr = environment.collapsedString();
        repr += " { ... }";
        repr
    }

    override def getType() = "Lazy"
    override def asString() = toString()
}
