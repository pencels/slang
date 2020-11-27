package slang.runtime

import scala.collection.mutable
import slang.sourcemap.Span

class Environment {
  private val variables = mutable.Map[String, Value]()

  def define(name: String, value: Value)(implicit stack: List[Span]): Unit = {
    if (variables contains name) {
      throw new RuntimeError(
        stack,
        s"Variable '$name' has already been declared."
      )
    } else {
      variables += (name -> value)
    }
  }

  def set(name: String, value: Value)(implicit stack: List[Span]): Unit = {
    if (variables contains name) {
      variables += (name -> value)
    } else {
      throw new RuntimeError(
        stack,
        s"Variable '$name' has not been declared in this scope."
      )
    }
  }

  def get(name: String)(implicit stack: List[Span]): Value = {
    tryGet(name) match {
      case Some(value) => value
      case None =>
        throw new RuntimeError(stack, s"Variable '$name' is not in scope.")
    }
  }

  def tryGet(name: String): Option[Value] = variables.get(name)
}
