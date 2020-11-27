package slang.runtime

import scala.collection.mutable
import slang.sourcemap.Span

class Environment(parent: Option[Environment] = None) {
  private val variables = mutable.Map[String, Value]()

  def inLocalScope(name: String): Boolean = variables contains name

  def define(name: String, value: Value)(implicit stack: List[Span]): Unit = {
    if (inLocalScope(name)) {
      throw new RuntimeError(
        stack,
        s"Variable '$name' has already been declared."
      )
    } else {
      variables += (name -> value)
    }
  }

  def set(name: String, value: Value)(implicit stack: List[Span]): Unit = {
    if (inLocalScope(name)) {
      variables += (name -> value)
    } else {
      parent match {
        case Some(parent) => parent.set(name, value)
        case None =>
          throw new RuntimeError(
            stack,
            s"Variable '$name' has not been declared in this scope."
          )
      }
    }
  }

  def get(name: String)(implicit stack: List[Span]): Value = {
    tryGet(name) match {
      case Some(value) => value
      case None =>
        throw new RuntimeError(stack, s"Variable '$name' is not in scope.")
    }
  }

  def tryGet(name: String): Option[Value] = {
    if (inLocalScope(name)) {
      variables.get(name)
    } else {
      parent.flatMap(_.tryGet(name))
    }
  }
}

object Environment {
  def fresh(env: Environment) = new Environment(Some(env))
  def empty = new Environment(None)
}
