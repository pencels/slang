package slang.runtime

import slang.parse.AstPrinter

import scala.collection._

class Environment(val parent: Environment) {
  final private val environment = new mutable.HashMap[String, Value]

  def this() = {
    this(null)
  }

  def define(name: String, value: Value): Unit = environment.put(name, value)

  def get(name: String): Value = tryGet(name) match {
    case Some(value) => value
    case _ => throw new RuntimeException("Unbound name '" + name + "'.")
  }

  def tryGet(name: String): Option[Value] = (environment.get(name), parent) match {
    case (Some(v), _) => Some(v)
    case (None, null) => None
    case (None, p) => p.tryGet(name)
  }

  def `with`(name: String, value: Value): Environment = {
    environment.put(name, value)
    this
  }

  def set(name: String, value: Value): Unit = {
    if (environment.contains(name))
      environment.put(name, value)
    else if (parent == null)
      throw new RuntimeException("Name '" + name + "' assigned to but has not been declared.")
    else parent.set(name, value)
  }

  def shortString: String = {
    val keyValPairs = environment.map({ case (name, value) =>
      val valueStr = value match {
        case _: Matchbox => "<Matchbox>"
        case _: Hashbox => "<Hashbox>"
        case _: Lazy => "{ ... }"
        case _ => new AstPrinter().print(value)
      }
      s"$name=$valueStr"
    }).toList

    val maybeTruncatedPairs = if (environment.size > 5) ("..." :: keyValPairs.takeRight(4)) else keyValPairs
    
    maybeTruncatedPairs.mkString("[", ", ", "]")
  }

  def collapsedString: String = {
    "{ <env> }"
  }

  override def toString: String = shortString

  def isEmpty: Boolean = environment.isEmpty

  def update(newBindings: Environment): Unit = environment ++= newBindings.environment
}