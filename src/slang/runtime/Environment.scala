package slang.runtime

import scala.collection.mutable

class Environment {
  private val variables = mutable.Map[String, Value]()
}
