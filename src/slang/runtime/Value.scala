package slang.runtime

sealed trait Value

object Value {
  case class Number(num: Double) extends Value
  case object Nothing extends Value
}
