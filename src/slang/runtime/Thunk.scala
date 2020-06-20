package slang.runtime

case class Thunk(var fullStrictValue: Option[Value], var strictValue: Option[Value], unevaluatedValue: Value)

object Thunk {
  def from(value: Value) = Thunk(None, None, value)
}
