package mw.react

trait LowPriorityImplicits {
  def apply[T](value: T) = Val.Value(value)
}
