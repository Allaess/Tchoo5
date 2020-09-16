package mw.react

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class Var[T](implicit exec: ExecutionContext) extends Def.Impl[T] {
  override def update(expr: => T) = super.update(expr)
  override def publish(t: T) = super.publish(t)
  override def close() = super.close()
  override def fail(error: Throwable) = super.fail(error)
  override def toString = current.future.value match {
    case Some(Success(list)) => s"Var($list)"
    case Some(Failure(error)) => s"Var(<$error>)"
    case None => "Var(<...>)"
  }
}
object Var {
  def apply[T](implicit exec: ExecutionContext) = new Var[T]
}
