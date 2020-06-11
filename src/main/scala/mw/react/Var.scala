package mw.react

import scala.concurrent.ExecutionContext

trait Var[T] extends Def[T] {
  def update(expr: => T): Unit
  def publish(t: T): Unit
  def close(): Unit
  def fail(error: Throwable): Unit
}
object Var {
  def apply[T](implicit exec: ExecutionContext): Var[T] = new Def.Implementation[T] with Var[T] {
    override def update(expr: => T) = super.update(expr)
    override def publish(t: T) = super.publish(t)
    override def close() = super.close()
    override def fail(error: Throwable) = super.fail(error)
  }
}