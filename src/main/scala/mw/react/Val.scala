package mw.react

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait Val[+T] extends Def[T] {
  source =>
  def map[S](f: T => S): Val[S]
  def withFilter(p: T => Boolean): Val[T]
  def collect[S](pf: PartialFunction[T, S]): Val[S] = withFilter(pf.isDefinedAt).map(pf)
  def scan[S](init: S)(f: (S, T) => S): Val[S] = map(f(init, _))
}
object Val {
  val silent: Val[Nothing] = new Val[Nothing] {
    def map[S](f: Nothing => S): Val[S] = this
    def withFilter(p: Nothing => Boolean): Val[Nothing] = this
    def subscribe(subscriber: Subscriber[Nothing]) = {}
  }
  val closed: Val[Nothing] = new Val[Nothing] {
    def map[S](f: Nothing => S): Val[S] = this
    def withFilter(p: Nothing => Boolean): Val[Nothing] = this
    def subscribe(subscriber: Subscriber[Nothing]) = subscriber.closed()
  }
  def failed(error: Throwable): Val[Nothing] = new Val[Nothing] {
    def map[S](f: Nothing => S): Val[S] = this
    def withFilter(p: Nothing => Boolean): Val[Nothing] = this
    def subscribe(subscriber: Subscriber[Nothing]) = subscriber.failed(error)
  }
  def apply[T](expr: => T): Val[T] = new Val[T] {
    private val value = Try(expr)
    def subscribe(subscriber: Subscriber[T]) = value match {
      case Success(t) =>
        subscriber.published(t)
        subscriber.closed()
      case Failure(e) =>
        subscriber.failed(e)
    }
    def map[S](f: T => S) = Val(f(value.get))
    def withFilter(p: T => Boolean) = Try(p(value.get)) match {
      case Success(true) => this
      case Success(false) => closed
      case Failure(error) => failed(error)
    }
  }
  implicit class FromFuture[T](future: Future[T])(implicit exec: ExecutionContext) extends Val[T] {
    def subscribe(subscriber: Subscriber[T]) = {
      var cancelled = false
      subscriber.onCancel {
        cancelled = true
      }
      future.onComplete {
        case _ if cancelled =>
        case Success(t) =>
          subscriber.published(t)
          subscriber.closed()
        case Failure(e) =>
          subscriber.failed(e)
      }
    }
    override def map[S](f: T => S): Val[S] = future.map(f)
    def withFilter(p: T => Boolean): Val[T] = future.withFilter(p)
  }
}
