package mw.react

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait Val[+T] extends Def[T] {
  def toFuture: Future[T]
  //  def map[S](f: T => S): Val[S]
  //  def withFilter(p: T => Boolean): Val[T]
  //  def collect[S](pf: PartialFunction[T, S]): Val[S]
  //  def scan[S](init: S)(f: (S, T) => S): Val[S]
}
object Val {
  def apply[T](expr: => T)(implicit exec: ExecutionContext): Val[T] = new Impl(expr)
  implicit class FromFuture[T](val toFuture: Future[T])(implicit val exec: ExecutionContext) extends Val[T] {
    def subscribe(subscriber: Subscriber[T]) = {
      var cancelled = false
      subscriber.onCancel {
        cancelled = true
      }
      toFuture.onComplete {
        case _ if cancelled =>
        case Success(t) =>
          subscriber.published(t)
          subscriber.closed()
        case Failure(e) =>
          subscriber.failed(e)
      }
    }
    override def map[S](f: T => S): Val[S] = toFuture.map(f)
    override def withFilter(p: T => Boolean): Val[T] = toFuture.withFilter(p)
    override def collect[S](pf: PartialFunction[T, S]): Val[S] = toFuture.collect(pf)
    override def scan[S](init: S)(f: (S, T) => S): Val[S] = map(f(init, _))
    override def toString = toFuture.value match {
      case Some(Success(t)) => s"Val($t)"
      case Some(Failure(e)) => s"Val(<$e>)"
      case None => "Val(<?>)"
    }
  }
  class Impl[T](expr: => T)(implicit val exec: ExecutionContext) extends Val[T] {
    private val value = Try(expr)
    lazy val toFuture = Future.fromTry(value)
    def subscribe(subscriber: Subscriber[T]) = {
      subscriber.onCancel {}
      value match {
        case Success(t) =>
          subscriber.published(t)
          subscriber.closed()
        case Failure(e) =>
          subscriber.failed(e)
      }
    }
    override def map[S](f: T => S): Val[S] = Val(f(value.get))
    override def withFilter(p: T => Boolean): Val[T] = Try(p(value.get)) match {
      case Success(true) => this
      case Success(false) => new Closed
      case Failure(error) => Val(throw error)
    }
    override def collect[S](pf: PartialFunction[T, S]): Val[S] = Try(pf.lift(value.get)) match {
      case Success(Some(s)) => Val(s)
      case Success(None) => new Closed
      case Failure(error) => Val(throw error)
    }
    override def scan[S](init: S)(f: (S, T) => S): Val[S] = Val(f(init, value.get))
    override def toString = value match {
      case Success(t) => s"Val($t)"
      case Failure(e) => s"Val(<$e>)"
    }
  }
  class Closed(implicit val exec: ExecutionContext) extends Val[Nothing] {
    def toFuture: Future[Nothing] = Future.never
    def subscribe(subscriber: Subscriber[Nothing]) = {
      subscriber.onCancel {}
      subscriber.closed()
    }
    override def map[S](f: Nothing => S) = this
    override def withFilter(p: Nothing => Boolean) = this
    override def collect[S](pf: PartialFunction[Nothing, S]) = this
    override def scan[S](init: S)(f: (S, Nothing) => S) = this
    override def toString = "Def(<->)"
  }
}
