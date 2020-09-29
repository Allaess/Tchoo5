package mw.react

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait Val[+T] extends Def[T] {
  override def map[S](f: T => S): Val[S]
  override def withFilter(p: T => Boolean): Val[T]
  def flatMap[S](f: T => Val[S]): Val[S]
  override def collect[S](pf: PartialFunction[T, S]) = withFilter(pf.isDefinedAt).map(pf)
  override def scan[S](init: S)(f: (S, T) => S) = {
    var s = init
    map { t =>
      s = f(s, t)
      s
    }
  }
  def flatten[S](implicit id: T => Val[S]) = flatMap(id)
}
object Val extends LowPriorityImplicits {
  def apply[T](future: Future[T])(implicit exec: ExecutionContext) = FromFuture(future)
  class Impl[T](implicit exec: ExecutionContext) extends Val[T] {
    source =>
    private val promise = Promise[Option[T]]()
    def subscribe(subscriber: Subscriber[T]) = {
      var cancelled = false
      subscriber.onCancel {
        cancelled = true
      }
      promise.future.onComplete {
        case Success(Some(t)) =>
          subscriber.published(t)
          subscriber.closed()
        case Success(None) =>
          subscriber.closed()
        case Failure(error) =>
          subscriber.failed(error)
      }
    }
    def foreach(action: T => Unit) = promise.future.onComplete {
      case Success(Some(t)) => Try(action(t))
      case _ =>
    }
    def map[S](f: T => S): Val[S] = new Val.Impl[S] {
      result =>
      source.subscribe(new Subscriber[T] {
        def onCancel(action: => Unit) = {}
        def published(t: T) = Try(f(t)) match {
          case Success(s) => result.publish(s)
          case Failure(e) => result.fail(e)
        }
        def closed() = result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Mapped${super.toString} <<< $source"
    }
    def withFilter(p: T => Boolean): Val[T] = new Val.Impl[T] {
      result =>
      source.subscribe(new Subscriber[T] {
        def onCancel(action: => Unit) = {}
        def published(t: T) = Try(p(t)) match {
          case Success(true) => result.publish(t)
          case Success(false) => result.close()
          case Failure(error) => result.fail(error)
        }
        def closed() = result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Filtered${super.toString} <<< $source"
    }
    def flatMap[S](f: T => Val[S]): Val[S] = new Val.Impl[S] {
      result =>
      source.subscribe(new Subscriber[T] {
        private var hasSub = false
        def onCancel(action: => Unit) = {}
        def published(t: T) = Try(f(t)) match {
          case Success(publisher) =>
            hasSub = true
            publisher.subscribe(new Subscriber[S] {
              def onCancel(action: => Unit) = {}
              def published(s: S) = result.publish(s)
              def closed() = result.close()
              def failed(error: Throwable) = result.fail(error)
            })
          case Failure(error) =>
            result.fail(error)
        }
        def closed() = if (!hasSub) result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Flatmapped${super.toString} <<< $source"
    }
    def flatMap[S](f: T => Def[S]): Def[S] = new Def.Impl[S] {
      result =>
      source.subscribe(new Subscriber[T] {
        private var hasSub = false
        def onCancel(action: => Unit) = {}
        def published(t: T) = Try(f(t)) match {
          case Success(publisher) =>
            hasSub = true
            publisher.subscribe(new Subscriber[S] {
              def onCancel(action: => Unit) = {}
              def published(s: S) = result.publish(s)
              def closed() = result.close()
              def failed(error: Throwable) = result.fail(error)
            })
          case Failure(error) =>
            result.fail(error)
        }
        def closed() = if (!hasSub) result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Flatmapped${super.toString} <<< $source"
    }
    def head: Val[T] = this
    protected def update(expr: => T) = promise.tryComplete(Try(Some(expr)))
    protected def publish(t: T) = promise.trySuccess(Some(t))
    protected def close() = promise.trySuccess(None)
    protected def fail(error: Throwable) = promise.tryFailure(error)
    override def toString = promise.future.value match {
      case Some(Success(t)) => s"Val($t)"
      case Some(Failure(e)) => s"Val(<$e>)"
      case None => "Val(<...>)"
    }
  }
  implicit class FromFuture[T](future: Future[T])(implicit exec: ExecutionContext) extends Val.Impl[T] {
    result =>
    future.onComplete {
      case Success(t) => result.publish(t)
      case Failure(e) => result.fail(e)
    }
  }
  case class Value[T](t: T) extends Val[T] {
    def subscribe(subscriber: Subscriber[T]) = {
      subscriber.onCancel {}
      subscriber.published(t)
      subscriber.closed()
    }
    def foreach(action: T => Unit) = Try(action(t))
    def map[S](f: T => S): Val[S] = Try(f(t)) match {
      case Success(s) => Value(s)
      case Failure(e) => Failed(e)
    }
    def withFilter(p: T => Boolean): Val[T] = Try(p(t)) match {
      case Success(true) => this
      case Success(false) => Closed
      case Failure(error) => Failed(error)
    }
    def flatMap[S](f: T => Val[S]): Val[S] = Try(f(t)) match {
      case Success(publisher) => publisher
      case Failure(error) => Failed(error)
    }
    def flatMap[S](f: T => Def[S]): Def[S] = Try(f(t)) match {
      case Success(publisher) => publisher
      case Failure(error) => Failed(error)
    }
    def head: Val[T] = this
    override def toString = s"Val($t)"
  }
  case class Failed(error: Throwable) extends Val[Nothing] {
    def subscribe(subscriber: Subscriber[Nothing]) = {
      subscriber.onCancel {}
      subscriber.failed(error)
    }
    def foreach(action: Nothing => Unit) = {}
    def map[S](f: Nothing => S) = this
    def withFilter(p: Nothing => Boolean) = this
    def flatMap[S](f: Nothing => Val[S]) = this
    def flatMap[S](f: Nothing => Def[S]): Def[S] = this
    def head: Val[Nothing] = this
    override def toString = s"Val(<$error>)"
  }
  object Closed extends Val[Nothing] {
    def subscribe(subscriber: Subscriber[Nothing]) = {
      subscriber.onCancel {}
      subscriber.closed()
    }
    def foreach(action: Nothing => Unit) = {}
    def map[S](f: Nothing => S) = this
    def withFilter(p: Nothing => Boolean) = this
    def flatMap[S](f: Nothing => Val[S]) = this
    def flatMap[S](f: Nothing => Def[S]): Def[S] = this
    def head: Val[Nothing] = this
    override def toString = "Val()"
  }
}
