package mw.react

import mw.react
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait Def[+T] {
  source =>
  def subscribe(subscriber: Subscriber[T]): Unit
  def foreach(action: T => Unit): Unit = subscribe(new Subscriber[T] {
    private var cancelled = false
    private var cancel: () => Unit = { () => }
    def onCancel(action: => Unit) = cancel = { () => action }
    def published(t: T) = if (!cancelled) Try(action(t)) match {
      case Failure(_) =>
        cancelled = true
        cancel()
        cancel = { () => }
      case Success(_) =>
    }
    def closed() = {}
    def failed(error: Throwable) = {}
  })
  def map[S](f: T => S)(implicit exec: ExecutionContext): Def[S] = new Def.Implementation[S] {
    result =>
    source.subscribe(new Subscriber[T] {
      private var cancelled = false
      private var cancel: () => Unit = { () => }
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!cancelled) Try(f(t)) match {
        case Success(s) =>
          result.publish(s)
        case Failure(e) =>
          cancelled = true
          result.fail(e)
          cancel()
          cancel = { () => }
      }
      def closed() = if (!cancelled) {
        result.close()
        cancel = { () => }
      }
      def failed(error: Throwable) = if (!cancelled) {
        result.fail(error)
        cancel = { () => }
      }
    })
  }
  def withFilter(p: T => Boolean)(implicit exec: ExecutionContext): Def[T] = new react.Def.Implementation[T] {
    result =>
    source.subscribe(new Subscriber[T] {
      private var cancelled = false
      private var cancel: () => Unit = { () => }
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!cancelled) Try(p(t)) match {
        case Success(true) =>
          result.publish(t)
        case Success(false) =>
        case Failure(error) =>
          cancelled = true
          result.fail(error)
          cancel()
          cancel = { () => }
      }
      def closed() = if (!cancelled) result.close()
      def failed(error: Throwable) = if (!cancelled) result.fail(error)
    })
  }
  def collect[S](pf: PartialFunction[T, S])(implicit exec: ExecutionContext): Def[S] =
    withFilter(pf.isDefinedAt).map(pf)
  def scan[S](init: S)(f: (S, T) => S)(implicit exec: ExecutionContext): Def[S] = {
    var s = init
    map { t =>
      s = f(s, t)
      s
    }
  }
  def merge[S >: T](that: Def[S])(implicit exec: ExecutionContext): Def[S] = new Def.Implementation[S] {
    result =>
    private val events = Var[Def.Event[S]]
    events.subscribe(new Subscriber[Def.Event[S]] {
      private var cancel: () => Unit = { () => }
      private var count = 3 - 1
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(event: Def.Event[S]) = event match {
        case Def.OnCancel(d, action) =>
        case Def.Published(d, s) =>
        case Def.Closed(d) =>
        case Def.Failed(d, e) =>
      }
      def closed() = ???
      def failed(error: Throwable) = ???
    })
    source.subscribe(new Def.EventSubscriber[S](events, source))
    that.subscribe(new Def.EventSubscriber[S](events, that))
  }
  def flatMap[S](f: T => Def[S])(implicit exec: ExecutionContext): Def[S] = ???
  def flatten[S](implicit id: T => Def[S], exec: ExecutionContext): Def[S] = flatMap(id)
}
object Def {
  class Implementation[T](implicit exec: ExecutionContext) extends Def[T] {
    private var current = Promise[AList]
    protected def update(expr: => T) = append(current, Try(ACons(expr)))
    protected def publish(t: T) = append(current, Success(ACons(t)))
    protected def close() = append(current, Success(ANil))
    protected def fail(error: Throwable) = append(current, Failure(error))
    private def append(promise: Promise[AList], value: Try[AList]): Unit =
      if (promise.tryComplete(value)) current = promise
      else promise.future.value match {
        case Some(Success(ACons(_, tail))) => append(tail, value)
        case _ => current = promise
      }
    def subscribe(subscriber: Subscriber[T]) = {
      var cancelled = false
      subscriber.onCancel {
        cancelled = true
      }
      monitor(current.future)
      def monitor(future: Future[AList]): Unit = future.onComplete {
        case _ if cancelled =>
        case Success(ACons(head, tail)) =>
          subscriber.published(head)
          monitor(tail.future)
        case Success(ANil) =>
          subscriber.closed()
        case Failure(error) =>
          subscriber.failed(error)
      }
    }
    sealed trait AList
    case object ANil extends AList
    case class ACons(head: T, tail: Promise[AList] = Promise[AList]) extends AList {
      override def toString = tail.future.value match {
        case Some(Success(tail)) => s"$head::$tail"
        case Some(Failure(error)) => s"$head::<$error>"
        case None => s"$head::<?>"
      }
    }
    override def toString = current.future.value match {
      case Some(Success(aList)) => s"Def($aList)"
      case Some(Failure(error)) => s"Def(<$error>)"
      case None => "Def(<?>)"
    }
  }
  sealed trait Event[+T]
  case class OnCancel[+T](source: Def[T], action: () => Unit) extends Event[T]
  case class Published[+T](source: Def[T], t: T) extends Event[T]
  case class Closed[+T](source: Def[T]) extends Event[T]
  case class Failed[+T](source: Def[T], error: Throwable) extends Event[T]
  class EventSubscriber[-T](events: Var[Event[T]], source: Def[T]) extends Subscriber[T] {
    def onCancel(action: => Unit) = events.publish(OnCancel(source, { () => action }))
    def published(t: T) = events.publish(Published(source, t))
    def closed() = events.publish(Closed(source))
    def failed(error: Throwable) = events.publish(Failed(source, error))
  }
}
