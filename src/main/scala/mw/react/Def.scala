package mw.react

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait Def[+T] {
  def subscribe(subscriber: Subscriber[T]): Unit
  def foreach(action: T => Unit): Unit
  def map[S](f: T => S): Def[S]
  def withFilter(p: T => Boolean): Def[T]
  def flatMap[S](f: T => Def[S]): Def[S]
  def collect[S](pf: PartialFunction[T, S]) = withFilter(pf.isDefinedAt).map(pf)
  def scan[S](init: S)(f: (S, T) => S) = {
    var s = init
    map { t =>
      s = f(s, t)
      s
    }
  }
  def flatten[S](implicit id: T => Def[S]) = flatMap(id)
}
object Def {
  class Impl[T](implicit exec: ExecutionContext) extends Def[T] {
    source =>
    protected var current = Promise[AList]()
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
    def foreach(action: T => Unit) = source.subscribe(new Subscriber[T] {
      private var cancel = { () => }
      private var cancelled = false
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!cancelled && Try(action(t)).isFailure) {
        cancelled = true
        cancel()
      }
      def closed() = {}
      def failed(error: Throwable) = {}
      override def toString = s"Subscriber($source)"
    })
    def map[S](f: T => S): Def[S] = new Def.Impl[S] {
      result =>
      source.subscribe(new Subscriber[T] {
        private var cancel = { () => }
        def onCancel(action: => Unit) = cancel = { () => action }
        def published(t: T) = Try(f(t)) match {
          case Success(s) =>
            result.publish(s)
          case Failure(e) =>
            result.fail(e)
            cancel()
        }
        def closed() = result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Mapped${super.toString} <<< $source"
    }
    def withFilter(p: T => Boolean): Def[T] = new Def.Impl[T] {
      result =>
      source.subscribe(new Subscriber[T] {
        private var cancel = { () => }
        def onCancel(action: => Unit) = cancel = { () => action }
        def published(t: T) = Try(p(t)) match {
          case Success(true) =>
            result.publish(t)
          case Success(false) =>
          case Failure(error) =>
            result.fail(error)
            cancel()
        }
        def closed() = result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Filtered${super.toString} <<< $source"
    }
    def flatMap[S](f: T => Def[S]): Def[S] = new Def.Impl[S] {
      result =>
      source.subscribe(new Subscriber[T] {
        private var cancelMain = { () => }
        private var cancelSub = { () => }
        private var current: Def[S] = Silent
        private var closing = false
        def onCancel(action: => Unit) = cancelMain = { () => action }
        def published(t: T) = Try(f(t)) match {
          case Success(publisher) =>
            current = publisher
            publisher.subscribe(new Subscriber[S] {
              def onCancel(action: => Unit) =
                if (current == publisher) cancelSub = { () => action }
                else action
              def published(s: S) = if (current == publisher) result.publish(s)
              def closed() = if (current == publisher) {
                current = Silent
                if (closing) result.close()
              }
              def failed(error: Throwable) = if (current == publisher) {
                result.fail(error)
                cancelMain()
              }
            })
          case Failure(error) =>
            result.fail(error)
            cancelMain()
            cancelSub()
        }
        def closed() = {
          closing = true
          if (current == Silent) result.close()
        }
        def failed(error: Throwable) = {
          result.fail(error)
          cancelSub()
        }
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"FlatMapped${super.toString} <<< $source"
    }
    protected def update(expr: => T) = append(current, Try(ACons(expr)))
    protected def publish(t: T) = append(current, Success(ACons(t)))
    protected def close() = append(current, Success(ANil))
    protected def fail(error: Throwable) = append(current, Failure(error))
    @tailrec private def append(promise: Promise[AList], elem: Try[AList]): Unit =
      if (promise.tryComplete(elem)) current = promise
      else promise.future.value match {
        case Some(Success(ACons(_, tail))) => append(tail, elem)
        case _ =>
      }
    sealed trait AList
    case object ANil extends AList
    case class ACons(head: T, tail: Promise[AList] = Promise[AList]()) extends AList {
      override def toString = tail.future.value match {
        case Some(Success(list)) => s"$head::$list"
        case Some(Failure(error)) => s"$head::<$error>"
        case None => s"$head::<...>"
      }
    }
    override def toString = current.future.value match {
      case Some(Success(list)) => s"Def($list)"
      case Some(Failure(error)) => s"Def(<$error>)"
      case None => s"Def(<...>)"
    }
  }
  object Silent extends Def[Nothing] {
    def subscribe(subscriber: Subscriber[Nothing]) = {}
    def foreach(action: Nothing => Unit) = {}
    def map[S](f: Nothing => S) = this
    def withFilter(p: Nothing => Boolean) = this
    def flatMap[S](f: Nothing => Def[S]) = this
    override def toString = "Def.Silent"
  }
}
