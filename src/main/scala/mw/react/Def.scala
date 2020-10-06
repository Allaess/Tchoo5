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
  def head: Val[T]
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
      current.future.value match {
        case Some(Success(ACons(head, tail))) =>
          subscriber.published(head)
          monitor(tail.future)
        case Some(Success(ANil)) =>
          subscriber.closed()
        case Some(Failure(error)) =>
          subscriber.failed(error)
        case None =>
          monitor(current.future)
      }
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
      private val events = Var[Event]
      events.subscribe(new Subscriber[Event] {
        private var cancelMain = { () => }
        private var cancelSub = { () => }
        private var current: Def[S] = Def.Silent
        private var closing = false
        def onCancel(action: => Unit) = {}
        def published(event: Event) = event match {
          case OnMainCancel(action) =>
            cancelMain = action
          case MainPublished(t) => Try(f(t)) match {
            case Success(sub) =>
              current = sub
              sub.subscribe(new Subscriber[S] {
                def onCancel(action: => Unit) = events.publish(OnSubCancel(sub, { () => action }))
                def published(s: S) = events.publish(SubPublished(sub, s))
                def closed() = events.publish(SubClosed(sub))
                def failed(error: Throwable) = events.publish(SubFailed(sub, error))
              })
            case Failure(err) =>
              events.fail(err)
              cancelMain()
              cancelSub()
          }
          case MainClosed =>
            closing = true
            if (current == Def.Silent) events.close()
          case MainFailed(error) =>
            events.fail(error)
            cancelSub()
          case OnSubCancel(publisher, action) =>
            if (current == publisher) cancelSub = action
            else action()
          case SubPublished(publisher, s) =>
            if (current == publisher) result.publish(s)
          case SubClosed(publisher) =>
            if (current == publisher) {
              current = Def.Silent
              if (closing) result.close()
            }
          case SubFailed(publisher, error) =>
            if (current == publisher) {
              result.fail(error)
              cancelMain()
            }
        }
        def closed() = result.close()
        def failed(error: Throwable) = result.fail(error)
      })
      source.subscribe(new Subscriber[T] {
        def onCancel(action: => Unit) = events.publish(OnMainCancel { () => action })
        def published(t: T) = events.publish(MainPublished(t))
        def closed() = events.publish(MainClosed)
        def failed(error: Throwable) = events.publish(MainFailed(error))
      })
      sealed trait Event
      case class OnMainCancel(action: () => Unit) extends Event
      case class MainPublished(t: T) extends Event
      case object MainClosed extends Event
      case class MainFailed(error: Throwable) extends Event
      case class OnSubCancel(publisher: Def[S], action: () => Unit) extends Event
      case class SubPublished(publisher: Def[S], s: S) extends Event
      case class SubClosed(publisher: Def[S]) extends Event
      case class SubFailed(publisher: Def[S], error: Throwable) extends Event
    }
    def head: Val[T] = new Val.Impl[T] {
      result =>
      source.subscribe(new Subscriber[T] {
        private var cancel = { () => }
        def onCancel(action: => Unit) = cancel = { () => action }
        def published(t: T) = {
          result.publish(t)
          cancel()
        }
        def closed() = result.close()
        def failed(error: Throwable) = result.fail(error)
        override def toString = s"Subscriber($result)"
      })
      override def toString = s"Head${super.toString} <<< $source"
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
    def head = Val.Closed
    override def toString = "Def.Silent"
  }
  implicit class FromSeq[T](seq: Seq[T]) extends Def[T] {
    def subscribe(subscriber: Subscriber[T]) = {
      subscriber.onCancel {}
      for (t <- seq) subscriber.published(t)
      subscriber.closed()
    }
    def foreach(action: T => Unit) = for (t <- seq) action(t)
    def map[S](f: T => S) = FromSeq(seq.map(f))
    def withFilter(p: T => Boolean) = FromSeq(seq.filter(p))
    def flatMap[S](f: T => Def[S]) = ???
    def head = if (seq.isEmpty) Val.Closed else Val.Value(seq.head)
  }
}
