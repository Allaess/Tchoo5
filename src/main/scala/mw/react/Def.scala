package mw.react

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait Def[+T] {
  source =>
  implicit def exec: ExecutionContext
  def subscribe(subscriber: Subscriber[T]): Unit
  def foreach(action: T => Unit) = source.subscribe(new Subscriber[T] {
    private var cancel = { () => }
    def onCancel(action: => Unit) = cancel = { () => action }
    def published(t: T) = Try(action(t)) match {
      case Failure(_) =>
        cancel()
        cancel = { () => }
      case _ =>
    }
    def closed() = cancel = { () => }
    def failed(error: Throwable) = cancel = { () => }
    override def toString = s"ForeachSubscriber($source)"
  })
  def map[S](f: T => S): Def[S] = new Def.Impl[S] {
    result =>
    source.subscribe(new Subscriber[T] {
      private var interrupted = false
      private var cancel = { () => }
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!interrupted) Try(f(t)) match {
        case Success(s) =>
          result.publish(s)
        case Failure(e) =>
          interrupted = true
          result.fail(e)
          cancel()
          cancel = { () => }
      }
      def closed() = if (!interrupted) {
        result.close()
        cancel = { () => }
      }
      def failed(error: Throwable) = if (!interrupted) {
        result.fail(error)
        cancel = { () => }
      }
      override def toString = s"MapSubscriber($source)"
    })
    override def toString = s"Mapped${super.toString} from $source"
  }
  def withFilter(p: T => Boolean): Def[T] = new Def.Impl[T] {
    result =>
    source.subscribe(new Subscriber[T] {
      private var interrupted = false
      private var cancel = { () => }
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!interrupted) Try(p(t)) match {
        case Success(true) =>
          result.publish(t)
        case Success(false) =>
        case Failure(error) =>
          interrupted = true
          result.fail(error)
          cancel()
          cancel = { () => }
      }
      def closed() = if (!interrupted) {
        result.close()
        cancel = { () => }
      }
      def failed(error: Throwable) = if (!interrupted) {
        result.fail(error)
        cancel = { () => }
      }
      override def toString = s"FilterSubscriber($source)"
    })
    override def toString = s"Filtered${super.toString} from $source"
  }
  def collect[S](pf: PartialFunction[T, S]): Def[S] = new Def.Impl[S] {
    result =>
    source.subscribe(new Subscriber[T] {
      private var interrupted = false
      private var cancel = { () => }
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!interrupted && pf.isDefinedAt(t)) Try(pf(t)) match {
        case Success(t) =>
          result.publish(t)
        case Failure(e) =>
          interrupted = true
          result.fail(e)
          cancel()
          cancel = { () => }
      }
      def closed() = if (!interrupted) {
        result.close()
        cancel = { () => }
      }
      def failed(error: Throwable) = if (!interrupted) {
        result.fail(error)
        cancel = { () => }
      }
      override def toString = s"CollectSubscriber($source)"
    })
    override def toString = s"Collected${super.toString} from $source"
  }
  def scan[S](init: S)(f: (S, T) => S): Def[S] = new Def.Impl[S] {
    result =>
    source.subscribe(new Subscriber[T] {
      private var s = init
      private var interrupted = false
      private var cancel = { () => }
      def onCancel(action: => Unit) = cancel = { () => action }
      def published(t: T) = if (!interrupted) Try(f(s, t)) match {
        case Success(newS) =>
          s = newS
          result.publish(s)
        case Failure(error) =>
          interrupted = true
          result.fail(error)
          cancel()
          cancel = { () => }
      }
      def closed() = if (!interrupted) {
        result.close()
        cancel = { () => }
      }
      def failed(error: Throwable) = if (!interrupted) {
        result.fail(error)
        cancel = { () => }
      }
      override def toString = s"ScanSubscriber($source)"
    })
    override def toString = s"Scanned${super.toString}"
  }
}
object Def {
  class Impl[T](implicit val exec: ExecutionContext) extends Def[T] {
    protected var current = Promise[AList]
    protected def update(expr: => T) = append(current, Try(ACons(expr)))
    protected def publish(t: T) = append(current, Success(ACons(t)))
    protected def close() = append(current, Success(ANil))
    protected def fail(error: Throwable) = append(current, Failure(error))
    protected def append(promise: Promise[AList], elem: Try[AList]): Unit =
      if (promise.tryComplete(elem)) current = promise
      else promise.future.value match {
        case Some(Success(ACons(_, tail))) => append(tail, elem)
        case _ =>
      }
    def subscribe(subscriber: Subscriber[T]) = {
      var cancelled = false
      subscriber.onCancel {
        cancelled = true
      }
      monitor(current.future)
      def monitor(future: Future[AList]): Unit = future.onComplete {
        case Success(ACons(head, tail)) =>
          subscriber.published(head)
          monitor(tail.future)
        case Success(ANil) =>
          subscriber.closed()
        case Failure(error) =>
          subscriber.failed(error)
      }
    }
    override def toString = current.future.value match {
      case Some(Success(aList)) => s"Def($aList)"
      case Some(Failure(error)) => s"Def(<$error>)"
      case None => "Def(<?>)"
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
  }
}
