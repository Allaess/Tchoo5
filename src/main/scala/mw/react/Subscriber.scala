package mw.react

trait Subscriber[-T] {
  def onCancel(action: => Unit): Unit
  def published(t: T): Unit
  def closed(): Unit
  def failed(error: Throwable): Unit
}
