package mw.persist

import scala.util.Try

trait Decoder[-F, +T] {
  def apply(data: F): Try[T]
}
