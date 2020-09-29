package mw.persist

trait Encoder[-T, +F] {
  def apply(t: T): F
}
