package mw.persist

class DecodeException[F](val expected: String, val actual: F) extends Exception(s"Expected: $expected Actual: $actual")
object DecodeException {
  def apply[F](expected: String, actual: F) = new DecodeException(expected, actual)
  def unapply[F](exception: DecodeException[F]) = Some(exception.expected, exception.actual)
}
