package mw.json

class JsonException(val expected: String, val actual: Json) extends Exception(s"Expected: $expected. Actual: $actual")
object JsonException {
  def apply(expected: String, actual: Json) = new JsonException(expected, actual)
  def unapply(e: JsonException) = Some(e.expected, e.actual)
}
