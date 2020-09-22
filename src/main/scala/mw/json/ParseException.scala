package mw.json

class ParseException(message: String) extends Exception(message)
object ParseException {
  def apply(message: String) = new ParseException(message)
}
