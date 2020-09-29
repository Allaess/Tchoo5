package mw.persist.json

class ParserException(message: String) extends Exception(message)
object ParserException {
  def apply(message: String) = new ParserException(message)
  def unapply(e: ParserException) = Some(e.getMessage)
}
