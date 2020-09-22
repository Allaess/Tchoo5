package mw.json

import scala.util.parsing.combinator.RegexParsers

sealed trait Json
object Json extends RegexParsers {
  def apply(text: String) = parseAll(json, text) match {
    case Success(obj, _) => scala.util.Success(obj)
    case NoSuccess(msg, _) => scala.util.Failure(ParseException(msg))
  }
  def encode[T](t: T)(implicit encode: Coder[T, Json]): String = encode(t).toString
  def decode[T](text: String)(implicit decode: Decoder[Json, T]) = Json(text).flatMap(decode(_))
  def json: Parser[Json] = jsonObject | jsonArray | jsonString | jsonNumber | jsonBoolean | jsonNull
  def jsonObject: Parser[JsonObject] = "{" ~> (jsonPair ~ ("," ~> jsonPair).*).? <~ "}" ^^ {
    case None => JsonObject(Map.empty)
    case Some(head ~ tail) => JsonObject((head :: tail).toMap)
  }
  def jsonPair: Parser[(JsonString, Json)] = jsonString ~ ":" ~ json ^^ {
    case k ~ _ ~ v => (k, v)
  }
  def jsonArray: Parser[JsonArray] = "[" ~> (json ~ ("," ~> json).*).? <~ "]" ^^ {
    case None => JsonArray(Nil)
    case Some(head ~ tail) => JsonArray(head :: tail)
  }
  def jsonString: Parser[JsonString] = """"[^"]*"""".r ^^ { text => JsonString(text.drop(1).dropRight(1)) }
  def jsonNumber: Parser[JsonNumber] = """-?[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?""".r ^^ { text => JsonNumber(BigDecimal(text)) }
  def jsonBoolean: Parser[JsonBoolean] = "true" ^^ { _ => JsonBoolean(true) } | "false" ^^ { _ => JsonBoolean(false) }
  def jsonNull: Parser[JsonNull.type] = "null" ^^ { _ => JsonNull }
}
case class JsonObject(value: Map[JsonString, Json]) extends Json {
  override def toString =
    if (value.isEmpty) "{}"
    else s"{${value.mkString(",")}"
}
case class JsonArray(value: List[Json]) extends Json {
  override def toString =
    if (value.isEmpty) "[]"
    else s"[${value.mkString(",")}]"
}
case class JsonString(value: String) extends Json {
  override def toString = s""""$value""""
}
case class JsonNumber(value: BigDecimal) extends Json {
  override def toString = value.toString
}
case class JsonBoolean(value: Boolean) extends Json {
  override def toString = value.toString
}
case object JsonNull extends Json {
  override def toString = "null"
}
