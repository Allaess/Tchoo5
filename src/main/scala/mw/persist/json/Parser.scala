package mw.persist.json

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  def json: Parser[Json] = jsonObject | jsonArray | jsonString | jsonNumber | jsonTrue | jsonFalse | jsonNull
  def jsonObject: Parser[JsonObject] = "{" ~> (jsonPair ~ ("," ~> jsonPair).*).? <~ "}" ^^ {
    case None => JsonObject(Map.empty[JsonString, Json])
    case Some(head ~ tail) => JsonObject((head :: tail).toMap)
  }
  def jsonPair: Parser[(JsonString, Json)] = jsonString ~ ":" ~ json ^^ {
    case k ~ _ ~ v => (k, v)
  }
  def jsonArray: Parser[JsonArray] = "[" ~> (json ~ ("," ~> json).*).? <~ "]" ^^ {
    case None => JsonArray(Nil)
    case Some(head ~ tail) => JsonArray(head :: tail)
  }
  def jsonString: Parser[JsonString] =
    """"([^"\\]|\\"|\\\\)*"""".r ^^ { txt =>
      JsonString(txt.drop(1).dropRight(1).replaceAll("""\\"""", """""""))
    }
  def jsonNumber: Parser[JsonNumber] =
    """-?[0-9]+(\.[0-9]+)?([+-]?[eE][0-9]+)?""".r ^^ { txt =>
      JsonNumber(BigDecimal(txt))
    }
  def jsonTrue: Parser[JsonTrue.type] = "true" ^^^ JsonTrue
  def jsonFalse: Parser[JsonFalse.type] = "false" ^^^ JsonFalse
  def jsonNull: Parser[JsonNull.type] = "null" ^^^ JsonNull
}
