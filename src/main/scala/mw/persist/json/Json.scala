package mw.persist.json

import mw.persist.{DecodeException, Decoder, Encoder}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

sealed trait Json
object Json extends LowPriority {
  implicit def apply[T, J <: Json](t: T)(implicit encode: Encoder[T, J]): J = encode(t)
  def parse[T](txt: String)(implicit decode: Decoder[Json, T]) = Parser.parseAll(Parser.json, txt) match {
    case Parser.Success(obj, _) => decode(obj)
    case Parser.NoSuccess(msg, _) => Failure(ParserException(msg))
  }
  def decode[T](json: Json)(implicit decoder: Decoder[Json, T]) = decoder(json)
  implicit class JsonOps[T](obj: T) {
    def toJson(implicit encode: Encoder[T, Json]) = encode(obj).toString
  }
  implicit val decodeBoolean: Decoder[Json, Boolean] = {
    case JsonBoolean(value) => Success(value)
    case json => Failure(DecodeException("boolean", json))
  }
  implicit val decodeString: Decoder[Json, String] = {
    case JsonString(value) => Success(value)
    case json => Failure(DecodeException("string", json))
  }
  implicit val decodeInt: Decoder[Json, Int] = {
    case JsonNumber(value) => Try(value.toInt)
    case json => Failure(DecodeException("integer", json))
  }
  implicit def decodeOption[T](implicit decode: Decoder[Json, T]): Decoder[Json, Option[T]] = {
    case JsonNull => Success(None)
    case json => decode(json).map(Some(_)) match {
      case Failure(DecodeException(expected, actual)) => Failure(DecodeException(s"null or $expected", actual))
      case other => other
    }
  }
  implicit def decodeList[T](implicit decode: Decoder[Json, T]): Decoder[Json, List[T]] = {
    case JsonArray(value) => Try(value.map(decode(_).get))
    case json => Failure(DecodeException("array", json))
  }
  implicit def decodeSet[T](implicit decode: Decoder[Json, T]): Decoder[Json, Set[T]] = {
    case JsonArray(value) => Try(value.map(decode(_).get).toSet)
    case json => Failure(DecodeException("array", json))
  }
  implicit def decodePair[A, B](implicit decodeA: Decoder[Json, A],
                                decodeB: Decoder[Json, B]): Decoder[Json, (A, B)] = {
    case JsonArray(a :: b :: Nil) => Try(decodeA(a).get, decodeB(b).get)
    case json => Failure(DecodeException("two elements Array", json))
  }
  implicit def decodeMap[K, V](implicit decodeKey: Decoder[Json, K],
                               decodeValue: Decoder[Json, V]): Decoder[Json, Map[K, V]] = {
    case JsonObject(map) => Try(map.map { case (k, v) => decodeKey(k).get -> decodeValue(v).get })
    case JsonArray(list) => Try(list.map(decodePair(decodeKey, decodeValue)(_).get).toMap)
    case json => Failure(DecodeException("object or array", json))
  }
  implicit val encodeBoolean: Encoder[Boolean, JsonBoolean] = JsonBoolean(_)
  implicit val encodeString: Encoder[String, JsonString] = JsonString(_)
  implicit val encodeInt: Encoder[Int, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit def encodeOption[T](implicit encode: Encoder[T, Json]): Encoder[Option[T], Json] = {
    case None => JsonNull
    case Some(t) => encode(t)
  }
  implicit def encodeList[T](implicit encode: Encoder[T, Json]): Encoder[List[T], JsonArray] = { list =>
    JsonArray(list.map(encode(_)))
  }
  implicit def encodeSet[T](implicit encode: Encoder[T, Json]): Encoder[Set[T], JsonArray] = { set =>
    JsonArray(set.toList.map(encode(_)))
  }
  implicit def encodePair[A, B](implicit encodeA: Encoder[A, Json],
                                encodeB: Encoder[B, Json]): Encoder[(A, B), JsonArray] = {
    case (a, b) => JsonArray(encodeA(a) :: encodeB(b) :: Nil)
  }
  implicit def encodeMapWithStringKey[K, V](implicit encodeKey: Encoder[K, JsonString],
                                            encodeValue: Encoder[V, Json]): Encoder[Map[K, V], JsonObject] = { map =>
    JsonObject(map.map { case (k, v) => encodeKey(k) -> encodeValue(v) })
  }
}
case class JsonObject(value: Map[JsonString, Json]) extends Json {
  override def toString = s"{${value.map { case (k, v) => s"$k:$v" }.mkString(",")}}"
}
object JsonObject {
  def apply[A, B](pair: (A, B))(implicit encodeA: Encoder[A, JsonString], encodeB: Encoder[B, Json]): JsonObject =
    JsonObject(Map(encodeA(pair._1) -> encodeB(pair._2)))
  def apply[A, B, C, D](pair1: (A, B), pair2: (C, D))
                       (implicit encodeA: Encoder[A, JsonString], encodeB: Encoder[B, Json],
                        encodeC: Encoder[C, JsonString], encodeD: Encoder[D, Json]): JsonObject =
    JsonObject(Map(encodeA(pair1._1) -> encodeB(pair1._2), encodeC(pair2._1) -> encodeD(pair2._2)))
  def apply[A, B, C, D, E, F](pair1: (A, B), pair2: (C, D), pair3: (E, F))
                             (implicit encodeA: Encoder[A, JsonString], encodeB: Encoder[B, Json],
                              encodeC: Encoder[C, JsonString], encodeD: Encoder[D, Json],
                              encodeE: Encoder[E, JsonString], encodeF: Encoder[F, Json]): JsonObject =
    JsonObject(Map(encodeA(pair1._1) -> encodeB(pair1._2), encodeC(pair2._1) -> encodeD(pair2._2),
      encodeE(pair3._1) -> encodeF(pair3._2)))
}
case class JsonArray(value: List[Json]) extends Json {
  override def toString = s"[${value.mkString(",")}]"
}
case class JsonString(value: String) extends Json {
  override def toString = "\"" + value.replaceAll("\"", "\"\"")
    .replaceAll("""\\""", "\\\\") + "\""
}
case class JsonNumber(value: BigDecimal) extends Json {
  override def toString = value.toString
}
sealed class JsonBoolean protected(val value: Boolean) extends Json
object JsonBoolean {
  def apply(value: Boolean) = if (value) JsonTrue else JsonFalse
  def unapply(json: JsonBoolean) = Some(json.value)
}
object JsonTrue extends JsonBoolean(true) {
  override def toString = "true"
}
object JsonFalse extends JsonBoolean(false) {
  override def toString = "false"
}
object JsonNull extends Json {
  override def toString = "null"
}
