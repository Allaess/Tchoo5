package mw.json

import scala.util.{Failure, Success, Try}

trait Decoder[+J <: Json, +T] {
  def apply(json: Json): Try[T]
}
object Decoder extends LowPrioDecoder {
  implicit val boolean: Decoder[JsonBoolean, Boolean] = {
    case JsonBoolean(value) => Success(value)
    case json => Failure(JsonException("Boolean", json))
  }
  implicit val char: Decoder[JsonString, Char] = {
    case JsonString(value) if value.length == 1 => Success(value.head)
    case json => Failure(JsonException("Single character string", json))
  }
  implicit val string: Decoder[JsonString, String] = {
    case JsonString(value) => Success(value)
    case json => Failure(JsonException("Character string", json))
  }
  implicit val byte: Decoder[JsonNumber, Byte] = {
    case JsonNumber(value) if value.isValidByte => Success(value.toByte)
    case json => Failure(JsonException(s"Integer value from ${Byte.MinValue} to ${Byte.MaxValue}", json))
  }
  implicit val short: Decoder[JsonNumber, Short] = {
    case JsonNumber(value) if value.isValidShort => Success(value.toShort)
    case json => Failure(JsonException(s"Integer value from ${Short.MinValue} to ${Short.MaxValue}", json))
  }
  implicit val int: Decoder[JsonNumber, Int] = {
    case JsonNumber(value) if value.isValidInt => Success(value.toInt)
    case json => Failure(JsonException(s"Integer value from ${Int.MinValue} to ${Int.MaxValue}", json))
  }
  implicit val long: Decoder[JsonNumber, Long] = {
    case JsonNumber(value) if value.isValidLong => Success(value.toLong)
    case json => Failure(JsonException(s"Integer value from ${Long.MinValue} to ${Long.MaxValue}", json))
  }
  implicit val float: Decoder[JsonNumber, Float] = {
    case JsonNumber(value) if value.isDecimalFloat => Success(value.toFloat)
    case json => Failure(JsonException(s"Decimal value from ${Float.MinValue} to ${Float.MaxValue}", json))
  }
  implicit val double: Decoder[JsonNumber, Double] = {
    case JsonNumber(value) if value.isDecimalDouble => Success(value.toDouble)
    case json => Failure(JsonException(s"Decimal value from ${Double.MinValue} to ${Double.MaxValue}", json))
  }
  implicit val bigInt: Decoder[JsonNumber, BigInt] = {
    case JsonNumber(value) if value.isWhole => Success(value.toBigInt)
    case json => Failure(JsonException("Integer value", json))
  }
  implicit val bigDecimal: Decoder[JsonNumber, BigDecimal] = {
    case JsonNumber(value) => Success(value)
    case json => Failure(JsonException("Decimal value", json))
  }
  implicit def option[J <: Json, T](implicit decode: Decoder[J, T]): Decoder[J, Option[T]] = {
    case JsonNull => Success(None)
    case json => decode(json) match {
      case Success(t) => Success(Some(t))
      case Failure(JsonException(expected, actual)) => Failure(JsonException(s"$expected or null", actual))
      case Failure(error) => Failure(error)
    }
  }
  implicit def list[T](implicit decode: Decoder[Json, T]): Decoder[JsonArray, List[T]] = {
    case JsonArray(list) => Try(list.map { json => decode(json).get })
    case json => Failure(JsonException("Array", json))
  }
  implicit def set[T](implicit decode: Decoder[Json, T]): Decoder[JsonArray, Set[T]] = {
    case JsonArray(list) => Try(list.map { json => decode(json).get }.toSet)
    case json => Failure(JsonException("Set", json))
  }
  implicit def pair[A, B](implicit decodeA: Decoder[Json, A],
                          decodeB: Decoder[Json, B]): Decoder[JsonArray, (A, B)] = {
    case JsonArray(a :: b :: Nil) => Try(decodeA(a).get, decodeB(b).get)
    case json => Failure(JsonException("Array with 2 elements", json))
  }
  implicit def mapWithStringKey[K, V](implicit decodeKey: Decoder[JsonString, K],
                                      decodeValue: Decoder[Json, V]): Decoder[JsonObject, Map[K, V]] = {
    case JsonObject(map) => Try(map.map { case (k, v) => decodeKey(k).get -> decodeValue(v).get })
    case json => Failure(JsonException("Object", json))
  }
}
