package mw.json

import scala.util.{Failure, Try}

trait LowPrioDecoder {
  implicit def map[K, V](implicit decodePair: Decoder[JsonArray, (K, V)]): Decoder[JsonArray, Map[K, V]] = {
    case JsonArray(list) => Try(list.map(decodePair(_).get).toMap)
    case json => Failure(JsonException("Array", json))
  }
}
