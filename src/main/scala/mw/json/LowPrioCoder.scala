package mw.json

trait LowPrioCoder {
  implicit def map[K, V](implicit encode: Coder[(K, V), JsonArray]): Coder[Map[K, V], JsonArray] = { map =>
    JsonArray(map.toList.map(encode(_)))
  }
}
