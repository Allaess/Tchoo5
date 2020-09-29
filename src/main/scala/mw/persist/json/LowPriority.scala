package mw.persist.json

import mw.persist.Encoder

trait LowPriority {
  implicit def encodeAnyMap[K, V](implicit encodePair: Encoder[(K, V), Json]): Encoder[Map[K, V], JsonArray] = { map =>
    JsonArray(map.toList.map(encodePair(_)))
  }
}
