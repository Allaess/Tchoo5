package mw.json

trait Coder[-T, +J <: Json] {
  def apply(t: T): J
}
object Coder extends LowPrioCoder {
  implicit def mapFromJsonString[K, V](implicit encodeKey: Coder[K, JsonString],
                                       encodeValue: Coder[V, Json]): Coder[Map[K, V], JsonObject] = { map =>
    JsonObject(map.map { case (k, v) => encodeKey(k) -> encodeValue(v) })
  }
  implicit def pair[A, B](implicit encodeA: Coder[A, Json],
                          encodeB: Coder[B, Json]): Coder[(A, B), JsonArray] = {
    case (a, b) => JsonArray(encodeA(a) :: encodeB(b) :: Nil)
  }
  implicit def list[T](implicit encode: Coder[T, Json]): Coder[List[T], JsonArray] = { list =>
    JsonArray(list.map(encode(_)))
  }
  implicit def set[T](implicit encode: Coder[T, Json]): Coder[Set[T], JsonArray] = { set =>
    JsonArray(set.toList.map(encode(_)))
  }
  implicit def option[T](implicit encode: Coder[T, Json]): Coder[Option[T], Json] = {
    case Some(t) => encode(t)
    case None => JsonNull
  }
  implicit val string: Coder[String, JsonString] = JsonString(_)
  implicit val char: Coder[Char, JsonString] = { c => JsonString(c.toString) }
  implicit val boolean: Coder[Boolean, JsonBoolean] = JsonBoolean(_)
  implicit val byte: Coder[Byte, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val short: Coder[Short, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val int: Coder[Int, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val long: Coder[Long, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val float: Coder[Float, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val double: Coder[Double, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val bigInt: Coder[BigInt, JsonNumber] = { n => JsonNumber(BigDecimal(n)) }
  implicit val bigDecimal: Coder[BigDecimal, JsonNumber] = JsonNumber(_)
}
