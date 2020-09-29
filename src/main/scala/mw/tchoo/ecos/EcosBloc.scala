package mw.tchoo.ecos

import mw.persist.{DecodeException, Decoder, Encoder}
import mw.persist.json.{Json, JsonNumber, JsonObject}
import mw.tchoo.Bloc
import scala.util.{Failure, Try}

class EcosBloc(val commandStation: Ecos, val oid: Int, val port: Int) extends Bloc {
  type Id = (Int, Int)
  val id = (oid, port)
  val ecosState = for (hex :: Nil <- commandStation.updates(oid, "state")) yield {
    if (hex.startsWith("0x")) (fromHex(hex.drop(2)) & (1 << port)) != 0
    else throw ParseException(s"Expected: hex number. Actual: $hex")
  }
  val railcom = for (p :: addr :: dir :: Nil <- commandStation.updates(oid, "railcom")
                     if p.toInt == port) yield
    (addr.toInt, dir.toInt != 0)
  val state = for {
    s <- ecosState // TODO convert (addr, dir) from railcom into suitable loco information
  } yield
    if (s) Bloc.Occupied(None)
    else Bloc.Free
  def fromHex(hex: String): Int = {
    val first = hex.last match {
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
      case 'A' => 10
      case 'B' => 11
      case 'C' => 12
      case 'D' => 13
      case 'E' => 14
      case 'F' => 15
      case 'a' => 10
      case 'b' => 11
      case 'c' => 12
      case 'd' => 13
      case 'e' => 14
      case 'f' => 15
    }
    val n = hex.length
    if (n > 1) fromHex(hex.dropRight(1) * 16 + first)
    else first
  }
}
object EcosBloc {
  def apply(ecos: Ecos, oid: Int, port: Int) = new EcosBloc(ecos, oid, port)
  implicit def jsonEncode: Encoder[EcosBloc, JsonObject] = { bloc =>
    JsonObject("oid" -> bloc.oid, "port" -> bloc.port)
  }
  implicit def jsonDecode(implicit ecos: Ecos): Decoder[Json, EcosBloc] = {
    case json@JsonObject(map) => (map.get("oid"), map.get("port")) match {
      case (Some(JsonNumber(oid)), Some(JsonNumber(port))) => Try(EcosBloc(ecos, oid.toInt, port.toInt))
      case (Some(json), _) => Failure(DecodeException("number", json))
      case (None, _) => Failure(DecodeException(""""oid" attribute""", json))
      case (_, Some(json)) => Failure(DecodeException("number", json))
      case (_, None) => Failure(DecodeException(""""port" attribute""", json))
    }
    case json => Failure(DecodeException("object", json))
  }
}