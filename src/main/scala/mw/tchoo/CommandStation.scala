package mw.tchoo

import mw.persist.{DecodeException, Decoder, Encoder}
import mw.persist.json.{Json, JsonObject, JsonString}
import mw.react.Val
import mw.tchoo.ecos.Ecos
import scala.concurrent.ExecutionContext
import scala.util.Failure

trait CommandStation {
  type MyBloc <: Bloc
  def protocolVersion: Val[String]
  def blocs: Set[MyBloc]
}
object CommandStation {
  implicit val encode: Encoder[CommandStation, JsonObject] = {
    case ecos: Ecos => Ecos.jsonEncode(ecos)
  }
  implicit def decode(implicit exec: ExecutionContext): Decoder[Json, CommandStation] = {
    case obj@JsonObject(map) => map.get(Json("type")) match {
      case Some(JsonString("Ecos")) => Ecos.jsonDecode(exec)(obj)
      case Some(typ) => Failure(DecodeException(""""Ecos"""", typ))
      case None => Failure(DecodeException(""""type" attribute""", obj))
    }
    case json => Failure(DecodeException("object", json))
  }
}
