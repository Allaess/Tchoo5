package mw.tchoo

import mw.persist.{DecodeException, Decoder, Encoder}
import mw.persist.json.{Json, JsonObject, JsonString}
import mw.react.Val
import mw.tchoo.ecos.Ecos
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

trait CommandStation {
  type MyBloc <: Bloc
  type MyRoute <: Route
  def protocolVersion: Val[String]
  def blocs: Set[MyBloc]
  def routes: Set[MyRoute]
}
object CommandStation {
  private var instances = Set.empty[CommandStation]
  implicit val encode: Encoder[CommandStation, JsonObject] = {
    case ecos: Ecos => Ecos.jsonEncode(ecos)
  }
  implicit def decode(implicit exec: ExecutionContext): Decoder[Json, CommandStation] = {
    case obj@JsonObject(map) => map.get(Json("type")) match {
      case Some(JsonString("Ecos")) => Ecos.jsonDecode(exec)(obj) match {
        case result@Success(station) =>
          instances += station
          result
        case result => result
      }
      case Some(json) => Failure(DecodeException(""""Ecos"""", json))
      case None => Failure(DecodeException(""""type" attribute""", obj))
    }
    case json => Failure(DecodeException("object", json))
  }
}
