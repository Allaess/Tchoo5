package mw.tchoo.ecos

import mw.persist.{DecodeException, Decoder, Encoder}
import mw.persist.json.{Json, JsonArray, JsonNumber, JsonObject}
import mw.tchoo.Route
import scala.util.{Failure, Success, Try}

case class EcosRoute(commandStation: Ecos, oid: Int)(val entry: EcosBloc#End, val exit: EcosBloc#End) extends Route {
  val switching = for (state :: Nil <- commandStation.updates(oid, "switching")) yield state.toInt != 0
  val ecosState = for (state :: Nil <- commandStation.updates(oid, "state")) yield state.toInt != 0
  val state = for {
    sw <- switching
    st <- ecosState
  } yield
    if (sw) Route.Switching
    else if (st) Route.Active
    else Route.Inactive
}
object EcosRoute {
  implicit val encode: Encoder[EcosRoute, JsonObject] = { route =>
    implicit val ecos: Ecos = route.commandStation
    JsonObject("oid" -> route.oid, "entry" -> route.entry.bloc)
  }
  implicit val decode: Decoder[Json, EcosRoute] = {
    case obj@JsonObject(map) =>
      val oid = map.get("oid") match {
        case Some(JsonNumber(num)) => Success(num.toInt)
        case Some(json) => Failure(DecodeException("number", json))
        case None => Failure(DecodeException(""""oid" attribute""", obj))
      }
      val entry=map.get("entry")
    case json =>
  }
}