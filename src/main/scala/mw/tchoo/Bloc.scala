package mw.tchoo

import java.beans.Encoder

import mw.persist.Decoder
import mw.persist.json.{Json, JsonObject}
import mw.react.Def
import mw.tchoo.ecos.EcosBloc

trait Bloc {
  type Id
  val commandStation: CommandStation
  val id: Id
  val state: Def[Bloc.State]
  sealed trait End {
    def opposite: End
    private var _connected = Set.empty[Route#End]
    def connected = _connected
    def connect(that: Route#End) = if (!_connected.contains(that)) {
      _connected += that
      that.connect(this)
    }
    def bloc = Bloc.this
  }
  case object West {
    def opposite = East
  }
  case object East {
    def opposite = West
  }
}
object Bloc {
  sealed trait State
  case object Free extends State
  case class Occupied(by: Option[Loco] = None) extends State
  implicit val encode: Encoder[Bloc, JsonObject] = {
    case bloc: EcosBloc => EcosBloc.jsonEncode(bloc)
  }
}
