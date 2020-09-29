package mw.tchoo

import mw.persist.Encoder
import mw.persist.json.JsonObject
import mw.react.Def

trait Bloc {
  type Id
  val commandStation: CommandStation
  val id: Id
  val state: Def[Bloc.State]
  sealed trait End {
    def opposite: End
    var connected = Set.empty[Route#End]
  }
  case object Entry {
    def opposite = Exit
  }
  case object Exit {
    def opposite = Entry
  }
}
object Bloc {
  sealed trait State
  case object Free extends State
  case class Occupied(by: Option[Loco] = None) extends State
}
