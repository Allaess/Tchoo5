package mw.tchoo

import mw.react.{Def, Val}

trait Route {
  val state: Def[Route.State]
  sealed trait End {
    def opposite: End
    var connected = Option.empty[Bloc#End]
  }
  case object Entry extends End {
    def opposite = Exit
  }
  case object Exit extends End {
    def opposite = Entry
  }
}
object Route {
  object Dummy extends Route {
    val state = Val(Active)
  }
  sealed trait State
  case object Active extends State
  case object Inactive extends State
  case object Switching extends State
}
