package mw.tchoo

import mw.react.{Def, Val}

trait Route {
  val state: Def[Route.State]
  sealed trait End {
    def opposite: End
    private var _connected = Option.empty[Bloc#End]
    def connected = _connected
    def connect(that: Bloc#End) = if (!_connected.contains(that)) {
      _connected = Some(that)
      that.connect(this)
    }
  }
  case object West extends End {
    def opposite = East
  }
  case object East extends End {
    def opposite = West
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
