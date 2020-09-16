package mw.tchoo.ecos

sealed trait Message {
  def oid: Int
  def entries: List[EntryLine]
  def errorNumber: Int
  def errorMessage: String
}
object Message {
  def apply(start: StartLine, entries: List[EntryLine], end: EndLine): Message = start match {
    case ReplyLine(request) => Reply(request, entries, end.errNum, end.errMsg)
    case EventLine(oid) => Event(oid, entries, end.errNum, end.errMsg)
  }
}
case class Reply(request: Request, entries: List[EntryLine], errorNumber: Int, errorMessage: String) extends Message {
  def oid = request.oid
  override def toString = entries match {
    case Nil => s"Reply($request, $errorNumber, $errorMessage)"
    case _ => s"Reply($request, ${entries.mkString(", ")}, $errorNumber, $errorMessage)"
  }
}
case class Event(oid: Int, entries: List[EntryLine], errorNumber: Int, errorMessage: String) extends Message {
  override def toString = entries match {
    case Nil => s"Event($oid, $errorNumber, $errorMessage)"
    case _ => s"Event($oid, ${entries.mkString(", ")}, $errorNumber, $errorMessage)"
  }
}
