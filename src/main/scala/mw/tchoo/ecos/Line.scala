package mw.tchoo.ecos

sealed trait Line
sealed trait StartLine extends Line
case class ReplyLine(request: Request) extends StartLine
case class EventLine(oid: Int) extends StartLine
case class EntryLine(oid: Int, arguments: Set[Argument]) extends Line {
  override def toString = s"EntryLine($oid, ${arguments.mkString(", ")})"
}
case class EndLine(errNum: Int, errMsg: String) extends Line
