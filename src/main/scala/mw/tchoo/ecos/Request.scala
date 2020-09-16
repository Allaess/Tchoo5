package mw.tchoo.ecos

case class Request(command: String, oid: Int, arguments: Set[Argument]) {
  override def toString = arguments.size match {
    case 0 => s"$command($oid)"
    case _ => s"$command($oid,${arguments.mkString(",")})"
  }
}
