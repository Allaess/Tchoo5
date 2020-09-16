package mw.tchoo.ecos

case class Argument(option: String, values: List[String]) {
  override def toString = values match {
    case Nil => option
    case _ => s"$option[${values.map(Ecos.quote).mkString(",")}]"
  }
}
