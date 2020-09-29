package mw.tchoo.panel

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

case class Interval(from: Int, to: Int) {
  override def toString =
    if (from == to) from.toString
    else s"$from - $to"
  def foreach(action: Int => Unit) = for (i <- from to to) action(i)
}
case class Intervals(intervals: List[Interval]) {
  override def toString = intervals.mkString(", ")
  def foreach(action: Int => Unit) = for {
    interval <- intervals
    i <- interval
  } action(i)
}
object Intervals extends RegexParsers {
  def parse(text: String): Try[Intervals] = parseAll(intervals, text) match {
    case Success(obj, _) => scala.util.Success(obj)
    case NoSuccess(msg, _) => scala.util.Failure(ParseException(msg))
  }
  def intervals: Parser[Intervals] = interval ~ ("," ~> interval).* ^^ {
    case head ~ tail => Intervals(head :: tail)
  }
  def interval: Parser[Interval] = num ~ ("-" ~> num).? ^^ {
    case from ~ None => Interval(from, from)
    case from ~ Some(to) => Interval(from, to)
  }
  def num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
}
