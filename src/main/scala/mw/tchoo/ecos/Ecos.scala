package mw.tchoo.ecos

import java.io.{BufferedWriter, OutputStreamWriter}
import java.net.Socket
import mw.react.Var
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers

case class Ecos(name: String, port: Int)(implicit exec: ExecutionContext) {
  val socket = new Socket(name, port)
  val in = Source.fromInputStream(socket.getInputStream, "UTF-8").getLines()
  val out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, "UTF-8"))
  val lines = Var[String]
  val messages = lines
    .map(Ecos.parseLine)
    .scan(Try(Option.empty[StartLine], List.empty[EntryLine], Option.empty[EndLine])) {
      case (_, Success(start: StartLine)) =>
        Success(Some(start), Nil, None)
      case (Success((Some(start), entries, None)), Success(entry: EntryLine)) =>
        Success(Some(start), entry :: entries, None)
      case (Success((Some(start), entries, None)), Success(end: EndLine)) =>
        Success(Some(start), entries, Some(end))
      case (_, Failure(error)) =>
        Failure(error)
      case (_, Success(line)) =>
        Failure(ParseException(s"Unexpected line ignored: $line"))
    }
    .collect {
      case Success((Some(start), entries, Some(end@EndLine(0, _)))) =>
        Success(Message(start, entries.reverse, end))
      case Success((_, _, Some(EndLine(errNum, errMsg)))) =>
        Failure(EcosException(errNum, errMsg))
      case Failure(error) =>
        Failure(error)
    }
  object Reader extends Runnable {
    def run() = for (line <- in) {
      lines.publish(line)
      println(s"ECOS >>> $line")
    }
  }
  new Thread(Reader).start()
  def send(request: String) = {
    out.write(request)
    out.newLine()
    out.flush()
    println(s"ECOS <<< $request")
  }
  override def toString = s"Ecos($name:$port)"
}
object Ecos extends RegexParsers {
  def parseLine(text: String): Try[Line] = parseAll(line, text) match {
    case Success(obj, _) => scala.util.Success(obj)
    case NoSuccess(msg, _) => scala.util.Failure(ParseException(msg))
  }
  def line: Parser[Line] = replyLine | eventLine | entryLine | endLine
  def replyLine: Parser[ReplyLine] = "<" ~> "REPLY" ~> request <~ ">" ^^ { req => ReplyLine(req) }
  def eventLine: Parser[EventLine] = "<" ~> "EVENT" ~> num <~ ">" ^^ { oid => EventLine(oid) }
  def entryLine: Parser[EntryLine] = num ~ argument.* ^^ {
    case oid ~ args => EntryLine(oid, args.toSet)
  }
  def endLine: Parser[EndLine] = "<" ~> "END" ~> num ~ "(" ~ errorMessage <~ ")" <~ ">" ^^ {
    case errNum ~ _ ~ errMsg => EndLine(errNum, errMsg)
  }
  def request: Parser[Request] = token ~ "(" ~ num ~ ("," ~> argument).* <~ ")" ^^ {
    case cmd ~ _ ~ oid ~ args => Request(cmd, oid, args.toSet)
  }
  def num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  def argument: Parser[Argument] = token ~ ("[" ~> (value ~ ("," ~> value).*).? <~ "]").? ^^ {
    case option ~ None => Argument(option, Nil)
    case option ~ Some(None) => Argument(option, Nil)
    case option ~ Some(Some(head ~ tail)) => Argument(option, head :: tail)
  }
  def errorMessage: Parser[String] = "[^()]+".r
  def token: Parser[String] = """[a-zA-Z0-9\-_]+""".r
  def value: Parser[String] = quoted | unquoted
  def quoted: Parser[String] = """("[^"]*")+""".r ^^ unquote
  def unquoted: Parser[String] = """[^,"\[\]]+""".r
  def quote(text: String) =
    if (text.contains('"')) s""""${text.replaceAll("\"", "\"\"")}""""
    else text
  def unquote(text: String) = text.drop(1).dropRight(1).replaceAll("\"\"", "\"")
}
