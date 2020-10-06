package mw.tchoo.ecos

import java.io.{BufferedWriter, OutputStreamWriter}
import java.net.Socket
import mw.persist.{DecodeException, Decoder, Encoder}
import mw.persist.json.{Json, JsonArray, JsonNull, JsonNumber, JsonObject, JsonString}
import mw.react.{Def, Var}
import mw.tchoo.CommandStation
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers

case class Ecos(name: String, port: Int = 15471)(implicit exec: ExecutionContext) extends CommandStation {
  ecos =>
  type MyBloc = EcosBloc
  var blocs = Set.empty[EcosBloc]
  var routes = Set.empty[EcosRoute]
  private var socket = new Socket(name, port)
  private var in = Source.fromInputStream(socket.getInputStream, "UTF-8").getLines()
  private var out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, "UTF-8"))
  private val lines = Var[String]
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
  private var views = Set.empty[Int]
  private var updatesCache = Map.empty[(Int, String), Def[List[String]]]
  val protocolVersion = (for {
    Success(msg) <- messages
    EntryLine(1, args) <- msg
    Argument("ProtocolVersion", num :: Nil) <- args
  } yield num).head
  send("get(1,info)")
  object Reader extends Runnable {
    @tailrec def run() = {
      for (line <- in) {
        lines.publish(line)
        println(s"  $ecos >>> $line")
      }
      socket = new Socket(name, port)
      in = Source.fromInputStream(socket.getInputStream, "UTF-8").getLines()
      out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, "UTF-8"))
      run()
    }
  }
  new Thread(Reader).start()
  def send(request: String) = {
    out.write(request)
    out.newLine()
    out.flush()
    println(s"$ecos <<< $request")
  }
  def updates(OID: Int, ATTR: String): Def[List[String]] = updatesCache.get(OID, ATTR) match {
    case Some(d) => d
    case None =>
      val d = for {
        Success(msg) <- messages
        EntryLine(OID, args) <- msg.entries
        Argument(ATTR, values) <- args
      } yield values
      updatesCache += (OID, ATTR) -> d
      if (!views.contains(OID)) {
        views += OID
        send(s"request($OID,view)")
      }
      send(s"get($OID,$ATTR)")
      d
  }
  override def toString = s"Ecos($name:$port)"
}
object Ecos extends RegexParsers {
  implicit def jsonDecode(implicit exec: ExecutionContext): Decoder[Json, Ecos] = {
    case obj@JsonObject(map) =>
      val name = map.get("name") match {
        case Some(JsonString(name)) => scala.util.Success(name)
        case Some(json) => scala.util.Failure(DecodeException("string", json))
        case None => scala.util.Failure(DecodeException(""""name" attribute""", obj))
      }
      val port = map.get("port") match {
        case Some(JsonNumber(num)) => scala.util.Success(Some(num.toInt))
        case Some(JsonNull) => scala.util.Success(None)
        case Some(json) => scala.util.Failure(DecodeException("null or number", json))
        case None => scala.util.Success(None)
      }
      for {
        name <- name
        port <- port
      } yield {
        implicit val ecos: Ecos = port match {
          case Some(port) => Ecos(name, port)
          case None => Ecos(name)
        }
        ecos.blocs = map.get("blocs") match {
          case Some(array@JsonArray(_)) => Json.decode[Set[EcosBloc]](array).get
          case Some(JsonNull) => Set.empty
          case Some(json) => throw DecodeException("null or array", json)
          case None => Set.empty
        }
        ecos.routes = map.get("routes") match {
          case Some(array@JsonArray(_)) => Json.decode[Set[EcosRoute]](array).get
          case Some(JsonNull) => Set.empty
          case Some(json) => throw DecodeException("null or array", json)
          case None => Set.empty
        }
        ecos
      }
    case json => scala.util.Failure(DecodeException("object", json))
  }
  implicit def jsonEncode: Encoder[Ecos, JsonObject] = { implicit ecos =>
    JsonObject("name" -> ecos.name, "port" -> ecos.port, "blocs" -> ecos.blocs)
  }
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
