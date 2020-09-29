import mw.persist.json.Json
import mw.persist.json.Json.JsonOps
import mw.tchoo.ecos.Ecos
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Test extends App {
  val ecos = Ecos("192.168.1.68")
  for (msg <- ecos.messages) msg match {
    case Success(msg) => println(msg)
    case Failure(err) => System.err.println(err)
  }
  ecos.send("get(1,info)")
  val data = Map("Marc" -> 54, "Claire" -> 52)
  val json = data.toJson
  println(json)
  println(Json.parse[Map[String, Int]](json))
}
