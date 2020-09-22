import mw.json.Json
import mw.tchoo.ecos.Ecos
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Test extends App {
  val ecos = Ecos("192.168.1.68", 15471)
  for (msg <- ecos.messages) msg match {
    case Success(msg) => println(msg)
    case Failure(err) => System.err.println(err)
  }
  ecos.send("get(1,info)")
  println(Json.decode[Map[String, Int]]("""{"Marc":54,"Claire":52}"""))
}
