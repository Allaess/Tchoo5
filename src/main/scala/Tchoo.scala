import mw.persist.json.Json
import mw.tchoo.CommandStation
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global

object Tchoo extends App {
  val in = getClass.getClassLoader.getResourceAsStream("config.json")
  val config = Source.fromInputStream(in, "UTF-8").getLines().foldLeft("")(_ + _)
  val files = Json.parse[List[String]](config).get
  val stations = for (file <- files) yield {
    val in = getClass.getClassLoader.getResourceAsStream(file)
    val config = Source.fromInputStream(in, "UTF-8").getLines().foldLeft("")(_ + _)
    Json.parse[CommandStation](config).get
  }
  for {
    station <- stations
    version <- station.protocolVersion
  } {
    println(s"Protocol version of $station: $version")
  }
}
