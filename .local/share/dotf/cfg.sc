import scala.io.Source
import org.ini4j.Ini
import ammonite.ops._

import Console._
import java.io.File

import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._

//import $ivy.`com.typesafe:config:1.4.1`
import $ivy.`org.ini4j:ini4j:0.5.4`

//import com.typesafe.config.{ Config, ConfigFactory }

final case class Cfg(
  headless: Boolean,
  xmobarMappings: List[String]
)

object Cfg {

  val empty = Cfg(true, List("xmobarrc0"))

  val cfgPath = home/".config"/"dotf"/"dotf.cfg"

  def load: Cfg = internalLoad match {
    case Success(c) => c
    case Failure(err) =>
      println(s"${RED}Unable to load/parse ${cfgPath.toString()}: ${err.getMessage()}${RESET}")
      empty
  }

  private def internalLoad: Try[Cfg] = Try {
    val lines = Source.fromFile(cfgPath.toString()).getLines()
    val splits = lines.map(v => v.split("="))
    val mapValues = splits.foldLeft(Map.empty[String, String]) {
      case (acc, arr) if arr.size == 2 => 
        acc + (arr(0).trim -> arr(1).trim)
      case (acc, _) => acc
    }

    val maybeResult = for {
      headless <- mapValues.get("headless").map(_.toBoolean)
      mappings <- mapValues.get("xmobarrc").map(_.split(",").map(_.trim))
    } yield Cfg(headless, mappings.toList)

    maybeResult.getOrElse(empty)
  }

}
