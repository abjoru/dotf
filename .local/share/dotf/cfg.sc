import ammonite.ops._

import $ivy.`org.typelevel::cats-effect:2.1.0`

import cats.implicits._
import cats.effect.Sync
import scala.io.Source

final case class Cfg(
  headless: Boolean,
  xmobarMappings: List[String]
)

object Cfg {

  val empty = Cfg(true, List("xmobarrc0"))

  val cfgPath = home/".config"/"dotf"/"dotf.cfg"

  def load[F[_]: Sync]: F[Cfg] = for {
    lines <- Sync[F].delay(Source.fromFile(cfgPath.toString).getLines())
    splits <- lines.map(v => v.split("=").toList).pure[F]
    mapped <- mkMap(splits).pure[F]
    maybe <- mkCfg(mapped).getOrElse(empty).pure[F]
  } yield maybe


  private def mkMap(xs: Iterator[List[String]]) = xs.foldLeft(Map.empty[String, String]) {
    case (acc, f :: s :: Nil) => acc + (f.trim -> s.trim)
    case (acc, _) => acc
  }

  private def mkCfg(m: Map[String, String]): Option[Cfg] = for {
    headless <- m.get("headless").map(_.toBoolean)
    mappings <- m.get("xmobarrc").map(_.split(",").map(_.trim))
  } yield Cfg(headless, mappings.toList)

}
