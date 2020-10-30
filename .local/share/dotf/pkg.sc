import ammonite.ops._


import $ivy.`io.circe::circe-yaml:0.13.0`

import io.circe._
import io.circe.yaml.parser

sealed abstract class PkgSys(val name: String)
case object Apt extends PkgSys("apt")
case object Pacman extends PkgSys("pacman")
case object Homebrew extends PkgSys("brew")

final case class Pkg(path: os.Path, data: JsonObject, pkgSystem: PkgSys) {
  private val root = data.toList.head

  private def rootField[T: Decoder](name: String): Option[T] =
    root._2.hcursor.downField(name).as[T].toOption

  private def field[T: Decoder](name: String): Option[T] = for {
    os <-  rootField[Json](pkgSystem.name)
    fd <- os.hcursor.downField(name).as[T].toOption
  } yield fd

  /** Name of the package as determined by the pkg system. */
  def name: String = field[String]("name").getOrElse(root._1)

  /** Warning message to display for the target pkg system. (i.e. note) */
  def warn: Option[String] = field[String]("warn").map(w => s"${name}: $w")

  /** Tests if the given package is set to be ignored for the target pkg system. */
  def isIgnored: Boolean = field[Boolean]("ignore").getOrElse(false)

  /** Tests if this pkg is in Homebrew cask. */
  def isCask: Boolean = field[Boolean]("cask").getOrElse(false)

  def isAur: Boolean = field[Boolean]("aur").getOrElse(false)

  def isSnap: Boolean = snapChannel.nonEmpty

  def snapChannel: List[String] = field[String]("snap").map(_.split(" ").toList).getOrElse(List.empty)

  /** Tests if this pkg requires special instructions and cannot be batched. */
  def isSpecial: Boolean = preInstallScripts.nonEmpty || postInstallScripts.nonEmpty

  /** Tests if this pkg is for headless installs (default: true) */
  def isHeadless: Boolean = rootField[Boolean]("headless").getOrElse(true)

  /** Gets the path to the optional pre-install scripts. */
  def preInstallScripts: Seq[os.Path] = {
    val maybeGlobalScript = rootField[String]("preinstall").map(n => path/up/n)
    val maybePkgScript = field[String]("preinstall").map(n => path/up/n)
    Seq(maybeGlobalScript, maybePkgScript).flatten
  }

  /** Gets the path to the optional post-install scripts. */
  def postInstallScripts: Seq[os.Path] = {
    val maybeGlobalScript = rootField[String]("postinstall").map(n => path/up/n)
    val maybePkgScript = field[String]("postinstall").map(n => path/up/n)
    Seq(maybeGlobalScript, maybePkgScript).flatten
  }
}
