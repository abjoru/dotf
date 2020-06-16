import ammonite.ops._
import Console._
import scala.util.Try
import scala.io.Source

import $ivy.`io.circe::circe-yaml:0.12.0`

import io.circe._
import io.circe.yaml.parser

object Const {

  implicit val ws = home

  val dotfDir = home/".dotf"

  /** Returns a sequence of paths managed by git. */
  def managedFiles(): Seq[os.Path] = {
    val cmd = %%("git", s"--git-dir=${dotfDir.toString}", s"--work-tree=${ws.toString}", "ls-files")
    cmd.out.lines.map(l => os.Path(s"${home.toString}/$l")).toSeq
  }

  /** Returns a sequence of managed `Pkg`s. */
  def pkgAll(): Seq[Pkg] = for {
    path <- managedFiles().filter(_.ext == "pkg")
    file = Source.fromFile(path.toString).reader()
    json <- parser.parse(file).toOption.flatMap(_.asArray).getOrElse(Seq.empty)
    obj <- json.asObject
  } yield Pkg(obj)

  def pkgs(): Seq[Pkg] = pkgAll().filterNot(_.isIgnored)

}

object Printer {
  /** Prints the given `msg` in terminal green. */
  def ok(msg: String): Unit = println(s"${GREEN}${msg}${RESET}")

  /** Prints the given `msg` in terminal red. */
  def err(msg: String): Unit = println(s"${RED}${msg}${RESET}")

  /** Prints the given `msg` in terminal yellow. */
  def warn(msg: String): Unit = println(s"${YELLOW}${msg}${RESET}")

  /** Prints the given `msg` in terminal cyan. */
  def info(msg: String): Unit = println(s"${CYAN}${msg}${RESET}")
}

sealed abstract class PkgSys(val name: String)

case object Apt extends PkgSys("apt")
case object Pacman extends PkgSys("pacman")
case object Homebrew extends PkgSys("brew")

object OS {

  /** Find the target package system if supported. */
  lazy val pkgSystem: Option[PkgSys] = System.getProperty("os.name", "generic").toLowerCase match {
    case s if s.contains("mac") || s.contains("osx") || s.contains("darwin") =>
      if (which("brew")) Some(Homebrew) else None

    case s if s.contains("linux") =>
      if (which("apt")) Some(Apt)
      else if (which("pacman")) Some(Pacman)
      else None
  }

  private def which(bin: String): Boolean = {
    implicit val wc = home
    Try(%%("which", bin).exitCode == 0).isSuccess
  }

}

/** Defines an OS dependent package descriptor.
  *
  * @param data package Json node
  */
final case class Pkg(data: JsonObject) {
  private val root = data.toList.head

  private def rootField[T: Decoder](name: String): Option[T] = 
    root._2.hcursor.downField(name).as[T].toOption

  private val osNode: Option[Json] = for {
    os <- OS.pkgSystem
    nd <- root._2.hcursor.downField(os.name).as[Json].toOption
  } yield nd

  private def field[T: Decoder](name: String): Option[T] = for {
    os <- osNode
    fd <- os.hcursor.downField(name).as[T].toOption
  } yield fd

  /** Name of the package as determined by the pkg system. */
  def name: String = field[String]("name").getOrElse(root._1)

  /** Warning message to display for the target pkg system. (i.e. note) */
  def warn: Option[String] = field[String]("warn")

  /** Tests if the given package is set to be ignored for the target pkg system. */
  def isIgnored: Boolean = field[Boolean]("ignore").getOrElse(false)

  /** Tests if this pkg is in Homebrew cask. */
  def isCask: Boolean = field[Boolean]("cask").getOrElse(false)

  /** Tests if this pkg requires special instructions and cannot be batched. */
  def isSpecial: Boolean = preInstall.nonEmpty || postInstall.nonEmpty || pkgPreInstall.nonEmpty || pkgPostInstall.nonEmpty

  /** Get the pre-install instructions for this package (pkg system independent). */
  def preInstall: Seq[String] = rootField[String]("preinstall").map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  /** Get the post-install instructions for this package (pkg system independent). */
  def postInstall: Seq[String] = rootField[String]("postinstall").map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  /** Get the pre-install instruction for this package as defined by the pkg system. */
  def pkgPreInstall: Seq[String] = field[String]("preinstall").map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  /** Get the post-install instruction for this package as defined by the pkg system. */
  def pkgPostInstall: Seq[String] = field[String]("postinstall").map(_.split("\\s+").toSeq).getOrElse(Seq.empty)
}
