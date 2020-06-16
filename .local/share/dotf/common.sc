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

  def managedFiles(): Seq[os.Path] = {
    val cmd = %%("git", s"--git-dir=${dotfDir.toString}", s"--work-tree=${ws.toString}", "ls-files")
    cmd.out.lines.map(l => os.Path(s"${home.toString}/$l")).toSeq
  }

  def pkgs(): Seq[Pkg] = {
    val pkgFiles = managedFiles().filter(_.ext == "pkg")
    val objs = pkgFiles.flatMap(f => parser.parse(Source.fromFile(f.toString).reader()).toOption)

    val result = objs.flatMap(_.asArray.getOrElse(Seq.empty[Json]).collect {
      case o if o.isObject => Pkg(o.asObject.get)
    })

    result.filterNot(_.isIgnored)
  }

  def customInstalls(): Seq[os.Path] =
    managedFiles().filter(_.last == "install.sh")

  private def isSupported(path: os.Path): Boolean = {
    path.last.split('_').toList match {
      case "brew" :: _ => OS.pkgSystem == Some(Homebrew)
      case "apt" :: _ => OS.pkgSystem == Some(Apt)
      case "pacman" :: _ => OS.pkgSystem == Some(Pacman)
      case _ => true
    }
  }

}

object Printer {
  def ok(msg: String): Unit = println(s"${GREEN}${msg}${RESET}")
  def err(msg: String): Unit = println(s"${RED}${msg}${RESET}")
  def warn(msg: String): Unit = println(s"${YELLOW}${msg}${RESET}")
  def info(msg: String): Unit = println(s"${CYAN}${msg}${RESET}")
}

sealed abstract class PkgSys(val name: String)

case object Homebrew extends PkgSys("brew")
case object Apt extends PkgSys("apt")
case object Pacman extends PkgSys("pacman")

object OS {

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

final case class Pkg(data: JsonObject) {
  private val root = data.toList.head
  private val apt: Option[Json] = getObj("apt")
  private val brew: Option[Json] = getObj("brew")
  private val pacman: Option[Json] = getObj("pacman")

  val name: String = OS.pkgSystem match {
    case Some(Apt) => nameFor(apt)
    case Some(Pacman) => nameFor(pacman)
    case Some(Homebrew) => nameFor(brew)
    case _ => root._1
  }

  val warn: Option[String] = OS.pkgSystem match {
    case Some(Apt) => apt.flatMap(_.hcursor.downField("warn").as[String].toOption)
    case Some(Pacman) => pacman.flatMap(_.hcursor.downField("warn").as[String].toOption)
    case Some(Homebrew) => brew.flatMap(_.hcursor.downField("warn").as[String].toOption)
    case _ => None
  }

  def isSpecial: Boolean = preInstall.nonEmpty || postInstall.nonEmpty || pkgPreInstall.nonEmpty || pkgPostInstall.nonEmpty

  def isIgnored: Boolean = OS.pkgSystem match {
    case Some(Apt) => apt.flatMap(_.hcursor.downField("ignore").as[Boolean].toOption).getOrElse(false)
    case Some(Pacman) => pacman.flatMap(_.hcursor.downField("ignore").as[Boolean].toOption).getOrElse(false)
    case Some(Homebrew) => brew.flatMap(_.hcursor.downField("ignore").as[Boolean].toOption).getOrElse(false)
    case _ => false
  }

  def isCask: Boolean = OS.pkgSystem match {
    case Some(Homebrew) => brew.flatMap(_.hcursor.downField("cask").as[Boolean].toOption).getOrElse(false)
    case _ => false
  }

  def preInstall: Seq[String] = 
    root._2.hcursor.downField("preinstall").as[String].toOption.map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  def pkgPreInstall: Seq[String] = OS.pkgSystem match {
    case Some(Apt) => preInstallFor(apt)
    case Some(Pacman) => preInstallFor(pacman)
    case Some(Homebrew) => preInstallFor(brew)
    case _ => Seq.empty
  }

  def postInstall: Seq[String] =
    root._2.hcursor.downField("postinstall").as[String].toOption.map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  def pkgPostInstall: Seq[String] = OS.pkgSystem match {
    case Some(Apt) => postInstallFor(apt)
    case Some(Pacman) => postInstallFor(pacman)
    case Some(Homebrew) => postInstallFor(brew)
    case _ => Seq.empty
  }

  private def nameFor(node: Option[Json]): String = 
    node.flatMap(_.hcursor.downField("name").as[String].toOption).getOrElse(root._1)

  private def preInstallFor(node: Option[Json]): Seq[String] =
    node.flatMap(_.hcursor.downField("preinstall").as[String].toOption).map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  private def postInstallFor(node: Option[Json]): Seq[String] =
    node.flatMap(_.hcursor.downField("postinstall").as[String].toOption).map(_.split("\\s+").toSeq).getOrElse(Seq.empty)

  private def getObj(key: String): Option[Json] = 
    root._2.hcursor.downField(key).as[Option[Json]].toOption.getOrElse(None)
}
