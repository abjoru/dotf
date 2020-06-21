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

  def customInstallers(): Seq[os.Path] = managedFiles().filter(_.last == "install.sh")

  /** Returns a sequence of managed `Pkg`s. */
  def pkgAll(): Seq[Pkg] = for {
    path <- managedFiles().filter(_.ext == "pkg")
    file = Source.fromFile(path.toString).reader()
    json <- parser.parse(file).toOption.flatMap(_.asArray).getOrElse(Seq.empty)
    obj <- json.asObject
  } yield Pkg(path, obj)

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

object Mode extends Enumeration {
  type M = Value
  val Dry, Install = Value

  def apply(s: String): M = s match {
    case "dry" => Dry
    case "install" => Install
    case other => throw new Exception(s"Invalid mode: $other")
  }
}

object OS {

  private implicit val wd = home

  /** Find the target package system if supported. */
  lazy val pkgSystem: Option[PkgSys] = System.getProperty("os.name", "generic").toLowerCase match {
    case s if s.contains("linux") && which("apt")     => Some(Apt)
    case s if s.contains("linux") && which("pacman")  => Some(Pacman)
    case s if s.contains("mac") && which("brew")      => Some(Homebrew)
    case _                                            => None
  }

  def update(mode: Mode.M): Unit = (mode, pkgSystem) match {
    case (Mode.Dry, Some(Homebrew)) => println("brew upgrade")
    case (Mode.Install, Some(Homebrew)) => try %("brew", "upgrade") catch {
      case _: Throwable => Printer.err("Non-zero exit code from: brew upgrade")
    }

    case (Mode.Dry, Some(Apt)) => println("sudo apt update\nsudo apt upgrade")
    case (Mode.Install, Some(Apt)) => try {
      %sudo("apt", "update")
      %sudo("apt", "upgrade")
    } catch { case _: Throwable => Printer.err("Non-zero exit code from: sudo apt update && sudo apt upgrade") }

    case (Mode.Dry, Some(Pacman)) => println("sudo pacman -Syyu")
    case (Mode.Install, Some(Pacman)) => try %sudo("pacman", "-Syyu") catch {
      case _: Throwable => Printer.err("Non-zero exit code from: sudo pacman -Syyu")
    }

    case _ => Printer.warn("Unsupported package system!")
  }

  def install(mode: Mode.M, pkgs: Pkg*): Unit = (mode, pkgSystem) match {
    case (Mode.Dry, Some(Homebrew)) =>
      val (casks, normals) = pkgs.partition(_.isCask)
      if (normals.nonEmpty) println(s"brew install ${normals.map(_.name).mkString(" ")}")
      if (casks.nonEmpty) println(s"brew cask install ${casks.map(_.name).mkString(" ")}")
    case (Mode.Install, Some(Homebrew)) =>
      val (casks, normals) = pkgs.partition(_.isCask)
      if (normals.nonEmpty) try %("brew", "install", normals.map(_.name)) catch {
        case _: Throwable => Printer.err(s"Non-zero exit code from: brew install ${normals.map(_.name)}")
      }
      if (casks.nonEmpty) try %("brew", "cask", "install", casks.map(_.name)) catch {
        case _: Throwable => Printer.err(s"Non-zero exit code from: brew cask install ${casks.map(_.name)}")
      }

    case (Mode.Dry, Some(Apt)) =>
      println(s"sudo apt install ${pkgs.map(_.name).mkString(" ")}")
    case (Mode.Install, Some(Apt)) =>
      try %sudo("apt", "install", pkgs.map(_.name)) catch {
        case _: Throwable => Printer.err(s"Non-zero exit code from: sudo apt install ${pkgs.map(_.name)}")
      }

    case (Mode.Dry, Some(Pacman)) =>
      val (aur, normals) = pkgs.partition(_.isAur)
      if (normals.nonEmpty) println(s"sudo pacman -S ${normals.map(_.name).mkString(" ")}")
      if (aur.nonEmpty) println(s"yay -S ${aur.map(_.name).mkString(" ")}")
    case (Mode.Install, Some(Pacman)) =>
      val (aur, normals) = pkgs.partition(_.isAur)
      try {
	if (normals.nonEmpty) %sudo("pacman", "-S", normals.map(_.name)) 
      } catch {
        case _: Throwable => Printer.err(s"Non-zero exit code from: sudo pacman -S ${normals.map(_.name)}")
      }
      try {
        if (aur.nonEmpty) {
          %("bash", (home/".local"/"share"/"dotf"/"os"/"aur.sh").toString)
          %("yes", "|", "yay", "-S", aur.map(_.name))
        }
      } catch { 
        case _: Throwable => Printer.err(s"Non-zero exit code from AUR install or yay -S ${aur.map(_.name)}") 
      }

    case _ => Printer.warn("Unsupported package system!")
  }

  def run(mode: Mode.M, scripts: os.Path*): Unit = mode match {
    case Mode.Dry => scripts.foreach(s => println(s"bash ${s.toString}"))
    case Mode.Install => scripts.foreach { s =>
      try %("bash", s.toString) catch {
        case _: Throwable => Printer.err(s"Non-zero exit code from: bash ${s.toString}")
      }
    }
  }

  private def which(bin: String): Boolean = 
    Try(%%("which", bin).exitCode == 0).isSuccess

}

/** Defines an OS dependent package descriptor.
  *
  * @param data package Json node
  */
final case class Pkg(path: os.Path, data: JsonObject) {
  private val root = data.toList.head

  private def rootField[T: Decoder](name: String): Option[T] = 
    root._2.hcursor.downField(name).as[T].toOption

  private def field[T: Decoder](name: String): Option[T] = for {
    os <- OS.pkgSystem.flatMap(v => rootField[Json](v.name))
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

  /** Tests if this pkg requires special instructions and cannot be batched. */
  def isSpecial: Boolean = preInstallScripts.nonEmpty || postInstallScripts.nonEmpty

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
