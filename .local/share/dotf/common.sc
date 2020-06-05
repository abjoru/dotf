import ammonite.ops._
import Console._

object Const {

  implicit val ws = home

  val dotfDir = home/".dotf"

  def managedFiles(): Seq[os.Path] = {
    val cmd = %%("git", s"--git-dir=${dotfDir.toString}", s"--work-tree=${ws.toString}", "ls-files")
    cmd.out.lines.map(l => os.Path(s"${home.toString}/$l")).toSeq
  }

  def pkgFiles(): Seq[PkgFile] = {
    managedFiles().filter(_.ext == "pkg").collect {
      case f if isSupported(f) => PkgFile(f)
    }
  }

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
    %%("which", bin).exitCode == 0
  }

}

final case class PkgFile(path: os.Path) {
  private val pattern = """^([a-zA-Z]+)\['(.+[^\]])'\]$""".r

  def pkgs(): Seq[Pkg] = {
    val lines = (read.lines! path).filterNot(l => l.startsWith("#") || l.isEmpty)

    lines.map {
      case pattern(name, cmd) => Pkg(name, cmd.split("\\s+").toSeq)
      case name => Pkg(name, Seq(name))
    }
  }
}

final case class Pkg(name: String, cmd: Seq[String]) {
  def isSpecial: Boolean = cmd.size > 1
}
