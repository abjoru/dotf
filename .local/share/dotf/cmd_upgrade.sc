import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $file.common, common._
import $file.builder, builder._

@main
def main(mode: String = "install", verbose: Boolean = false): Unit = {
  val m = Mode(mode)
  val pkgs = filteredPkgs()
  val cust = Const.customInstallers()

  // Regenerate homepage
  Builder.genHomepage()

  // Custom installers
  OS.run(m, cust: _*)

  if (pkgs.size == 0) {
    Printer.ok("System is up to date!")
  } else {
    OS.update(m)
    OS.run(m, pkgs.flatMap(_.preInstallScripts): _*)
    OS.install(m, pkgs: _*)
    OS.run(m, pkgs.flatMap(_.postInstallScripts): _*)
  }
}

private def filteredPkgs(): Seq[Pkg] = {
  val pkgs = Const.pkgs()

  // Always print warnings until physically removed!
  pkgs.flatMap(_.warn).foreach(w => Printer.warn(w))

  OS.pkgSystem match {
    case Some(Homebrew) =>
      val installed = %%("brew", "list").out.lines.foldLeft(Seq.empty[String]) {
        case (acc, line) => acc ++ line.split("\\s+")
      }
      val casks = %%("brew", "cask", "list").out.lines.foldLeft(Seq.empty[String]) {
        case (acc, line) => acc ++ line.split("\\s+")
      }

      val all = installed ++ casks
      pkgs.filterNot(p => all.contains(p.name))

    case Some(Apt) =>
      val installed = %%("apt", "list", "--installed").out.lines.foldLeft(Seq.empty[String]) {
        case (acc, line) => acc :+ line.takeWhile(_ != '/')
      }

      pkgs.filterNot(p => installed.contains(p.name))

    case Some(Pacman) =>
      val installed = %%("pacman", "-Q").out.lines.foldLeft(Seq.empty[String]) {
        case (acc, line) => acc :+ line.split("\\s+")(0)
      }

      pkgs.filterNot(p => installed.contains(p.name))
    case _ => pkgs
  }
}
