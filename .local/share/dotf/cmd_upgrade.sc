import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $file.common, common._

@main
def main(mode: String = "install", verbose: Boolean = false): Unit = {
  val pkgs = filteredPkgs()

  if (pkgs.isEmpty) Printer.ok("System is up to date!")
  else {
    val (batch, specials) = pkgs.partition(_.isSpecial)

    runPreInstall(mode, specials)
    runPkgInstall(mode, batch)
    runPostInstall(mode, specials)

  }

}

private def runPreInstall(mode: String, pkgs: Seq[Pkg]): Unit = pkgs.foreach { pkg =>
  val cmds = pkg.preInstall
  if (cmds.nonEmpty) {
    mode match {
      case "dry" => println(s"bash -c ${cmds.mkString(" ")}")
      case _ => %("bash", "-c", cmds)
    }
  }
}

private def runPostInstall(mode: String, pkgs: Seq[Pkg]): Unit = pkgs.foreach { pkg =>
  val cmds = pkg.postInstall
  if (cmds.nonEmpty) {
    mode match {
      case "dry" => println(s"bash -c ${cmds.mkString(" ")}")
      case _ => %("bash", "-c", cmds)
    }
  }
}

private def runPkgInstall(mode: String, pkgs: Seq[Pkg]): Unit = (mode, OS.pkgSystem) match {
    case ("dry", Some(Homebrew)) if pkgs.nonEmpty =>
      val (casks, normals) = pkgs.partition(_.isCask)
      println("brew upgrade")
      if (normals.nonEmpty) println(s"brew install ${normals.map(_.name).mkString(" ")}")
      if (casks.nonEmpty) println(s"brew cask install ${casks.map(_.name).mkString(" ")}")
    case ("install", Some(Homebrew)) if pkgs.nonEmpty =>
      %("brew", "upgrade")
      val (casks, normals) = pkgs.partition(_.isCask)
      if (normals.nonEmpty) %("brew", "install", normals.map(_.name))
      if (casks.nonEmpty) %("brew", "cask", "install", casks.map(_.name))

    case ("dry", Some(Apt)) if pkgs.nonEmpty =>
      println("sudo apt update")
      println("sudo apt upgrade")
      println(s"sudo apt install ${pkgs.map(_.name).mkString(" ")}")
    case ("install", Some(Apt)) if pkgs.nonEmpty =>
      %sudo("apt", "update")
      %sudo("apt", "upgrade")
      %sudo("apt", "install", pkgs.map(_.name))

    case ("dry", Some(Pacman)) if pkgs.nonEmpty =>
      println("sudo pacman -Syy")
      println("sudo pacman -Su")
      println(s"sudo pacman -S ${pkgs.map(_.name).mkString(" ")}")
    case ("install", Some(Pacman)) if pkgs.nonEmpty =>
      %sudo("pacman", "-Syy")
      %sudo("pacman", "-Su")
      %("sudo", "pacman", "-S", pkgs.map(_.name))
    case (_, Some(_)) =>
      Printer.ok("System is up to date!")
    case _ =>
      Printer.err("Unsupported!")
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

      pkgs.filterNot(p => installed.contains(p.name))

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
