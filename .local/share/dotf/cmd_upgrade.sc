import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $file.common, common._

@main
def main(mode: String = "install", verbose: Boolean = false, action: String = "all"): Unit = {
  val pkgs = filteredPkgs()

  if (pkgs.isEmpty) Printer.ok("System is up to date!")
  else {
    val batch = pkgs.filterNot(_.isSpecial).map(_.name)
    val specials = pkgs.filter(_.isSpecial)
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

private def runPkgInstall(mode: String, pkgs: Seq[String]): Unit = (mode, OS.pkgSystem) match {
    case ("dry", Some(Homebrew)) =>
      println("brew upgrade")
      if (pkgs.nonEmpty) println(s"brew install ${pkgs.mkString(" ")}")
    case ("install", Some(Homebrew)) =>
      %("brew", "upgrade")
      if (pkgs.nonEmpty) %("brew", "install", pkgs)

    case ("dry", Some(Apt)) =>
      println("sudo apt update")
      println("sudo apt upgrade")
      if (pkgs.nonEmpty) println(s"sudo apt install ${pkgs.mkString(" ")}")
    case ("install", Some(Apt)) =>
      %sudo("apt", "update")
      %sudo("apt", "upgrade")
      if (pkgs.nonEmpty) %sudo("apt", "install", pkgs)

    case ("dry", Some(Pacman)) =>
      println("sudo pacman -Syy")
      println("sudo pacman -Su")
      if (pkgs.nonEmpty) println(s"sudo pacman -S ${pkgs.mkString(" ")}")
    case ("install", Some(Pacman)) =>
      %sudo("pacman", "-Syy")
      %sudo("pacman", "-Su")
      if (pkgs.nonEmpty) %("sudo", "pacman", "-S", pkgs)
    case _ =>
      Printer.err("Unsupported!")
}

private def filteredPkgs(): Seq[Pkg] = {
  val pkgs = Const.pkgs()

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
      val installed = %%("pacman", "-Qet").out.lines.foldLeft(Seq.empty[String]) {
        case (acc, line) => acc :+ line.split("\\s+")(0)
      }

      pkgs.filterNot(p => installed.contains(p.name))
    case _ => pkgs
  }
}
