import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $file.common, common._
import $file.builder, builder._

@main
def main(mode: String = "install", verbose: Boolean = false): Unit = {
  val pkgs = filteredPkgs()
  val cust = Const.customInstallers()

  // Regenerate homepage
  Builder.genHomepage()

  // Custom installers
  mode match {
    case "dry" => cust.foreach(c => println(s"bash ${c.toString}"))
    case _ => cust.foreach(c => %("bash", c.toString))
  }

  if (pkgs.size == 0) {
    Printer.ok(">>> System is up to date!")
  } else {
    val (specials, batch) = pkgs.partition(_.isSpecial)

    runPreInstall(mode, specials)
    runPkgBatchInstall(mode, batch)
    runPkgSpecialInstall(mode, specials)
    runPostInstall(mode, specials)
  }
}

private def runPreInstall(mode: String, pkgs: Seq[Pkg]): Unit = pkgs.foreach { pkg =>
  val scripts = pkg.preInstallScripts

  if (scripts.nonEmpty) {
    mode match {
      case "dry" => scripts.foreach(s => println(s"bash ${s.toString}"))
      case _ => scripts.foreach(s => %("bash", s.toString))
    }
  }
}

private def runPostInstall(mode: String, pkgs: Seq[Pkg]): Unit = pkgs.foreach { pkg =>
  val scripts = pkg.postInstallScripts

  if (scripts.nonEmpty) {
    mode match {
      case "dry" => scripts.foreach(s => println(s"bash ${s.toString}"))
      case _ => scripts.foreach(s => %("bash", s.toString))
    }
  }
}

private def runPkgBatchInstall(mode: String, pkgs: Seq[Pkg]): Unit = (mode, OS.pkgSystem) match {
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

private def runPkgSpecialInstall(mode: String, pkgs: Seq[Pkg]): Unit = (mode, OS.pkgSystem) match {
  case ("dry", Some(Homebrew)) if pkgs.nonEmpty =>
    val (casks, normals) = pkgs.partition(_.isCask)
    println("brew upgrade")
    normals.foreach(n => println(s"brew install ${n.name}"))
    casks.foreach(c => println(s"brew cask install ${c.name}"))
  case ("install", Some(Homebrew)) if pkgs.nonEmpty =>
    val (casks, normals) = pkgs.partition(_.isCask)
    %("brew upgrade")
    normals.foreach(n => %("brew", "install", n.name))
    casks.foreach(c => %("brew", "cask", "install", c.name))

  case ("dry", Some(Apt)) if pkgs.nonEmpty =>
    println("sudo apt update")
    println("sudo apt upgrade")
    pkgs.foreach(p => println(s"sudo apt install ${p.name}"))
  case ("install", Some(Apt)) if pkgs.nonEmpty =>
    %sudo("apt", "update")
    %sudo("apt", "upgrade")
    pkgs.foreach(p => %sudo("apt", "install", p.name))

  case ("dry", Some(Pacman)) if pkgs.nonEmpty =>
    println("sudo pacman -Syy")
    println("sudo pacman -Su")
    pkgs.foreach(p => println(s"sudo pacman -S ${p.name}"))
  case ("install", Some(Pacman)) if pkgs.nonEmpty =>
    %sudo("pacman", "-Syy")
    %sudo("pacman", "-Su")
    pkgs.foreach(p => %sudo("pacman", "-S", p.name))

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
