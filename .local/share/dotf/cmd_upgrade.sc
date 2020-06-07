import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $file.common, common._

@main
def main(mode: String = "install", verbose: Boolean = false, action: String = "all"): Unit = {
  val pkgs = filteredPkgs()
  val batch = pkgs.filterNot(_.isSpecial).map(_.cmd.mkString)
  val specials = pkgs.filter(_.isSpecial).map(_.cmd)

  if (action != "pkgs") runPreInstall(mode, verbose, Const.preInstallFiles())

  (mode, OS.pkgSystem) match {
    case ("dry", Some(Homebrew)) =>
      println("brew upgrade")
      if (batch.nonEmpty) println(s"brew install ${batch.mkString(" ")}")
      specials.foreach(p => println(s"brew install ${p.mkString(" ")}"))
    case ("install", Some(Homebrew)) =>
      %("brew", "upgrade")
      if (batch.nonEmpty) %("brew", "install", batch)
      specials.foreach(p => %("brew", "install", p))

    case ("dry", Some(Apt)) =>
      println("sudo apt update")
      println("sudo apt upgrade")
      if (batch.nonEmpty) println(s"sudo apt install ${batch.mkString(" ")}")
      specials.foreach(p => println(s"sudo apt install ${p.mkString(" ")}"))
    case ("install", Some(Apt)) =>
      %sudo("apt", "update")
      %sudo("apt", "upgrade")
      if (batch.nonEmpty) %sudo("apt", "install", batch)
      specials.foreach(p => %sudo("apt", "install", p))

    case ("dry", Some(Pacman)) =>
      println("sudo pacman -Syy")
      println("sudo pacman -Su")
      if (batch.nonEmpty) println(s"sudo pacman -S ${batch.mkString(" ")}")
      specials.foreach(p => println(s"sudo pacman -S ${p.mkString(" ")}"))
    case ("install", Some(Pacman)) =>
      %sudo("pacman", "-Syy")
      %sudo("pacman", "-Su")
      if (batch.nonEmpty) %("sudo", "pacman", "-S", batch)
      specials.foreach(p => %("sudo", "pacman", "-S", p))
    case _ =>
      Printer.err("Unsupported!")
  }

  if (action != "pkgs") runPostInstall(mode, verbose, Const.postInstallFiles())
}

private def runPreInstall(mode: String, verbose: Boolean, files: Seq[os.Path]): Unit = mode match {
  case "dry" =>
    files.foreach(f => println(s"sh -c ${f.toString}"))
  case _ =>
    files.foreach(f => %("bash", "-c", f.toString))
}

private def runPostInstall(mode: String, verbose: Boolean, files: Seq[os.Path]): Unit = mode match {
  case "dry" =>
    files.foreach(f => println(s"sh -c ${f.toString}"))
  case _ =>
    files.foreach(f => %("bash", "-c", f.toString))
}

private def filteredPkgs(): Seq[Pkg] = {
  val pkgs = Const.pkgFiles().flatMap(_.pkgs())

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
