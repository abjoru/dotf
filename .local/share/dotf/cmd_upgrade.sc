import ammonite.ops._

import $file.common, common._

implicit val wc = home

@main
def main(mode: String = "install", verbose: Boolean = false): Unit = {
  val pkgs = filteredPkgs()
  val batch = pkgs.filterNot(_.isSpecial).map(_.cmd.mkString)
  val specials = pkgs.filter(_.isSpecial).map(_.cmd)

  (mode, OS.pkgSystem) match {
    case ("dry", Some(Homebrew)) =>
      println("brew update && brew upgrade")
      if (batch.nonEmpty) println(s"brew install ${batch.mkString(" ")}")
      specials.foreach(p => println(s"brew install ${p.mkString(" ")}"))
    case ("install", Some(Homebrew)) =>
      %("brew", "update", "&&", "brew", "upgrade")
      if (batch.nonEmpty) %("brew", "install", batch)
      specials.foreach(p => %("brew", "install", p))

    case ("dry", Some(Apt)) =>
      println("sudo apt update && sudo apt upgrade")
      if (batch.nonEmpty) println(s"sudo apt install ${batch.mkString(" ")}")
      specials.foreach(p => println(s"sudo apt install ${p.mkString(" ")}"))
    case ("install", Some(Apt)) =>
      %("sudo", "apt", "update", "&&", "sudo", "apt", "upgrade")
      if (batch.nonEmpty) %("sudo", "apt", "install", batch)
      specials.foreach(p => %("sudo", "apt", "install", p))

    case ("dry", Some(Pacman)) =>
      println("sudo pacman -Syy && sudo pacman -Su")
      if (batch.nonEmpty) println(s"sudo pacman -S ${batch.mkString(" ")}")
      specials.foreach(p => println(s"sudo pacman -S ${p.mkString(" ")}"))
    case ("install", Some(Pacman)) =>
      %("sudo", "pacman", "-Syy", "&&", "sudo", "pacman", "-Su")
      if (batch.nonEmpty) %("sudo", "pacman", "-S", batch)
      specials.foreach(p => %("sudo", "pacman", "-S", p))
    case _ =>
      Printer.err("Unsupported!")
  }
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
