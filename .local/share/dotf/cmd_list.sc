import ammonite.ops._

import $file.common
import common._

implicit val wd = home

@main
def main(action: String = "all"): Unit = action match {
  case "pkgs" => Const.pkgs().foreach(p => println(p.name))
  case "pkgs-all" => Const.pkgAll().foreach(p => println(p.name))
  case "warn" => Const.pkgAll().flatMap(_.warn).foreach(println)
  case "custom" => Const.customInstallers().foreach(println)
  case "untracked" => showUntracked()
  case "tracked" => Const.managedFiles().foreach(println)
  case other => Printer.err(s"[ERROR]: Invalid command '$other'")
}

private def showUntracked(): Unit = {
  val trackedFiles = Const.managedFiles()
  val allRoots = Const.unmanagedDirs()
  val (tracked, untracked) = allRoots.partition(r => trackedFiles.exists(_.startsWith(r)))

  Printer.info(s"\nUntraced directories in $home")
  untracked.foreach(println)

  tracked.foreach { p =>
    val gitDir = (home/".dotf").toString
    val workTree = home.toString
    val cmd = %%("git", s"--git-dir=$gitDir", s"--work-tree=$workTree", "ls-files", "--other", "--directory", p)
    val untrackedTargets = cmd.out.lines

    if (untrackedTargets.nonEmpty) {
      Printer.info(s"\nUntracked items in $p")
      untrackedTargets.foreach(println)
    }
  }
}
