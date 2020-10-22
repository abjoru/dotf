import ammonite.ops._

import $file.common
import common._

@main
def main(action: String = "all", arg: String): Unit = action match {
  case "pkgs" => Const.pkgs().foreach(p => println(p.name))
  case "pkgs-all" => Const.pkgAll().foreach(p => println(p.name))
  case "warn" => Const.pkgAll().flatMap(_.warn).foreach(println)
  case "custom" => Const.customInstallers().foreach(println)
  case "untracked" => showUntracked(arg)
  case "tracked" => Const.managedFiles().foreach(println)
  case other => Printer.err(s"[ERROR]: Invalid command '$other'")
}

final case class Root(parent: String, tracked: Seq[String])

// FIXME This does not handle untracked in parent tracked directories!
private def showUntracked(arg: String): Unit = arg match {
  case s if s.trim.isEmpty =>
    val tracked = Const.managedFiles()

    tracked.map(v => v/up).distinct.filterNot(_ == home).foreach { p =>
      val (dx, fx) = (ls! p).partition(_.isDir)

      val untrackedDirs = dx.filterNot(d => tracked.contains(d)).filterNot { d =>
        tracked.find(_.toString.startsWith(d.toString)).isDefined
      }

      val untrackedFiles = fx.filterNot(f => tracked.contains(f))

      if (untrackedDirs.nonEmpty || untrackedFiles.nonEmpty)
        Printer.info(s"\nUntracked items in $p")

      untrackedDirs.foreach(println)
      untrackedFiles.foreach(println)
    }
  case p =>
    implicit val ws = home
    val gitDir = (home/".dotf").toString
    val workTree = home.toString
    %("git", s"--git-dir=$gitDir", s"--work-tree=${workTree}", "ls-files", "--other", "--directory", p)

}
