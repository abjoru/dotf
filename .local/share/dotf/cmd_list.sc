import ammonite.ops._

import $file.common
import common._

// TODO make list untracked to check untracked files in each config parent (or parent in general)
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

}
