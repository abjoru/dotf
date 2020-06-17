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
    val files = ls! p |? (_.isDir == false)
    val untracked = files.filterNot(f => tracked.contains(f))
    
    if (untracked.nonEmpty) {
      Printer.info(s"\nUntracked files in $p")
      untracked.foreach(println)
    }
  }
}
