import ammonite.ops._

import $file.common
import common._

@main
def main(action: String = "all"): Unit = action match {
  case "pkgs" => Const.pkgs().foreach(p => println(p.name))
  case "pkgs-all" => Const.pkgAll().foreach(p => println(p.name))
  case "warn" => Const.pkgAll().flatMap(_.warn).foreach(println)
  case _ => Const.managedFiles().foreach(println)
}
