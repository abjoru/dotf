import ammonite.ops._

import $file.common
import common._

@main
def main(action: String = "all"): Unit = action match {
  case "pkgs" => Const.pkgFiles().flatMap(_.pkgs()).foreach(p => println(p.name))
  case _ => Const.managedFiles().foreach(println)
}
