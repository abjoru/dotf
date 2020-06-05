import ammonite.ops._

import $file.common
import common._

@main
def main(action: String = "all"): Unit = action match {
  case "pkgs" => listPkgs()
  case _ => Const.managedFiles().foreach(println)
}

private def listPkgs(): Unit = {
  implicit val ws = home

  val pkgFiles = Const.managedFiles().filter(_.ext == "pkg")
  val allPkgs = pkgFiles.flatMap(f => read.lines! f)

  allPkgs.foreach(println)
}
