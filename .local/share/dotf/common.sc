import ammonite.ops._

object Const {

  implicit val ws = home

  val dotfDir = home/".dotf"

  def pkgFiles(): Seq[os.Path] = {
    val cmd = %%("git", s"--git-dir=${dotfDir.toString}", s"--work-tree=${ws.toString}", "ls-tree", "-r", "HEAD", "--name-only")
    val paths = cmd.out.lines.map(l => os.Path(s"${home.toString}/$l")).toSeq

    paths.filter(_.ext == "pkg")
  }
}

Const.pkgFiles().foreach(println)
