import ammonite.ops._
import Console._

import $ivy.`org.typelevel::cats-effect:2.1.0`

import cats.implicits._
import cats.effect.IO

import $file.common, common._
import $file.cfg, cfg.Cfg

implicit val wd = home

@main
def main(args: String*): Unit = args.toList.drop(1) match {
  case "pkgs" :: _ => listPackages().unsafeRunSync
  case "pkgs-all" :: _ => listAllPackages().unsafeRunSync
  case "warn" :: _ => listWarnings().unsafeRunSync
  case "custom" :: _ => listCustomInstallers().unsafeRunSync
  case "tracked" :: _ => listTracked().unsafeRunSync
  case "untracked" :: _ => listUntracked().unsafeRunSync
  case "config" :: _ => showConfig().unsafeRunSync
  case "dir" :: "." :: _ => listStatusForDir(pwd).unsafeRunSync
  case "dir" :: p :: _ if p.startsWith("~/") => listStatusForDir(normalize(p)).unsafeRunSync
  case "dir" :: p :: _ if p.nonEmpty => listStatusForDir(Path(p)).unsafeRunSync
  case other => Printer.err[IO](s"[ERROR]: Invalid command '$other'").unsafeRunSync
}

private def normalize(p: String): os.Path =
  Path(home.toString ++ p.drop(1))

private def listPackages(): IO[Unit] =
  DotF.packages[IO].map(_.foreach(p => println(p.name)))

private def listAllPackages(): IO[Unit] =
  DotF.allPackages[IO].map(_.foreach(p => println(p.name)))

private def listWarnings(): IO[Unit] = for {
  w <- DotF.allPackages[IO].map(_.flatMap(_.warn))
  _ <- w.foreach(println).pure[IO]
} yield ()

private def listCustomInstallers(): IO[Unit] = 
  DotF.customInstallers[IO].map(_.foreach(println))

private def listTracked(): IO[Unit] = 
  DotF.managedFiles[IO].map(_.foreach(println))

private def listUntracked(): IO[Unit] = for {
  tf <- DotF.managedFiles[IO]
  ar <- DotF.unmanagedDirs[IO]
  px <- ar.partition(r => tf.exists(_.startsWith(r))).pure[IO]
  _ <- Printer.info[IO](s"\nUntracked directories in $home")
  _ <- px._2.map(v => IO.delay(println(v))).toList.sequence
  _ <- printUntrackedTargets(px._1)
} yield ()

private def listStatusForDir(dir: os.Path): IO[Unit] = for {
  tracked <- DotF.managedFiles[IO].map(_.filter(_.startsWith(dir)))
  untracked <- collectUntrackedTargets(Seq(dir))
  ft <- tracked.map(_.toString).pure[IO]
  ut <- untracked.map(p => s"${RED}${p.toString}${RESET}").pure[IO]
  all <- (ft ++ ut).sorted.pure[IO]
  _ <- Printer.info[IO](s"\nFile status in $dir")
  _ <- IO.delay(all.foreach(println))
} yield ()

private def showConfig(): IO[Unit] = for {
  c <- Cfg.load[IO]
  _ <- Printer.info[IO]("\nCurrent configuration:")
  _ <- IO.delay(println(s"Headless: ${c.headless}"))
  _ <- IO.delay(println(s"Xmobar mappings: ${c.xmobarMappings.mkString(", ")}"))
} yield ()

private def printUntrackedTargets(paths: Seq[os.Path]): IO[Unit] = {
  val groups = paths.map { p =>
    val gitDir = (home/".dotf").toString
    val workTree = home.toString()
    val cmd = IO.delay(%%("git", s"--git-dir=$gitDir", s"--work-tree=$workTree", "ls-files", "--other", "--directory", p))

    cmd.flatMap { r =>
      val targets = r.out.lines
      if (targets.nonEmpty) {
        Printer.info[IO](s"\nUntracked items in $p") *> IO.delay(targets.foreach(println))
      } else IO.unit
    }
  }

  groups.toList.sequence *> IO.unit
}

private def collectUntrackedTargets(paths: Seq[os.Path]): IO[Seq[os.Path]] = {
  val groups = paths.map { p =>
    val gitDir = (home/".dotf").toString
    val workTree = home.toString()
    val cmd = IO.delay(%%("git", s"--git-dir=$gitDir", s"--work-tree=$workTree", "ls-files", "--other", "--directory", p))

    cmd.map(_.out.lines.map(v => Path(home.toString ++ "/" ++ v)).toSeq)
  }

  groups.toList.sequence.map(_.flatten)
}
