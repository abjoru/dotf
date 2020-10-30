import ammonite.ops._
import Console._
import scala.io.Source
import scala.util.Try

import $ivy.`org.typelevel::cats-effect:2.1.0`
import $ivy.`io.circe::circe-yaml:0.13.0`

import $file.pkg, pkg._
import $file.cfg, cfg.Cfg

import io.circe._
import io.circe.yaml.parser

import cats.implicits._
import cats.effect.Sync

object DotF {
  implicit val ws = home

  val dotfGitDir = home/".dotf"
  val dotfConfigDir = home/".config"/"dotf"

  def managedFiles[F[_]: Sync]: F[Seq[os.Path]] = Sync[F].delay {
    val cmd = %%("git", s"--git-dir=${dotfGitDir.toString}", s"--work-tree=${ws.toString}", "ls-files")
    cmd.out.lines.map(l => os.Path(s"${home.toString}/${l}")).toSeq
  }

  def unmanagedDirs[F[_]: Sync]: F[Seq[os.Path]] =
    Sync[F].delay(home.toIO.listFiles().filter(_.isDirectory()).map(os.Path(_)))

  def customInstallers[F[_]: Sync]: F[Seq[os.Path]] = 
    managedFiles[F].map(_.filter(_.last == "install.sh"))

  def allPackages[F[_]: Sync]: F[Seq[Pkg]] = for {
    pkgs <- managedFiles[F].map(_.filter(_.ext == "pkg"))
    objs <- pkgs.map(mkPkg[F]).toList.sequence
  } yield objs.flatten

  def packages[F[_]: Sync]: F[Seq[Pkg]] = for {
    cfg <- Cfg.load[F]
    fx1 <- allPackages[F].map(_.filterNot(_.isIgnored))
    fx2 <- fx1.filter(byHeadlessValue(cfg)).pure[F]
  } yield fx2

  private def mkPkg[F[_]: Sync](path: os.Path): F[Seq[Pkg]] = for {
    s <- pkgSystem[F]
    f <- Source.fromFile(path.toString).reader().pure[F]
    j <- parser.parse(f).liftTo[F]
  } yield j.asArray.map(_.flatMap(buildPkg(path, s)).toList).getOrElse(List.empty)
  
  private def buildPkg(path: os.Path, s: PkgSys)(data: Json): Option[Pkg] = data.asObject match {
    case Some(obj) => Some(Pkg(path, obj, s))
    case None => None
  }

  private def byHeadlessValue(c: Cfg)(pkg: Pkg): Boolean = 
    if (c.headless) pkg.isHeadless == true
    else true

  private def which[F[_]: Sync](bin: String): F[Boolean] = 
    Sync[F].delay(Try(%%("which", bin).exitCode == 0).isSuccess)

  /** Find the target package system if supported. */
  def pkgSystem[F[_]: Sync]: F[PkgSys] = System.getProperty("os.name", "generic").toLowerCase match {
    case s if s.contains("linux") =>
      val tuple = for {
        apt <- which[F]("apt")
        pac <- which[F]("pacman")
      } yield (apt -> pac)

      tuple.flatMap {
        case (true, _) => Sync[F].pure(Apt)
        case (_, true) => Sync[F].pure(Pacman)
        case _ => Sync[F].raiseError(new Exception("Unable to determine package system for linux!"))
      }

     case s if s.contains("mac") => 
       which[F]("brew").flatMap {
         case true => Sync[F].pure(Homebrew)
         case false => Sync[F].raiseError(new Exception("Missing Homebrew install!"))
       }
            
    case _ => Sync[F].raiseError(new Exception("Unsupported OS!"))
  }

}

object Printer {
  /** Prints the given `msg` in terminal green. */
  def ok[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay(println(s"${GREEN}${msg}${RESET}"))

  /** Prints the given `msg` in terminal red. */
  def err[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay(println(s"${RED}${msg}${RESET}"))

  /** Prints the given `msg` in terminal yellow. */
  def warn[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay(println(s"${YELLOW}${msg}${RESET}"))

  /** Prints the given `msg` in terminal cyan. */
  def info[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay(println(s"${CYAN}${msg}${RESET}"))
}
