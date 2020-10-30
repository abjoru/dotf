import ammonite.ops._

import $file.pkg, pkg._

import $ivy.`org.typelevel::cats-effect:2.1.0`

import cats.implicits._
import cats.effect.Sync

sealed trait Algebra[F[_]] {

  /** Update packages for the given OS. */
  def update(dry: Boolean)(implicit wd: os.Path): F[Unit]

  /** Install a sequence of packages for the given OS. */
  def install(dry: Boolean, pkgs: Seq[Pkg])(implicit wd: os.Path): F[Unit]

  /** List installed packages on the target OS. */
  def listInstalledPkgNames(implicit wd: os.Path): F[List[String]]

  protected def putLn(str: String)(implicit F: Sync[F]): F[Unit] = F.delay(println(str))

  protected def run(thunk: => Unit)(implicit F: Sync[F]): F[Unit] = F.delay(thunk)
}

object Algebra {

  def apply[F[_]: Sync](system: PkgSys): Algebra[F] = system match {
    case Apt => new AptInterpreter[F]
    case Pacman => new PacmanInterpreter[F]
    case Homebrew => new HomebrewInterpreter[F]
  }

  /** Execute the given sequence of bash scripts. */
  def runScripts[F[_]: Sync](dry: Boolean, scripts: Seq[os.Path])(implicit wd: os.Path): F[Unit] = dry match {
    case true => scripts.map(s => Sync[F].delay(println(s"bash ${s.toString}"))).toList.sequence *> Sync[F].unit
    case false => scripts.map(s => Sync[F].delay(%("bash", s.toString))).toList.sequence *> Sync[F].unit
  }
}

final class PacmanInterpreter[F[_]: Sync] extends Algebra[F] {

  def update(dry: Boolean)(implicit wd: os.Path): F[Unit] = dry match {
    case true => putLn("sudo pacman -Syyu")
    case false => run(%sudo("pacman", "-Syyu"))
  }

  def install(dry: Boolean, pkgs: Seq[pkg.Pkg])(implicit wd: Path): F[Unit] = dry match {
    case true => for {
      px <- pkgs.partition(_.isAur).pure[F]
      p1 <- px._1.map(_.name).mkString(" ").pure[F]
      p2 <- px._2.map(_.name).mkString(" ").pure[F]
      _ <- if (px._2.nonEmpty) putLn(s"sudo pacman -S $p2") else Sync[F].unit
      _ <- if (px._1.nonEmpty) putLn(s"sudo yay -S $p1") else Sync[F].unit
    } yield ()

    case false => for {
      px <- pkgs.partition(_.isAur).pure[F]
      p1 <- px._1.map(_.name).pure[F]
      p2 <- px._2.map(_.name).pure[F]
      _ <- if (px._2.nonEmpty) run(%sudo("pacman", "-S", p2)) else Sync[F].unit
      _ <- if (px._1.nonEmpty) run(%sudo("yay", "-S", p1)) else Sync[F].unit
    } yield ()
  }

  def listInstalledPkgNames(implicit wd: Path): F[List[String]] = 
    Sync[F].delay(%%("pacman", "-Q").out.lines.foldLeft(List.empty[String]) {
      case (acc, line) => acc :+ line.split("\\s+")(0)
    })
}

final class AptInterpreter[F[_]: Sync] extends Algebra[F] {

  def update(dry: Boolean)(implicit wd: os.Path): F[Unit] = dry match {
    case true => putLn("sudo apt update\nsudo apt upgrade")
    case false => run(%sudo("apt", "update")) *> run(%sudo("apt", "upgrade"))
  }

  def install(dry: Boolean, pkgs: Seq[Pkg])(implicit wd: os.Path): F[Unit] = dry match {
    case true => for {
      px <- pkgs.partition(_.isSnap).pure[F]
      p2 <- px._2.map(_.name).mkString(" ").pure[F]
      _ <- if (px._2.nonEmpty) putLn(s"sudo apt install $p2") else Sync[F].unit
      _ <- px._1.map(s => putLn(s"sudo snap install ${s.name} ${s.snapChannel.mkString(" ")}")).toList.sequence
    } yield ()

    case false => for {
      px <- pkgs.partition(_.isSnap).pure[F]
      p2 <- px._2.map(_.name).pure[F]
      _ <- if (px._2.nonEmpty) run(%sudo("apt", "install", p2)) else Sync[F].unit
      _ <- px._1.map(s => run(%sudo("snap", "install", s.name, s.snapChannel))).toList.sequence
    } yield ()
  }

  def listInstalledPkgNames(implicit wd: Path): F[List[String]] = 
    Sync[F].delay(%%("apt", "list", "--installed").out.lines.foldLeft(List.empty[String]) {
      case (acc, line) => acc :+ line.split("\\s+")(0)
    })
}

final class HomebrewInterpreter[F[_]: Sync] extends Algebra[F] {

  def update(dry: Boolean)(implicit wd: Path): F[Unit] = dry match {
    case true => putLn("brew upgrade")
    case false => run(%("brew", "upgrade"))
  }

  def install(dry: Boolean, pkgs: Seq[pkg.Pkg])(implicit wd: Path): F[Unit] = dry match {
    case true => for {
      px <- pkgs.partition(_.isCask).pure[F]
      p1 <- px._1.map(_.name).mkString(" ").pure[F]
      p2 <- px._2.map(_.name).mkString(" ").pure[F]
      _ <- if (px._2.nonEmpty) putLn(s"brew install $p2") else Sync[F].unit
      _ <- if (px._1.nonEmpty) putLn(s"brew cask install $p1") else Sync[F].unit
    } yield ()

    case false => for {
      px <- pkgs.partition(_.isCask).pure[F]
      p1 <- px._1.map(_.name).pure[F]
      p2 <- px._2.map(_.name).pure[F]
      _ <- if (px._2.nonEmpty) run(%("brew", "install", p2)) else Sync[F].unit
      _ <- if (px._1.nonEmpty) run(%("brew", "cask", "install", p1)) else Sync[F].unit
    } yield ()
  }

  def listInstalledPkgNames(implicit wd: Path): F[List[String]] = {
    val brew = Sync[F].delay(%%("brew", "list").out.lines.foldLeft(List.empty[String]) {
      case (acc, line) => acc ++ line.split("\\s+")
    })

    val cask = Sync[F].delay(%%("brew", "cask", "list").out.lines.foldLeft(List.empty[String]) {
      case (acc, line) => acc ++ line.split("\\s+")
    })

    brew >>= (b => cask.map(_ ++ b))
  }
}
