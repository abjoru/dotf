import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $ivy.`org.typelevel::cats-effect:2.1.0`

import cats.implicits._
import cats.effect.IO

import $file.common, common._
import $file.builder, builder._
import $file.pkg, pkg._
import $file.algebra, algebra.Algebra

@main
def main(mode: String = "install", verbose: Boolean = false): Unit = 
  run(mode).unsafeRunSync()

private def run(mode: String): IO[Unit] = for {
  dry  <- (mode != "install").pure[IO]
  sys  <- DotF.pkgSystem[IO]
  alg  <- Algebra[IO](sys).pure[IO]
  pkgs <- DotF.packages[IO].flatMap(filterPkgs(alg))
  cust <- DotF.customInstallers[IO]
  _    <- Builder.genHomepage[IO]
  _    <- Algebra.runScripts[IO](dry, cust)
  _    <- installUpdates(dry, pkgs, alg)
} yield ()

private def installUpdates(dry: Boolean, pkgs: Seq[Pkg], alg: Algebra[IO]): IO[Unit] = {
  if (pkgs.size == 0) Printer.ok[IO]("System is up to date!")
  else for {
    _ <- alg.update(dry)(home)
    _ <- Algebra.runScripts[IO](dry, pkgs.flatMap(_.preInstallScripts))
    _ <- alg.install(dry, pkgs)(home)
    _ <- Algebra.runScripts[IO](dry, pkgs.flatMap(_.postInstallScripts))
  } yield ()
}

private def filterPkgs(alg: Algebra[IO])(pkgs: Seq[Pkg]): IO[Seq[Pkg]] = {
  val warnIo = pkgs.flatMap(_.warn).map(s => Printer.warn[IO](s)).toList.sequence
  val namesIo = alg.listInstalledPkgNames(home)

  warnIo *> namesIo.map(n => pkgs.filterNot(p => n.contains(p.name)))
}
