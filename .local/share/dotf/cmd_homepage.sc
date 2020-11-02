import ammonite.ops._
import ammonite.ops.ImplicitWd._

import $ivy.`org.typelevel::cats-effect:2.1.0`

import cats.implicits._
import cats.effect.IO

import $file.builder, builder._

@main
def main(): Unit = Builder.genHomepage[IO].unsafeRunSync()
