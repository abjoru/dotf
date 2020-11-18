import ammonite.ops._

import $ivy.`io.circe::circe-generic:0.12.0`
import $ivy.`io.circe::circe-parser:0.12.0`
import $ivy.`org.typelevel::cats-effect:2.1.0`

import cats.implicits._
import cats.effect.Sync
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._

import scala.io.Source

import java.io.{File, PrintWriter}

final case class Link(name: String, link: String)

object Link {
  implicit val linkDecoder: Decoder[Link] = deriveDecoder[Link]
}

final case class Group(group: String, hostFilter: Option[String], links: Seq[Link])

object Group {
  implicit val groupDecoder: Decoder[Group] = deriveDecoder[Group]
}

object Builder {

  private val skelDir = home/".local"/"share"/"dotf"/"skel"

  private val targetDir = home/".cache"/"dotf"

  private def hostname[F[_]: Sync](implicit ws: os.Path) = Sync[F].delay(%%("hostname").out.string)

  private def readJson[F[_]: Sync](file: os.Path): F[Json] = for {
    f <- new File(file.toString).pure[F]
    c <- Sync[F].delay(Source.fromFile(f).getLines.mkString)
    r <- parse(c).liftTo[F]
  } yield r

  private def readString[F[_]: Sync](file: os.Path): F[String] = for {
    f <- Sync[F].pure(new File(file.toString))
    r <- Sync[F].delay(Source.fromFile(f).getLines.mkString("\n"))
  } yield r

  private def getGroups[F[_]: Sync](file: os.Path): F[Seq[Group]] = for {
    a <- readJson(file)
    b <- a.as[Seq[Group]].liftTo[F]
  } yield b

  private def matchesHost(host: String)(grp: Group): Boolean = grp.hostFilter match {
    case Some(f) => f.r.findFirstMatchIn(host).isDefined
    case _ => true
  }

  private def groups[F[_]: Sync](implicit ws: os.Path): F[Seq[Group]] = for {
    hn <- hostname[F]
    gs <- getGroups(skelDir/"links.json")
    rs <- gs.filter(matchesHost(hn)).pure[F]
  } yield rs

  private def writeFile[F[_]: Sync](path: os.Path, contents: String): F[Unit] = Sync[F].delay {
    val f = new File(path.toString)

    if (!f.exists()) {
      f.getParentFile().mkdirs()
    } else f.delete()

    val pw = new PrintWriter(f)
    pw.write(contents)
    pw.close()
  }

  def genHomepage[F[_]: Sync](implicit ws: os.Path): F[Unit] = for {
    header <- readString(skelDir/"head.html")
    footer <- readString(skelDir/"tail.html")
    styles <- readString(skelDir/"homepage.css")
    links  <- groups.map(_.foldLeft("")(genLinks))

    _ <- writeFile(targetDir/"homepage.html", header ++ "\n" ++ links ++ footer)
    _ <- writeFile(targetDir/"homepage.css", styles)
  } yield ()

  private def genLinks(s: String, g: Group): String = {
    val links = g.links.map(l => s"""<a class="bookmark" href="${l.link}" target="_blank">${l.name}</a>""")
    val pre = s"""<div class="bookmark-set">\n<div class="bookmark-title">${g.group}</div>\n<div class="bookmark-inner-container">"""
    val post = "</div>\n</div>\n"

    s ++ pre ++ links.mkString("\n", "\n", "\n") ++ post
  }

}
