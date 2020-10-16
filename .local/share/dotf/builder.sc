import ammonite.ops._

import $ivy.`io.circe::circe-generic:0.12.0`
import $ivy.`io.circe::circe-parser:0.12.0`

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._

import java.io.File

final case class Link(name: String, link: String)

object Link {
  implicit val linkDecoder: Decoder[Link] = deriveDecoder[Link]
}

final case class Group(group: String, hostFilter: Option[String], links: Seq[Link])

object Group {
  implicit val groupDecoder: Decoder[Group] = deriveDecoder[Group]
}

object Builder {

  implicit private val ws = home

  private val skelDir = home/".local"/"share"/"dotf"/"skel"

  private val targetDir = home/".cache"/"dotf"

  private val hostname = %%("hostname").out.string

  private def readJson(file: os.Path) = {
    val f = new File(file.toString)
    val c = scala.io.Source.fromFile(f).getLines.mkString
    parse(c)
  }

  private def readString(file: os.Path): String = {
    val f = new File(file.toString)
    scala.io.Source.fromFile(f).getLines.mkString("\n")
  }

  private def getGroups(file: os.Path) = for {
    a <- readJson(file)
    b <- a.as[Seq[Group]]
  } yield b

  private def matchesHost(grp: Group): Boolean = grp.hostFilter match {
    case Some(f) => f.r.findFirstMatchIn(hostname).isDefined
    case _ => true
  }

  private def groups(): Seq[Group] = for {
    grp <- getGroups(skelDir/"links.json").toOption.getOrElse(Seq.empty)
    if matchesHost(grp)
  } yield grp

  private def writeFile(path: os.Path, contents: String): Unit = {
    val f = new File(path.toString)

    if (!f.exists()) {
      f.getParentFile().mkdirs()
    } else f.delete()

    val pw = new java.io.PrintWriter(f)
    pw.write(contents)
    pw.close()
  }

  def genHomepage(): Unit = {
    val header = readString(skelDir/"head.html")
    val footer = readString(skelDir/"tail.html")
    val links = groups.foldLeft("")(genLinks)

    writeFile(targetDir/"homepage.html", header ++ "\n" ++ links ++ footer)
    writeFile(targetDir/"homepage.css", readString(skelDir/"homepage.css"))
  }

  private def genLinks(s: String, g: Group): String = {
    val links = g.links.map(l => s"""<a class="bookmark" href="${l.link}" target="_blank">${l.name}</a>""")
    val pre = s"""<div class="bookmark-set">\n<div class="bookmark-title">${g.group}</div>\n<div class="bookmark-inner-container">"""
    val post = "</div>\n</div>\n"

    s ++ pre ++ links.mkString("\n", "\n", "\n") ++ post
  }

}
