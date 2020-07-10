import com.scalapenos.sbt.prompt._
import sbt._
import complete.Parser
import complete.DefaultParsers._

import java.io.File

val osx = ""
val linux = ""

lazy val cleanIvy2 = taskKey[Unit]("Cleans Ivy2 Cache for the given project")

cleanIvy2 := {
  val log = streams.value.log
  val orgName = Keys.organization.value
  val moduleName = Keys.moduleName.value
  val ivyHome = Option(System.getProperty("sbt.ivy.home"))

  val files = for {
    base <- ivyHome
    baseDir = new File(base)
    localPath = new File(baseDir, "local")
    cachePath = new File(baseDir, "cache")
    localFiles = (localPath ** ("*" + orgName + "*") ** ("*" + moduleName + "*")).get
    cacheFiles = (cachePath ** ("*" + orgName + "*") ** ("*" + moduleName + "*")).get
  } yield localFiles ++ cacheFiles

  files match {
    case Some(fx) if fx.nonEmpty => 
      log.info(s"Cleaning ${fx.size} files for module: $moduleName")
      IO.delete(fx)
    case None if ivyHome.isEmpty =>
      log.warn(s"Unable to clean ivy directories! sbt.ivy.home not set...")
    case _ => 
      log.warn(s"Nothing to be cleaned for module: $moduleName")
  }
}

promptTheme := PromptTheme(
  List(
    text("  ", fg(9).bg(235)),
    text("", fg(235).bg(239)),
    currentProject(fg(251).bg(239)).padLeft("  ").padRight("  "),
    text("", fg(15).bg(239)),
    gitBranch(clean = fg(236).bg(15), dirty = fg(52).bg(15)).padLeft("  ").padRight(" "),
    text(" ", fg(15))
    //text(s"${linux}  ", fg(15))
  )
)
