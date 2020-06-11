import com.scalapenos.sbt.prompt._

val osx = ""
val linux = ""

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
