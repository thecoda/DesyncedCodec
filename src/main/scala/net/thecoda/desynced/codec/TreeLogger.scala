package net.thecoda.desynced.codec

class TreeLogger {

  enum LogEntry:
    case Start(name: String, text: String)
    case Atom(text: String)
    case End(name: String, text: String)

  var currentIndentLevel: Int = 0
  val logLines: collection.mutable.Buffer[(Int, LogEntry)] = collection.mutable.Buffer.empty

  def withIndent[A](fn: => A): A = {
    currentIndentLevel += 1
    val result = try {
      fn
    } finally currentIndentLevel -= 1
    if (currentIndentLevel == 0) dumpToConsole()
    result
  }

  def start(name: String, text: String): Unit =
    logLines += currentIndentLevel -> LogEntry.Start(name, text)

  def end(name: String, text: String): Unit =
    logLines += currentIndentLevel -> LogEntry.End(name, text)

  def atom(text: String): Unit =
    logLines += currentIndentLevel -> LogEntry.Atom(text)

  def treeLog(entry: LogEntry) = {
    logLines += currentIndentLevel -> entry
  }

  def dumpToConsole(): Unit = {
    logLines.toSeq.foldLeft(List.empty[(Int, LogEntry)]) {
      case ((sl, LogEntry.Start(sn, _)) :: acc, (el, LogEntry.End(en, et)))
        if sn == en && sl == el =>
        el -> LogEntry.Atom(s"$en = $et") :: acc
      case (acc, levelAndEntry) => levelAndEntry :: acc
    }.reverse.foreach {
      case (level, LogEntry.Start(name, text)) => println("  " * level + s"begin $name $text")
      case (level, LogEntry.Atom(text)) => println("  " * level + text)
      case (level, LogEntry.End(name, text)) => println("  " * level + s"end $name = $text")
    }
  }
}
