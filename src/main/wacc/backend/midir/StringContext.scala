package wacc.backend.midir

import scala.collection.mutable
import wacc.backend.midir.TAC.*

final class StringContext(val wordSize: Int) {

  private val stringMap: mutable.Map[String, Int] = mutable.Map.empty

  private def addEscapes(str: String): String =
    // Strings are canonicalized before interning so identical literals share one TACStr entry.
    val sb: StringBuilder = new StringBuilder()
    for (char <- str) {
      char match {
        case '"' | '\'' =>
          sb.addOne('\\')
          sb.addOne(char)
        case '\n' =>
          sb.addOne('\\')
          sb.addOne('n')
        case '\r' =>
          sb.addOne('\\')
          sb.addOne('r')
        case _ => sb.addOne(char)
      }
    }
    sb.result

  def getString(str: String): TACStr = {
    val strEscaped = addEscapes(str)
    val id = stringMap.getOrElse(
      strEscaped, {
        // The insertion order becomes the final string table order exposed through `strings`.
        val freshId = stringMap.size
        stringMap.addOne(strEscaped, freshId)
        freshId
      }
    )

    TACStr(id)
  }

  def strings: Seq[String] = stringMap.toIndexedSeq.sortBy(_._2).map(_._1)
}
