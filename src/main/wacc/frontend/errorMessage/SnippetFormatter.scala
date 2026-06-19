package wacc.frontend.errorMessage

import wacc.common.PositionInfo

// used for generation of souce code snippet
trait Snippet {
  // number of lines shown before the error line
  protected final val ContextLinesBefore: Int = 1
  // number of lines shown after the error line
  protected final val ContextLinesAfter: Int = 1

  // Extracts source lines around a 1-based row index
  // Returns (lineNumber, lineContent) pairs
  protected final def contextLines(
                            input: String,
                            row1: Int,
                            linesBefore: Int,
                            linesAfter: Int
                          ): Seq[(Int, String)] = {
    val lines = input.linesIterator.toArray
    val start = math.max(1, row1 - linesBefore)
    val end = math.min(lines.length, row1 + linesAfter)
    (start to end).map(r => (r, lines(r - 1)))
  }

  // Computes how many caret characters should be drawn starting
  // at the given 1-based column index
  // If the column points at an identifier, the entire token is underlined
  // otherwise a single caret is produced
  protected final def caretLen(line: String, col1: Int): Int = {
    val i0 = math.max(0, col1 - 1)
    if (line.isEmpty) 1
    else if (i0 >= line.length) 1
    else {
      def isTokenChar(c: Char): Boolean =
        c.isLetterOrDigit || c == '_'

      val ch0 = line.charAt(i0)
      if (!isTokenChar(ch0)) 1
      else {
        var j = i0
        while (j < line.length && isTokenChar(line.charAt(j))) j += 1
        math.max(1, j - i0)
      }
    }
  }

  // render snippet, highlight the given position
  def renderSnippet(
                             input: String,
                             pos: PositionInfo,
                             linesBefore: Int = ContextLinesBefore,
                             linesAfter: Int = ContextLinesAfter
                           ): String = {
    val row = pos.row
    val col = pos.column

    val ctx = contextLines(input, row, linesBefore, linesAfter)

    val snippetLines =
      ctx.flatMap { case (r, line) =>
        if (r == row) {
          val pad = " " * math.max(0, col - 1)
          val carets = "^" * caretLen(line, col)
          Seq(
            s"|$line",
            s"|$pad$carets"
          )
        } else {
          Seq(s"|$line")
        }
      }

    ("|" +: snippetLines :+ "|").mkString("\n")
  }
}

// snippet object
object Snippet extends Snippet
