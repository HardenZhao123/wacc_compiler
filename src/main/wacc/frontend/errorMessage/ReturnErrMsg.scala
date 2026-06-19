package wacc.frontend.errorMessage

import wacc.common.PositionInfo
import wacc.frontend.returnCheck.NonReturnErr
import java.nio.file.Paths

object ReturnErrSnippet extends Snippet{
  // render the snippet, highlight the code at given length
  def renderSnippetWithLen(
                            input: String,
                            pos: PositionInfo,
                            caretLength: Int,
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
          val carets = "^" * math.max(1, caretLength)
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

object NonReturnErrorFormat {
  // api for generating error message 
  def format(
              path: String,
              input: String,
              errors: Seq[NonReturnErr]
            ): String = {
    val file = Paths.get(path).getFileName.toString
    errors.map(e => renderOne(file, input, e)).mkString("\n\n")
  }

  // render error message for non-return error
  private def renderOne(
                         file: String,
                         input: String,
                         err: NonReturnErr
                       ): String = {
    val PositionInfo(row, col) = err.pos

    val header =
      s"Syntax error (Non-Returning Function Error) in $file ($row, $col):"

    val details =
      err.msg

    // Extract the exact source line
    val line = input.linesIterator.drop(row - 1).nextOption().getOrElse("")

    // Caret covers from function start to end of line
    val caretLen =
      math.max(1, line.length - (col - 1))

    val snippet =
      ReturnErrSnippet.renderSnippetWithLen(
        input,
        err.pos,
        caretLen
      )

    s"$header\n$details\n$snippet"
  }
}