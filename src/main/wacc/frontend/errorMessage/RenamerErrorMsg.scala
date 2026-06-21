package wacc.frontend.errorMessage

import wacc.common.PositionInfo
import java.nio.file.Paths
import wacc.frontend.ast.typeToString
import wacc.frontend.renamer.{RenameError}

object RenamerErrorFormat {
  // render error message for renamer error:
  // 1 variable out of scope error 
  // 2 redeclaration error
  // 3 function redeclaration error
  // 4 function out of scope error
  private def renderOne(file: String, input: String, err: RenameError): String = {
    val PositionInfo(row, col) = err.pos

    val (header, details) = err match {

      case RenameError.OutOfScope(name, pos, entries) =>
        val vars =
          if (entries.nonEmpty)
            "relevant in-scope variables include:\n" +
              entries
                .sortBy(_.declaredAt.row)
                .map(e => s"    ${typeToString(e.t) } ${e.name} (declared on line ${e.declaredAt.row})")
                .mkString("\n")
          else ""

        (
          s"Scope error in $file ($row, $col):",
          s"variable $name has not been declared in this scope\n$vars"
        )

      case RenameError.Redeclaration(name, pos, prevPos) =>
        (
          s"Scope error in $file ($row, $col):",
          s"illegal redeclaration of variable $name\n  previously declared (in this scope) on line ${prevPos.row}"
        )

      case RenameError.FuncRedeclaration(name, pos, prevPos) =>
        (
          s"Function redefinition error in $file ($row, $col):",
          s"illegal redefinition of function $name\n  previously declared on line ${prevPos.row}"
        )

      case RenameError.FuncOutOfScope(name, pos) =>
        (
          s"Undefined function error in $file ($row, $col):",
          s"function $name has not been defined"
        )
    }

    val snippet = Snippet.renderSnippet(input, err.pos)
    s"$header\n$details\n$snippet"
  }

  // api for generating the renamer error message
  // given the file path and input source code and all renamer errors
  def format(path: String, input: String, errors: Seq[RenameError]): String = {
    val file = Paths.get(path).getFileName.toString
    errors.map(e => renderOne(file, input, e)).mkString("\n\n")
  }
}
