package wacc.frontend.errorMessage

import java.nio.file.Paths
import wacc.common.PositionInfo
import wacc.frontend.typeCheck.TypeCheckError

object SemanticError {
  // categorises a type checking error into a user-friendly error kind.
  private def kind(err: TypeCheckError): String = err match {
    case _: TypeCheckError.FuncArityMismatch | _: TypeCheckError.FuncNoMatchingOverload | _: TypeCheckError.FuncAmbiguousOverload => "Function call error"
    case _ => "Type error"
  }

  // render error message for type checking error
  private def renderOne(file: String, input: String, err: TypeCheckError): String = {
    val PositionInfo(row, col) = err.pos
    val header = s"${kind(err)} in $file ($row, $col):"
    val details = err.msg
    val snippet = Snippet.renderSnippet(input, err.pos)
    s"$header\n$details\n$snippet"
  }

  // api for generating type-checking error message
  def format(path: String, input: String, errors: Seq[TypeCheckError]): String = {
    val file = Paths.get(path).getFileName.toString
    errors.map(e => renderOne(file, input, e)).mkString("\n\n")
  }
}
