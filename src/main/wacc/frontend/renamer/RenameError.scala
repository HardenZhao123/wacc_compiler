package wacc.frontend.renamer

import wacc.common.PositionInfo
import wacc.frontend.ast.Type

// entry in error messages (use unrenamed name)
final case class ScopeEntry(name: String, t: Type, declaredAt: PositionInfo)


// Base type for all renaming errors. With source position and err message
sealed trait RenameError {def pos: PositionInfo; def msg: String}

object RenameError {
  // variable that is not visible in the current scope
  final case class OutOfScope(name: String,
                               pos: PositionInfo,
                               visible: List[ScopeEntry] // the list of variables (entries) that is in the scope
                             ) extends RenameError {
    val msg = s"out-of-scope: $name"
  }

  // redeclaration of a variable within the same scope
  final case class Redeclaration(name: String, pos: PositionInfo, prevPos: PositionInfo) extends RenameError {
    val msg = s"redeclaration: $name"
  }

  // duplicate definition of a function
  final case class FuncRedeclaration(name: String, pos: PositionInfo, prevPos: PositionInfo) extends RenameError {
    val msg = s"function redeclaration: $name"
  }

  // call to a function that is not defined
  final case class FuncOutOfScope(name: String, pos: PositionInfo) extends RenameError {
    val msg = s"function out-of-scope: $name"
  }
}
