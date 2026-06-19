package wacc.frontend.returnCheck

import wacc.common.PositionInfo

// Represents an error related to functions that fail to guarantee a return value
sealed trait NonReturnErr{
  def pos: PositionInfo
  def msg: String
}

object NonReturnErr {
  // Error indicating that a function does not guarantee a return on all control paths
  // For example, a function may exit without reaching a return statement.
  final case class NonReturningFunction(name: String, pos: PositionInfo)
    extends NonReturnErr {
    val msg: String = s"function '$name' missing a returning block"
  }
}
