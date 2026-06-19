package wacc.frontend.ast

import wacc.common.PositionInfo
import wacc.frontend.ParserBridge.ParserBridgePos1

// Exceptions
sealed trait WACCException {
  val positionInfo: PositionInfo
  val msg: Expr
}
object WACCException {
  case class ArrayOutOfBoundsException(msg: Expr)(override val positionInfo: PositionInfo) extends WACCException
  case class BadCharException(msg: Expr)(override val positionInfo: PositionInfo) extends WACCException
  case class ArithmeticException(msg: Expr)(override val positionInfo: PositionInfo) extends WACCException
  case class IntegerOverflowException(msg: Expr)(override val positionInfo: PositionInfo) extends WACCException
  case class NullDereferenceException(msg: Expr)(override val positionInfo: PositionInfo) extends WACCException
  case class GeneralException(msg: Expr)(override val positionInfo: PositionInfo) extends WACCException

  // Parser bridges for exceptions
  object ArrayOutOfBoundsException extends ParserBridgePos1[Expr, ArrayOutOfBoundsException]
  object BadCharException extends ParserBridgePos1[Expr, BadCharException]
  object ArithmeticException extends ParserBridgePos1[Expr, ArithmeticException]
  object IntegerOverflowException extends ParserBridgePos1[Expr, IntegerOverflowException]
  object NullDereferenceException extends ParserBridgePos1[Expr, NullDereferenceException]
  object GeneralException extends ParserBridgePos1[Expr, GeneralException]
}


