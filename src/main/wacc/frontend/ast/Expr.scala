package wacc.frontend.ast

import wacc.common.PositionInfo
import wacc.frontend.ParserBridge.{ParserBridgePos1, ParserBridgePos2}

sealed trait Expr {
  val positionInfo: PositionInfo
}
object Expr {

  // Atoms
  sealed trait Atom extends Expr

  // Literal values
  case class IntLiter(value: Int)(override val positionInfo: PositionInfo) extends Atom
  case class BooleanLiter(bool: Boolean)(override val positionInfo: PositionInfo) extends Atom
  case class CharLiter(char: Char)(override val positionInfo: PositionInfo) extends Atom
  case class FloatLiter(value: Float)(override val positionInfo: PositionInfo) extends Atom
  case class StringLiter(string: String)(override val positionInfo: PositionInfo) extends Atom
  case class PairLiter()(override val positionInfo: PositionInfo) extends Atom
  case class Identifier(ident: String)(override val positionInfo: PositionInfo) extends Atom
  case class ArrayElem(ident: Identifier, indices: List[Expr])(override val positionInfo: PositionInfo) extends Atom
  case class Parens(expr: Expr)(override val positionInfo: PositionInfo) extends Atom

  // Parser bridges for atoms (attach position info automatically)
  object IntLiter extends ParserBridgePos1[Int, IntLiter]
  object BooleanLiter extends ParserBridgePos1[Boolean, BooleanLiter]
  object CharLiter extends ParserBridgePos1[Char, CharLiter]
  object FloatLiter extends ParserBridgePos1[Float, FloatLiter]
  object StringLiter extends ParserBridgePos1[String, StringLiter]
  object Identifier extends ParserBridgePos1[String, Identifier]
  object ArrayElem extends ParserBridgePos2[Identifier, List[Expr], ArrayElem]
  object Parens extends ParserBridgePos1[Expr, Parens]

  // Unary operators
  sealed trait UnaryOperator extends Expr {
    val operand: Expr
  }

  case class Not(operand: Expr)(override val positionInfo: PositionInfo) extends UnaryOperator
  case class Neg(operand: Expr)(override val positionInfo: PositionInfo) extends UnaryOperator
  case class Len(operand: Expr)(override val positionInfo: PositionInfo) extends UnaryOperator
  case class Ord(operand: Expr)(override val positionInfo: PositionInfo) extends UnaryOperator
  case class Chr(operand: Expr)(override val positionInfo: PositionInfo) extends UnaryOperator
  case class BitNot(operand: Expr)(override val positionInfo: PositionInfo) extends UnaryOperator

  // Parser bridges for unary operators
  object Not extends ParserBridgePos1[Expr, Not]
  object Neg extends ParserBridgePos1[Expr, Neg]
  object Len extends ParserBridgePos1[Expr, Len]
  object Ord extends ParserBridgePos1[Expr, Ord]
  object Chr extends ParserBridgePos1[Expr, Chr]
  object BitNot extends ParserBridgePos1[Expr, BitNot]

  // Binary operators
  sealed trait BinaryOperator extends Expr {
    val left: Expr
    val right: Expr
  }
  case class Mul(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Div(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Mod(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Add(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Sub(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Greater(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class GreaterEqual(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Less(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class LessEqual(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Equal(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class NotEqual(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class And(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class Or(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class BitAnd(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator
  case class BitOr(left: Expr, right: Expr)(override val positionInfo: PositionInfo) extends BinaryOperator

  // Parser bridges for binary operators
  object Mul extends ParserBridgePos2[Expr, Expr, Mul]
  object Div extends ParserBridgePos2[Expr, Expr, Div]
  object Mod extends ParserBridgePos2[Expr, Expr, Mod]
  object Add extends ParserBridgePos2[Expr, Expr, Add]
  object Sub extends ParserBridgePos2[Expr, Expr, Sub]
  object Greater extends ParserBridgePos2[Expr, Expr, Greater]
  object GreaterEqual extends ParserBridgePos2[Expr, Expr, GreaterEqual]
  object Less extends ParserBridgePos2[Expr, Expr, Less]
  object LessEqual extends ParserBridgePos2[Expr, Expr, LessEqual]
  object Equal extends ParserBridgePos2[Expr, Expr, Equal]
  object NotEqual extends ParserBridgePos2[Expr, Expr, NotEqual]
  object And extends ParserBridgePos2[Expr, Expr, And]
  object Or extends ParserBridgePos2[Expr, Expr, Or]
  object BitAnd extends ParserBridgePos2[Expr, Expr, BitAnd]
  object BitOr extends ParserBridgePos2[Expr, Expr, BitOr]

  // Unary side-effecting expressions
  sealed trait UnarySideEffecting extends Expr {
    val operand: LValue
  }
  case class Increment(operand: LValue)(override val positionInfo: PositionInfo) extends UnarySideEffecting
  case class Decrement(operand: LValue)(override val positionInfo: PositionInfo) extends UnarySideEffecting

  object Increment extends ParserBridgePos1[LValue, Increment]
  object Decrement extends ParserBridgePos1[LValue, Decrement]

  // Binary side-effecting expressions
  sealed trait BinarySideEffecting extends Expr {
    val left: LValue
    val right: Expr
  }
  case class AddEqual(left: LValue, right: Expr)(override val positionInfo: PositionInfo) extends BinarySideEffecting
  case class SubEqual(left: LValue, right: Expr)(override val positionInfo: PositionInfo) extends BinarySideEffecting
  case class MulEqual(left: LValue, right: Expr)(override val positionInfo: PositionInfo) extends BinarySideEffecting
  case class DivEqual(left: LValue, right: Expr)(override val positionInfo: PositionInfo) extends BinarySideEffecting
  case class ModEqual(left: LValue, right: Expr)(override val positionInfo: PositionInfo) extends BinarySideEffecting

  object AddEqual extends ParserBridgePos2[LValue, Expr, AddEqual]
  object SubEqual extends ParserBridgePos2[LValue, Expr, SubEqual]
  object MulEqual extends ParserBridgePos2[LValue, Expr, MulEqual]
  object DivEqual extends ParserBridgePos2[LValue, Expr, DivEqual]
  object ModEqual extends ParserBridgePos2[LValue, Expr, ModEqual]
}
