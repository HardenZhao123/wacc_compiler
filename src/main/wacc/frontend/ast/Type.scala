package wacc.frontend.ast

import wacc.common.PositionInfo
import wacc.frontend.ParserBridge.ParserBridgePos2

sealed trait Type {
  val positionInfo: PositionInfo
}

// Primitive types
case class IntType()(override val positionInfo: PositionInfo) extends Type
case class BoolType()(override val positionInfo: PositionInfo) extends Type
case class CharType()(override val positionInfo: PositionInfo) extends Type
case class StringType()(override val positionInfo: PositionInfo) extends Type

// Array type with element type and number of dimensions
case class ArrayType(elem: Type, dimensions: Int)(override val positionInfo: PositionInfo) extends Type {
  require(dimensions >= 1)
}
object ArrayType extends ParserBridgePos2[Type, Int, ArrayType]

// Pair type with two component types
case class PairType(fst: Type, snd: Type)(override val positionInfo: PositionInfo) extends Type
object PairType extends ParserBridgePos2[Type, Type, PairType]
case class PairErasedType()(override val positionInfo: PositionInfo) extends Type

// Exception type
sealed trait WACCExceptionType extends Type {
  override val positionInfo: PositionInfo
}
case class Arithmetic()(override val positionInfo: PositionInfo) extends WACCExceptionType
case class BadChar()(override val positionInfo: PositionInfo) extends WACCExceptionType
case class ArrayOutOfBounds()(override val positionInfo: PositionInfo) extends WACCExceptionType
case class IntegerOverflow()(override val positionInfo: PositionInfo) extends WACCExceptionType
case class NullDereference()(override val positionInfo: PositionInfo) extends WACCExceptionType
case class General()(override val positionInfo: PositionInfo) extends WACCExceptionType

/* Converts a Type AST into a readable string representation. */
def typeToString(t: Type): String = t match {
  case IntType()          => "int"
  case BoolType()         => "bool"
  case CharType()         => "char"
  case StringType()       => "string"
  case ArrayType(te, _)   => s"${typeToString(te)}[]"
  case PairType(t1, t2)   => s"pair(${typeToString(t1)}, ${typeToString(t2)})"
  case PairErasedType()   => "pair"
  case Arithmetic()       => "ArithmeticException"
  case BadChar()          => "BadCharException"
  case ArrayOutOfBounds() => "ArrayOutOfBoundsException"
  case IntegerOverflow()  => "IntegerOverflowException"
  case NullDereference()  => "NullDereferenceException"
  case General()          => "Exception"
}


