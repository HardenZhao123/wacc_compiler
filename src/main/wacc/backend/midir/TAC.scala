package wacc.backend.midir

import wacc.backend.label.Label
import wacc.frontend.typedAST.SemanticType

/* Mid-level IR (TAC-style) used as codegen input. */
object TAC {

  // TACProgram separates string literals, lowered functions, and the lowered main body.
  final case class TACProgram(strs: Seq[String], funcs: Seq[TACFunction], body: Seq[Instr], locals: Seq[Temp])

  final case class TACFunction(name: Label, params: List[Temp], locals: List[Temp], body: List[Instr])

  // Instructions
  sealed trait Instr

  // exit <rhs>
  final case class TACExit(code: Rhs) extends Instr

  sealed trait Print extends Instr { def content: Rhs }
  final case class PrintInt(content: Rhs) extends Print
  final case class PrintChar(content: Rhs) extends Print
  final case class PrintBool(content: Rhs) extends Print
  final case class PrintFloat(content: Rhs) extends Print
  final case class PrintStr(content: Rhs) extends Print
  final case class PrintPointer(content: Rhs) extends Print
  final case class PrintLn() extends Instr

  // dst = lhs (op) rhs
  final case class BinOp(dst: Temp, op: BinaryOp, lhs: Rhs, rhs: Rhs) extends Instr
  final case class CheckedBinOp(dst: Temp, op: ArithOp, lhs: Rhs, rhs: Rhs, onOverflow: Label) extends Instr

  // dst = (op) x
  final case class UnOp(dst: Temp, op: UnaryOp, x: Rhs) extends Instr
  final case class CheckedUnOp(dst: Temp, op: UnaryOp, x: Rhs, onOverflow: Label) extends Instr

  // Exception state operations
  final case class TACStoreException(value: Rhs) extends Instr
  final case class TACLoadExceptionFlag(dst: Temp) extends Instr
  final case class TACLoadExceptionValue(dst: Temp) extends Instr
  final case class TACClearException() extends Instr
  final case class TACReportError(value: Rhs) extends Instr

  // Values
  sealed trait Rhs { def len: BitLength }
  final case class ImmValue(value: Long, len: BitLength = BitLength._32) extends Rhs
  final case class FloatValue(value: Float, len: BitLength = BitLength._32) extends Rhs {
    def rawBits: Int =
      java.lang.Float.floatToRawIntBits(value)

    def rawBitsAsLong: Long =
      rawBits & 0xffffffffL
  }
  final case class TACStr(id: Int, len: BitLength = BitLength._ptr) extends Rhs
  final case class Pair(fst: Rhs, snd: Rhs, len: BitLength = BitLength._ptr) extends Rhs
  final case class Array(elems: List[Rhs], len: BitLength) extends Rhs

  // `Val` is the subset of RHS forms that can appear on the left-hand side of an assignment.
  sealed trait Val extends Rhs
  final case class Temp(id: Int, len: BitLength) extends Val
  final case class IndirectTemp(base: Temp,
                                offset: Temp | ImmValue,
                                isArray: Boolean,
                                len: BitLength) extends Val {
    // Reuse the same address calculation while projecting a different payload width.
    def withSize(size: BitLength): IndirectTemp = IndirectTemp(base, offset, isArray, size)
  }

  enum BitLength {
    case _64, _32, _8, _ptr

    def convertToBytes(size: Int): Int = this match {
      case BitLength._64 => 8
      case BitLength._32 => 4
      case BitLength._8 => 1
      case BitLength._ptr => size
    }
  }

  def sizeof(semType: SemanticType): BitLength = semType match {
    // Aggregate values lower to pointers because the runtime layout lives on the heap.
    case SemanticType.SemInt => BitLength._32
    case SemanticType.SemFloat => BitLength._32
    case SemanticType.SemBool => BitLength._8
    case SemanticType.SemChar => BitLength._8
    case SemanticType.SemString => BitLength._ptr
    case SemanticType.SemPair(_, _) => BitLength._ptr
    case SemanticType.SemArray(_, _) => BitLength._ptr
    case SemanticType.SemPairErased => BitLength._ptr
    case SemanticType.SemUnknown => BitLength._ptr
    case SemanticType.SemException => throw new Exception("Exception has no bit length")
  }

  sealed trait BinaryOp

  enum ArithOp extends BinaryOp {
    case Add, Sub, Mul, Div, Mod
  }

  enum FloatArithOp extends BinaryOp {
    case Add, Sub, Mul, Div
  }

  enum CondOp extends BinaryOp {
    case EQ, NEQ, LT, GT, LEQ, GEQ
  }

  enum FloatCondOp extends BinaryOp {
    case EQ, NEQ, LT, GT, LEQ, GEQ
  }

  enum BoolOp extends BinaryOp {
    case And, Or
  }

  enum BitwiseOp extends BinaryOp {
    case BitAnd, BitOr
  }

  enum UnaryOp {
    case Neg, Not, Len, Ord, Chr, BitNot
  }

  // control flow
  final case class Mark(l: Label) extends Instr
  final case class Jmp(to: Label) extends Instr
  final case class CmpJmp(cond: CondOp, lhs: Rhs, rhs: Rhs, to: Label) extends Instr
  final case class TACSkip() extends Instr

  final case class TACAssign(lhs: Val, rhs: Rhs) extends Instr
  final case class TACReturn(value: Rhs) extends Instr

  enum ReadType {
    case Int, Char, Float
  }

  final case class TACCall(dst: Temp, func: Label, args: List[Rhs]) extends Instr
  final case class TACRead(dst: Val, t: ReadType) extends Instr
  final case class TACFree(x: Rhs, isPair: Boolean) extends Instr
}
