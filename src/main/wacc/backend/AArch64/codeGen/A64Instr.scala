package wacc.backend.AArch64.codeGen

import wacc.backend.BackendCommon.ShiftType
import wacc.backend.AArch64.target.*
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*

// Represents memory addresses
type A64MemAddress = MemAddress[A64Reg]

// Base type for all AArch64 instructions emitted by the backend.
trait A64Instr
object A64Instr {

  // Textual AArch64 instruction with an opcode and ordered operands.
  sealed trait AsmInstr extends A64Instr {
    def opcode: String
    def operands: Seq[Any] = Seq()
  }

  // Arithmetic instructions
  sealed trait A64ArithmeticInstr extends AsmInstr {
    val dest: A64Reg
    val op1: A64Reg
    val op2: A64Operand
    override def operands = Seq(dest, op1, op2)
  }
  case class Add(dest: A64Reg, op1: A64Reg, op2: A64Operand) extends A64ArithmeticInstr {
    val opcode = "add"
  }
  case class Adds(dest: A64Reg, op1: A64Reg, op2: A64Operand) extends A64ArithmeticInstr {
    val opcode = "adds"
  }
  case class Sub(dest: A64Reg, op1: A64Reg, op2: A64Operand) extends A64ArithmeticInstr {
    val opcode = "sub"
  }
  case class Subs(dest: A64Reg, op1: A64Reg, op2: A64Operand) extends A64ArithmeticInstr {
    val opcode = "subs"
  }
  case class Mul(dest: A64Reg, op1: A64Reg, op2: A64Reg) extends A64ArithmeticInstr {
    val opcode = "mul"
  }
  case class SMull(dest: A64Reg, op1: A64Reg, op2: A64Reg) extends A64ArithmeticInstr {
    val opcode = "smull"
  }
  case class SDiv(dest: A64Reg, op1: A64Reg, op2: A64Reg) extends A64ArithmeticInstr {
    val opcode = "sdiv"
  }

  sealed trait FloatArithmeticInstr extends AsmInstr {
    val dest: S
    val op1: S
    val op2: S
    override def operands = Seq(dest, op1, op2)
  }
  case class FAdd(dest: S, op1: S, op2: S) extends FloatArithmeticInstr {
    val opcode = "fadd"
  }
  case class FSub(dest: S, op1: S, op2: S) extends FloatArithmeticInstr {
    val opcode = "fsub"
  }
  case class FMul(dest: S, op1: S, op2: S) extends FloatArithmeticInstr {
    val opcode = "fmul"
  }
  case class FDiv(dest: S, op1: S, op2: S) extends FloatArithmeticInstr {
    val opcode = "fdiv"
  }

  case class MSub(dest: A64Reg, op1: A64Reg, op2: A64Reg, op3: A64Reg) extends AsmInstr {
    val opcode = "msub"
    override def operands = Seq(dest, op1, op2, op3)
  }

  // Logical instructions
  sealed trait A64LogicalInstr extends AsmInstr {
    val dest: A64Reg
    val op1: A64Reg
    val op2: A64Operand
    override def operands = Seq(dest, op1, op2)
  }
  case class And(dest: A64Reg, op1: A64Reg, op2: A64Operand) extends A64LogicalInstr {
    val opcode = "and"
  }
  case class Orr(dest: A64Reg, op1: A64Reg, op2: A64Operand) extends A64LogicalInstr {
    val opcode = "orr"
  }

  // Unary instructions
  sealed trait UnaryInstr extends AsmInstr {
    val dest: A64Reg
    val src: A64Operand
    override def operands = Seq(dest, src)
  }
  case class Neg(dest: A64Reg, src: A64Operand) extends UnaryInstr {
    val opcode = "neg"
  }
  case class Negs(dest: A64Reg, src: A64Operand) extends UnaryInstr {
    val opcode = "negs"
  }

  // Move instructions
  case class Mov(dest: A64Reg, src: A64Operand) extends AsmInstr {
    val opcode = "mov"
    override def operands: Seq[Any] = (dest, src) match {
      // Illegal in GAS: mov xN, wM  => rewrite to mov wN, wM (zero-extend into xN)
      case (dx: X, sw: W) => Seq(Reg.toW(dx), sw)
      // Also avoid mov wN, xM (should not happen, but safe): rewrite to mov wN, wM (truncate)
      case (dw: W, sx: X) => Seq(dw, Reg.toW(sx))
      case _ => Seq(dest, src)
    }
  }
  case class Movk(dest: A64Reg, src: A64Operand, shift: ShiftType) extends AsmInstr {
    val opcode = "movk"
    override def operands = Seq(dest, src, shift)
  }
  case class FMov(dest: Register, src: Register) extends AsmInstr {
    val opcode = "fmov"
    override def operands = Seq(dest, src)
  }
  case class FCvt(dest: D, src: S) extends AsmInstr {
    val opcode = "fcvt"
    override def operands = Seq(dest, src)
  }
  case class SCvtf(dest: S, src: A64Reg) extends AsmInstr {
    val opcode = "scvtf"
    override def operands = Seq(dest, src)
  }

  // Branch instructions
  sealed trait BranchInstr extends AsmInstr {
    val label: Label
    override def operands = Seq(label)
  }
  // normal unconditional branch
  case class B(label: Label) extends BranchInstr {
    val opcode = "b"
  }
  // branch with link, use for branching with saving return address
  case class Bl(label: Label) extends BranchInstr {
    val opcode = "bl"
  }
  case class BCond(cond: Cond, label: Label) extends BranchInstr {
    val opcode = s"b.${cond.toCondStr}"
  }

  case class Cbz(reg: A64Reg, label: Label) extends AsmInstr {
    val opcode = "cbz"
    override def operands = Seq(reg, label)
  }

  case class Cbnz(reg: A64Reg, label: Label) extends AsmInstr {
    val opcode = "cbnz"
    override def operands = Seq(reg, label)
  }

  // Load and store instructions
  sealed trait LoadStoreInstr extends AsmInstr {
    val reg: A64Reg
    val address: A64MemAddress
    override def operands = Seq(reg, address)
  }
  case class Ldr(reg: A64Reg, address: A64MemAddress) extends LoadStoreInstr {
    val opcode = "ldr"
  }
  case class Str(reg: A64Reg, address: A64MemAddress) extends LoadStoreInstr {
    val opcode = "str"
  }

  // Unscaled load/store (signed 9-bit offset, allows negative)
  case class Ldur(reg: A64Reg, address: A64MemAddress) extends LoadStoreInstr {
    val opcode = "ldur"
  }
  case class Stur(reg: A64Reg, address: A64MemAddress) extends LoadStoreInstr {
    val opcode = "stur"
  }

  // byte load/store (for char[]/bool[])
  case class Ldrb(reg: A64Reg, address: A64MemAddress) extends LoadStoreInstr {
    val opcode = "ldrb"
  }
  case class Strb(reg: A64Reg, address: A64MemAddress) extends LoadStoreInstr {
    val opcode = "strb"
  }

  // Instruction for address calculation
  sealed trait AddressInstr extends AsmInstr {
    val dest: A64Reg
    val label: Label
    override def operands = Seq(dest, label)
  }
  case class Adrp(dest: A64Reg, label: Label) extends AddressInstr {
    val opcode = "adrp"
  }
  case class Adr(dest: A64Reg, label: Label) extends AddressInstr {
    val opcode = "adr"
  }

  case class Cset(dest: A64Reg, cond: Cond) extends AsmInstr {
    val opcode = "cset"
    override def operands = Seq(dest, cond)
  }

  // Comparison instructions
  case class Cmp(lhs: A64Reg, rhs: A64Operand, isMull: Boolean = false) extends AsmInstr {
    val opcode = "cmp"
    override def operands: Seq[Any] =
      if (isMull) Seq(lhs, rhs, "sxtw")
      else Seq(lhs, rhs)
  }
  case class FCmp(lhs: S, rhs: S) extends AsmInstr {
    val opcode = "fcmp"
    override def operands = Seq(lhs, rhs)
  }

  // Pair load/store & return (needed for stack frames)
  sealed trait PairLoadStoreInstr extends AsmInstr {
    val reg1: A64Reg
    val reg2: A64Reg
    val address: A64MemAddress
    override def operands = Seq(reg1, reg2, address)
  }
  case class Stp(reg1: A64Reg, reg2: A64Reg, address: A64MemAddress) extends PairLoadStoreInstr {
    val opcode = "stp"
  }
  case class Ldp(reg1: A64Reg, reg2: A64Reg, address: A64MemAddress) extends PairLoadStoreInstr {
    val opcode = "ldp"
  }

  case class Mvn(reg1: A64Reg, reg2: A64Reg) extends AsmInstr {
    val opcode = "mvn"
    override def operands = Seq(reg1, reg2)
  }

  // Return
  case object Ret extends AsmInstr {
    val opcode = "ret"
  }

  // Represents the low bits of a label's address for Adrp + Add
  case class MaskedLabel(lastBitCount: Int, label: Label) extends A64Operand
}
