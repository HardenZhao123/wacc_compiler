package wacc.backend.Arm32.codeGen

import wacc.backend.Arm32.target.*
import wacc.backend.Arm32.target.A32Operand
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*

// Represents memory addresses
type A32MemAddress = MemAddress[A32Reg]

// Base trait for all ARM32 instructions and assembler entities
trait A32Instr
object A32Instr {

  // Base trait for real ARM32 assembly instructions
  sealed trait AsmInstr extends A32Instr {
    def opcode: String
    def operands: Seq[Any] = Seq()
  }

  // ARM32 data-processing instructions
  sealed trait DataProcessingInstr extends AsmInstr {
    val dest: A32Reg
    val op1: A32Reg
    val op2: A32Operand
    override def operands = Seq(dest, op1, op2)
  }

  // Arithmetic instructions
  case class Add(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "add"
  }
  case class Adds(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "adds"
  }
  case class Sub(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "sub"
  }
  case class Subs(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "subs"
  }
  case class Mul(dest: A32Reg, op1: A32Reg, op2: A32Reg) extends DataProcessingInstr {
    val opcode = "mul"
  }
  case class SDiv(dest: A32Reg, op1: A32Reg, op2: A32Reg) extends DataProcessingInstr {
    val opcode = "sdiv"
  }

  // Boolean instructions
  case class And(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "and"
  }
  case class Orr(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "orr"
  }

  // Shift instructions
  case class Asr(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "asr"
  }
  case class Lsl(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "lsl"
  }
  case class Lsr(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "lsr"
  }

  // Bit clear: dest = op1 & ~op2
  case class Bic(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "bic"
  }
  // Reverse subtract and update flags: dest = op2 - op1
  case class Rsbs(dest: A32Reg, op1: A32Reg, op2: A32Operand) extends DataProcessingInstr {
    val opcode = "rsbs"
  }

  // Signed 64-bit multiply. Result is split into high and low registers.
  case class SMull(destHigh: A32Reg, destLow: A32Reg, op1: A32Reg, op2: A32Reg) extends AsmInstr {
    val opcode = "smull"
    override def operands = Seq(destHigh, destLow, op1, op2)
  }

  // Move instructions
  sealed trait MoveInstr extends AsmInstr {
    val dest: A32Reg
    val src: A32Operand
    override def operands = Seq(dest, src)
  }
  case class Mov(dest: A32Reg, src: A32Operand) extends MoveInstr {
    val opcode = "mov"
  }
  case class MovCond(dest: A32Reg, src: A32Operand, cond: Cond) extends MoveInstr {
    val opcode = s"mov${cond.toCondStr}"
  }

  // Branch instructions
  sealed trait BranchInstr extends AsmInstr {
    val label: Label
    override def operands = Seq(label)
  }
  case class B(label: Label) extends BranchInstr {
    val opcode = "b"
  }
  case class Bl(label: Label) extends BranchInstr {
    val opcode = "bl"
  }
  case class BCond(cond: Cond, label: Label) extends BranchInstr {
    val opcode = s"b${cond.toCondStr}"
  }
  case class BlCond(cond: Cond, label: Label) extends BranchInstr {
    val opcode = s"bl${cond.toCondStr}"
  }

  // Push and pop instructions
  sealed trait PushPopInstr extends AsmInstr {
    val registers: List[A32Reg]
    override def operands = Seq(registers)
  }
  case class Push(registers: List[A32Reg]) extends PushPopInstr {
    val opcode = "push"
  }
  case class Pop(registers: List[A32Reg]) extends PushPopInstr {
    val opcode = "pop"
  }

  // Load/store instructions
  sealed trait LoadStoreInstr extends AsmInstr {
    val reg: A32Reg
    val address: A32MemAddress
    override def operands = Seq(reg, address)
  }
  case class Ldr(reg: A32Reg, address: A32MemAddress) extends LoadStoreInstr {
    val opcode = "ldr"
  }
  case class Str(reg: A32Reg, address: A32MemAddress) extends LoadStoreInstr {
    val opcode = "str"
  }
  case class Ldrb(reg: A32Reg, address: A32MemAddress) extends LoadStoreInstr {
    val opcode = "ldrb"
  }
  case class Strb(reg: A32Reg, address: A32MemAddress) extends LoadStoreInstr {
    val opcode = "strb"
  }
  case class LdrFuncPtr(reg: A32Reg, address: A32MemAddress) extends LoadStoreInstr {
    val opcode = "ldr"
  }

  // Load address of label relative to the program counter
  case class Adr(dest: A32Reg, label: Label) extends AsmInstr {
    val opcode = "adr"
    override def operands = Seq(dest, label)
  }

  // Compare two operands and update condition flags
  case class Cmp(op1: A32Reg, op2: A32Operand) extends AsmInstr {
    val opcode = "cmp"
    override def operands = Seq(op1, op2)
  }

  // Bitwise NOT: dest = ~src
  case class Mvn(reg1: A32Reg, reg2: A32Reg) extends AsmInstr {
    val opcode = "mvn"
    override def operands = Seq(reg1, reg2)
  }
}
