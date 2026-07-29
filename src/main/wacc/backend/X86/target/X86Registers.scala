package wacc.backend.X86.target

import wacc.backend.BackendCommon.{Operand, Register, RegisterAllocator}

// Base trait for all operands in x86-64 instructions
trait X86Operand extends Operand
// Represents x86-64 registers
sealed trait X86Reg extends X86Operand with Register {
  def qword: String
  def dword: String
  def byte: String

  override def name: String = qword
}

case class Reg8(reg: X86Reg) extends X86Operand
case class Reg32(reg: X86Reg) extends X86Operand
case class Reg64(reg: X86Reg) extends X86Operand

final case class XMM(n: Int) extends X86Operand with Register {
  require(0 <= n && n <= 15, s"invalid x86-64 xmm register: xmm$n")

  val name: String = s"xmm$n"
}

final case class R(n: Int) extends X86Reg {
  require(8 <= n && n <= 15, s"invalid x86-64 register: r$n")

  val qword = s"r$n"
  val dword = s"r${n}d"
  val byte = s"r${n}b"
}

// special registers in x86-64
case object RAX extends X86Reg {
  val qword = "rax"
  val dword = "eax"
  val byte = "al"
}
case object RCX extends X86Reg {
  val qword = "rcx"
  val dword = "ecx"
  val byte = "cl"
}
case object RDX extends X86Reg {
  val qword = "rdx"
  val dword = "edx"
  val byte = "dl"
}
case object RBX extends X86Reg {
  val qword = "rbx"
  val dword = "ebx"
  val byte = "bl"
}
case object RSI extends X86Reg {
  val qword = "rsi"
  val dword = "esi"
  val byte = "sil"
}
case object RDI extends X86Reg {
  val qword = "rdi"
  val dword = "edi"
  val byte = "dil"
}
case object RSP extends X86Reg {
  val qword = "rsp"
  val dword = "esp"
  val byte = "spl"
}
case object RBP extends X86Reg {
  val qword = "rbp"
  val dword = "ebp"
  val byte = "bpl"
}
case object RIP extends X86Reg {
  val qword = "rip"
  def dword: String = throw new UnsupportedOperationException("rip has no dword operand form")
  def byte: String = throw new UnsupportedOperationException("rip has no byte operand form")
}

sealed trait X86Registers
object X86Registers {
  val RAX = wacc.backend.X86.target.RAX
  val RCX = wacc.backend.X86.target.RCX
  val RDX = wacc.backend.X86.target.RDX
  val RBX = wacc.backend.X86.target.RBX
  val RSI = wacc.backend.X86.target.RSI
  val RDI = wacc.backend.X86.target.RDI
  val RSP = wacc.backend.X86.target.RSP
  val RBP = wacc.backend.X86.target.RBP

  val XMM0 = XMM(0)
  val XMM1 = XMM(1)

  val R8 = R(8)
  val R9 = R(9)
  val R10 = R(10)
  val R11 = R(11)
  val R12 = R(12)
  val R13 = R(13)
  val R14 = R(14)
  val R15 = R(15)
}

/** Register allocator for X86-64 */
class X86RegisterAllocator(scratchRegisters: List[X86Reg]) extends RegisterAllocator[X86Reg](scratchRegisters) {

  // Allocate a register, use it in a block, then automatically free it
  def withNewRegister[A](f: X86Reg => A): A = withRegister(f)

  // Allocate two registers at once
  def withNewRegisters2[A](f: (X86Reg, X86Reg) => A): A = withRegisters2(f)

  // Allocate six registers at once
  def withNewRegisters6[A](f: (X86Reg, X86Reg, X86Reg, X86Reg, X86Reg, X86Reg) => A): A =
    withRegisters6(f)
}
