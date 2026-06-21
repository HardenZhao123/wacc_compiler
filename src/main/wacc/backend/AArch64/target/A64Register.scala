package wacc.backend.AArch64.target

import wacc.backend.BackendCommon.{Operand, Register, RegisterAllocator}

// Base trait for all operands in AArch64 instructions
trait A64Operand extends Operand
// Represents AArch64 registers
sealed trait A64Reg extends A64Operand with Register

// 64-bit general-purpose registers
final case class X(n: Int) extends A64Reg {
  val name: String = s"x$n"
}

// 32-bit general-purpose registers
final case class W(n: Int) extends A64Reg {
  val name: String = s"w$n"
}

// Scalar floating-point register views used by float code generation.
final case class S(n: Int) extends A64Operand with Register {
  val name: String = s"s$n"
}

final case class D(n: Int) extends A64Operand with Register {
  val name: String = s"d$n"
}

// Special registers
case object SP extends A64Reg { val name = "sp" }   // stack pointer
case object FP extends A64Reg { val name = "fp" }   // frame pointer
case object LR extends A64Reg { val name = "lr" }   // link register
case object XZR extends A64Reg { val name = "xzr" } // Zero register 64-bit
case object WZR extends A64Reg { val name = "wzr" } // Zero register 32-bit

sealed trait Reg
object Reg {

  // 64-bit registers
  val X0 = X(0)
  val X1 = X(1)
  val X2 = X(2)
  val X3 = X(3)
  val X4 = X(4)
  val X5 = X(5)
  val X6 = X(6)
  val X7 = X(7)
  val X8 = X(8)
  val X9 = X(9)
  val X10 = X(10)
  val X11 = X(11)
  val X12 = X(12)
  val X13 = X(13)
  val X14 = X(14)
  val X15 = X(15)
  val X16 = X(16)
  val X17 = X(17)
  val X18 = X(18)
  val X19 = X(19)
  val X20 = X(20)
  val X21 = X(21)
  val X22 = X(22)
  val X23 = X(23)
  val X24 = X(24)
  val X25 = X(25)
  val X26 = X(26)
  val X27 = X(27)
  val X28 = X(28)
  val X29 = FP
  val X30 = LR

  // 32-bit registers
  val W0 = W(0)
  val W1 = W(1)
  val W2 = W(2)
  val W3 = W(3)
  val W4 = W(4)
  val W5 = W(5)
  val W6 = W(6)
  val W7 = W(7)
  val W8 = W(8)
  val W9 = W(9)
  val W10 = W(10)
  val W11 = W(11)
  val W12 = W(12)
  val W13 = W(13)
  val W14 = W(14)
  val W15 = W(15)
  val W16 = W(16)
  val W17 = W(17)
  val W18 = W(18)
  val W19 = W(19)
  val W20 = W(20)
  val W21 = W(21)
  val W22 = W(22)
  val W23 = W(23)
  val W24 = W(24)
  val W25 = W(25)
  val W26 = W(26)
  val W27 = W(27)
  val W28 = W(28)
  val W29 = W(29)
  val W30 = W(30)

  // Conversions between 32-bit and 64-bit registers
  def toX(w: A64Reg): A64Reg = w match {
    case x: X => x
    case w: W => X(w.n)
    case _ => throw new IllegalArgumentException("Unsupported register type in toW")
  }

  def toW(x: A64Reg): A64Reg = x match {
    case x: X => W(x.n)
    case w: W => w
    case _ => throw new IllegalArgumentException("Unsupported register type in toW")
  }

  // Physical register equality: xN/wN alias, and special registers compare by identity.
  def samePhysicalReg(a: A64Reg, b: A64Reg): Boolean = {
    def canonicalReg(r: A64Reg): String = r match {
      case X(n)    => s"gpr:$n"
      case W(n)    => s"gpr:$n"
      case FP      => "fp"
      case LR      => "lr"
      case SP      => "sp"
      case XZR | WZR => "zr"
    }

    canonicalReg(a) == canonicalReg(b)
  }
}

class A64RegisterAllocator(scratchRegisters: List[A64Reg]) extends RegisterAllocator[A64Reg](scratchRegisters) {

  // Allocate a register, use it in a block, then automatically free it
  def withNewRegister[A](f: A64Reg => A): A = withRegister(f)

  // Allocate two registers at once
  def withNewRegisters2[A](f: (A64Reg, A64Reg) => A): A = withRegisters2(f)

  // Allocate six registers at once
  def withNewRegisters6[A](f: (A64Reg, A64Reg, A64Reg, A64Reg, A64Reg, A64Reg) => A): A =
    withRegisters6(f)
}
