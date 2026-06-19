package wacc.backend.Arm32.target

import wacc.backend.BackendCommon.{Register, RegisterAllocator, Operand, ShiftType}

// Base trait for all operands in Arm32 instructions
trait A32Operand extends Operand
// Trait for Arm32 registers
sealed trait A32Reg extends A32Operand with Register
// Arm32 registers with a shifted operation
final case class A32ShiftedReg(reg: A32Reg, shift: ShiftType) extends A32Operand

final case class R(n: Int) extends A32Reg {
  val name: String = s"r$n"
}

// special registers in Arm32
case object FP extends A32Reg { val name = "fp" }
case object IP extends A32Reg { val name = "ip" }
case object SP extends A32Reg { val name = "sp" }
case object LR extends A32Reg { val name = "lr" }
case object PC extends A32Reg { val name = "pc" }

sealed trait A32Registers
object A32Registers {
  val R0 = R(0)
  val R1 = R(1)
  val R2 = R(2)
  val R3 = R(3)
  val R4 = R(4)
  val R5 = R(5)
  val R6 = R(6)
  val R7 = R(7)
  val R8 = R(8)
  val R9 = R(9)
  val R10 = R(10)
}

/** Register allocator for Arm32 */
class A32RegisterAllocator(scratchRegisters: List[A32Reg]) extends RegisterAllocator[A32Reg](scratchRegisters) {

  // Allocate a register, use it in a block, then automatically free it
  def withNewRegister[A](f: A32Reg => A): A = withRegister(f)

  // Allocate two registers at once
  def withNewRegisters2[A](f: (A32Reg, A32Reg) => A): A = withRegisters2(f)

  // Allocate six registers at once
  def withNewRegisters6[A](f: (A32Reg, A32Reg, A32Reg, A32Reg, A32Reg, A32Reg) => A): A =
    withRegisters6(f)
}
