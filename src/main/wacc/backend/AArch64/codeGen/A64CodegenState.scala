package wacc.backend.AArch64.codeGen

import wacc.backend.midir.TAC
import wacc.backend.AArch64.target.A64CallingConvention.scratchRegisters
import wacc.backend.AArch64.target.Reg.*
import wacc.backend.AArch64.target.*


// Represents the state of the AArch64 code generator
final class A64CodegenState(val frame: StackFrame) {

  val ra: A64RegisterAllocator = A64RegisterAllocator(scratchRegisters)

  // Chooses the correct register width for evaluating a TAC value.
  private def evalReg(reg: A64Reg, rhs: TAC.Rhs): A64Reg = rhs match {
    case _: TAC.TACStr | _: TAC.Pair | _: TAC.Array => reg
    case _ => rhs.len match {
      case TAC.BitLength._32 | TAC.BitLength._8 => toW(reg)
      case _ => reg
    }
  }

  // Evaluates one TAC operand into a fresh scratch register for the provided continuation.
  def withEval[A](rhs: TAC.Rhs)(f: (A64Reg) => A)(using cs: A64CodegenState, gr: GenRes): A =
    ra.withNewRegister { reg =>
      val eval = evalReg(reg, rhs)
      frame.loadRhs(rhs, eval, ra)
      f(eval)
    }

  // Evaluates two TAC operands into fresh scratch registers for the provided continuation.
  def withEval2[A](lhs: TAC.Rhs, rhs: TAC.Rhs)(f: (A64Reg, A64Reg) => A)(using cs: A64CodegenState, gr: GenRes): A =
    ra.withNewRegisters2 { (reg1, reg2) =>
      val lhsReg = evalReg(reg1, lhs)
      val rhsReg = evalReg(reg2, rhs)
      frame.loadRhs(lhs, lhsReg, ra)
      frame.loadRhs(rhs, rhsReg, ra)
      f(lhsReg, rhsReg)
    }
}
