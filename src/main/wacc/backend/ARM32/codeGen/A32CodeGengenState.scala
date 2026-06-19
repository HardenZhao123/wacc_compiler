package wacc.backend.Arm32.codeGen

import wacc.backend.midir.TAC
import wacc.backend.Arm32.target.A32StackFrame
import wacc.backend.Arm32.target.*
import wacc.backend.Arm32.target.A32CallingConvetion.scratchRegisters

/** Code generation state for ARM32 backend. Maintains the current stack frame,
 *  provides helper methods to evaluate TAC expressions into registers while
 *  automatically managing temporary register allocation */
final class A32CodegenState(val frame: A32StackFrame) {

  // Register allocator used to obtain temporary registers during code generation
  val ra: A32RegisterAllocator = A32RegisterAllocator(scratchRegisters)

  // Evaluate a TAC right-hand-side expression into a fresh register.
  def withEval[A](rhs: TAC.Rhs)(f: A32Reg => A)(using cs: A32CodegenState, gr: GenRes): A =
    ra.withNewRegister { reg =>
      frame.loadRhs(rhs, reg, ra)
      f(reg)
    }

  // Evaluate two TAC right-hand-side expressions into fresh registers.
  def withEval2[A](lhs: TAC.Rhs, rhs: TAC.Rhs)(f: (A32Reg, A32Reg) => A)
                  (using cs: A32CodegenState, gr: GenRes): A =
    ra.withNewRegisters2 { (reg1, reg2) =>
      frame.loadRhs(lhs, reg1, ra)
      frame.loadRhs(rhs, reg2, ra)
      f(reg1, reg2)
    }
}
