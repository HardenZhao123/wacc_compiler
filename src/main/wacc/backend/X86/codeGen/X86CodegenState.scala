package wacc.backend.X86.codeGen

import wacc.backend.midir.TAC
import wacc.backend.X86.target.X86StackFrame
import wacc.backend.X86.target.*
import wacc.backend.X86.target.X86CallingConvention.scratchRegisters

/** Code generation state for x86-64 backend. Maintains the current stack frame,
 *  provides helper methods to evaluate TAC expressions into registers while
 *  automatically managing temporary register allocation */
final class X86CodegenState(val frame: X86StackFrame) {

  // Register allocator used to obtain temporary registers during code generation
  val ra: X86RegisterAllocator = X86RegisterAllocator(scratchRegisters)

  // Evaluate a TAC right-hand-side expression into a fresh register.
  def withEval[A](rhs: TAC.Rhs)(f: X86Reg => A)(using cs: X86CodegenState, gr: GenRes): A =
    ra.withNewRegister { reg =>
      frame.loadRhs(rhs, reg, ra)
      f(reg)
    }

  // Evaluate two TAC right-hand-side expressions into fresh registers.
  def withEval2[A](lhs: TAC.Rhs, rhs: TAC.Rhs)(f: (X86Reg, X86Reg) => A)
                  (using cs: X86CodegenState, gr: GenRes): A =
    ra.withNewRegisters2 { (reg1, reg2) =>
      frame.loadRhs(lhs, reg1, ra)
      frame.loadRhs(rhs, reg2, ra)
      f(reg1, reg2)
    }
}
