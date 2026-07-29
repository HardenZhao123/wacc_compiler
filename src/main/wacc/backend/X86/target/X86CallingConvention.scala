package wacc.backend.X86.target

import wacc.backend.X86.target.X86Registers.*

object X86CallingConvention {

  // SysV x86-64 integer/pointer argument registers.
  val argRegisters: List[X86Reg] =
    List(RDI, RSI, RDX, RCX, R8, R9)

  // Caller-saved scratch registers used for TAC temporaries.
  val scratchRegisters: List[X86Reg] =
    List(R10, R11, R8, R9, RCX, RDX)
}
