package wacc.backend.AArch64.target

import Reg.*

object A64CallingConvention {

  // argument registers
  val argRegisters: List[A64Reg] = List(X0, X1, X2, X3, X4, X5, X6, X7)

  // caller-saved registers
  val callerSavedRegisters: List[A64Reg] =
    List(
      X0, X1, X2, X3, X4, X5, X6, X7,
      X9, X10, X11, X12, X13, X14, X15
    )

  // callee-saved registers
  val calleeSavedRegisters: List[A64Reg] =
    List(
      X19, X20, X21, X22, X23, X24,
      X25, X26, X27, X28
    )

  // registers that can store the temporary value of TAC
  val scratchRegisters: List[A64Reg] =
    List(X9, X10, X11, X12, X13, X14, X15, X16, X17)

  // special registers
  val framePointer = FP
  val linkRegister = LR
  val stackPointer = SP
}
