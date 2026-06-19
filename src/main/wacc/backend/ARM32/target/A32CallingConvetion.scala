package wacc.backend.Arm32.target

import wacc.backend.Arm32.target.A32Registers.*

object A32CallingConvetion {

  // registers that can store the temporary value of TAC
  val scratchRegisters: List[A32Reg] =
    List(R4, R5, R6, R7, R8, R9, R10)
}
