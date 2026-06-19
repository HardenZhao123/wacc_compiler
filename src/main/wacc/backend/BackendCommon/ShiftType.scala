package wacc.backend.BackendCommon

import wacc.backend.BackendCommon.Immediate

// Shift operations that can decorate register operands in generated assembly.
enum ShiftType {
  case ASR(imm: Immediate)
  case ASL(imm: Immediate)
  case LSL(imm: Immediate)
  case LSR(imm: Immediate)
  case ROR(imm: Immediate)

  // Converts the shift node to its assembly mnemonic form.
  def toShiftStr: String = this match {
    case ASR(imm)   => s"asr #${imm.value}"
    case ASL(imm)   => s"asl #${imm.value}"
    case LSL(imm)   => s"lsl #${imm.value}"
    case LSR(imm)   => s"lsr #${imm.value}"
    case ROR(imm)   => s"ror #${imm.value}"
  }
}
