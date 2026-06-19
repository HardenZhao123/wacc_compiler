package wacc.backend.BackendCommon

import wacc.backend.AArch64.target.A64Operand
import wacc.backend.Arm32.target.A32Operand

// Immediate operand wrapper shared by both instruction sets.
case class Immediate(value: Long) extends A64Operand, A32Operand
