package wacc.backend.BackendCommon

import wacc.backend.AArch64.target.A64Operand
import wacc.backend.Arm32.target.A32Operand
import wacc.backend.X86.target.X86Operand

// Immediate operand wrapper shared by all instruction sets.
case class Immediate(value: Long) extends A64Operand, A32Operand, X86Operand
