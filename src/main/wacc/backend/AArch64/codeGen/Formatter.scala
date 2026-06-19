package wacc.backend.AArch64.codeGen

import A64FormatterConstant.*
import A64Instr.*
import wacc.backend.BackendCommon.*
import wacc.backend.AArch64.target.A64Reg
import wacc.backend.BackendCommon.AsmEntity.*

// Formats AArch64 instructions and shared asm entities into textual GAS syntax.
object A64Formatter extends CommonFormatter[A64Instr, A64Reg] {

  private val immediateValueBound = A64_UIMM12_MAX

  override def formatInstr(instr: A64Instr): String = instr match {
    case asm: AsmInstr    => formatOperation(asm.opcode, asm.operands*)
    case instr: AsmEntity => formatAsmEntity(instr)
  }

  // Format an immediate value (if -4095 <= x <= 4095, else hex)
  override def formatImmediate(value: Long): String =
    if (value >= -immediateValueBound && value <= immediateValueBound) s"$IMM_PREFIX_DEC$value"
    else s"$IMM_PREFIX_HEX${value.toHexString}"

  override def formatArmOperand(op: Any): String = op match {
    case label: Label             => label.label
    case maskedLabel: MaskedLabel => formatMaskedLabel(maskedLabel)
  }

  override def formatArmAsmInstr(instr: A64Instr): String = instr match {
    case i: AsmInstr => formatOperation(i.opcode, i.operands)
    case _           => throw new Exception("cannot reach other cases")
  }

  // Formats the relocation suffix used by `adrp` + `add` label materialisation.
  private def formatMaskedLabel(masked: MaskedLabel): String =
    s":lo${masked.lastBitCount}:${masked.label.label}"
}
