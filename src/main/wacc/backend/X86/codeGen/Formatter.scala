package wacc.backend.X86.codeGen

import wacc.backend.X86.codeGen.X86Instr.*
import wacc.backend.X86.codeGen.X86Instr.X86Address.*
import wacc.backend.X86.target.X86Reg
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.X86.target.*

/** x86-64 assembly formatter. */
object X86Formatter extends CommonFormatter[X86Instr, X86Reg] {

  // Format an x86-64 instruction or assembler entity
  override def formatInstr(instr: X86Instr): String = instr match {
    case Setcc(cond, dst) => formatOperation(s"set${cond.suffix}", dst.byte)
    case asm: AsmInstr    => formatOperation(asm.opcode, asm.operands*)
    case instr: AsmEntity => formatAsmEntity(instr)
  }

  override def formatAsmEntity(instr: AsmEntity): String = instr match {
    case StringData(label, value, printAlign) =>
      val base =
        s"""${indent}.int ${asmStringLength(value)}
           |${label.label}:
           |${indent}$ASM_DIRECTIVE_ASCIZ \"$value\"""".stripMargin
      if (printAlign) s"$base\n$ASM_DIRECTIVE_ALIGN_4"
      else base

    case WordData(label, value) =>
      s"${label.label}:\n${indent}.int $value"

    case QuadData(label, value) =>
      s"${label.label}:\n${indent}$ASM_DIRECTIVE_QUAD $value"

    case DataSeg(items) =>
      s"$ASM_DIRECTIVE_DATA\n" + items.map(formatAsmEntity).mkString("\n")

    case other =>
      super.formatAsmEntity(other)
  }

  // Format x86-64-specific operands that require special syntax
  override def formatArmOperand(op: Any): String = op match {
    case label: Label => label.label
    case mem: Mem     => formatMem(mem)
    case Reg8(reg)    => reg.byte
    case Reg32(reg)   => reg.dword
    case Reg64(reg)   => reg.qword
    case _            => throw new Exception(s"unsupported x86 operand: $op")
  }

  // Format a standard x86-64 assembly instruction.
  override def formatArmAsmInstr(instr: X86Instr): String = instr match {
    case i: AsmInstr => formatOperation(i.opcode, i.operands*)
    case _           => throw new Exception("expected x86-64 instruction")
  }

  // Intel-syntax immediates are printed without a prefix.
  override def formatImmediate(value: Long): String = value.toString

  // format memory address for x86-64
  private def formatMem(mem: Mem): String = {
    val prefix = mem.size.map(s => s"${s.ptrName} ").getOrElse("")
    s"$prefix[${formatAddress(mem.address)}]"
  }

  private def formatAddress(addr: X86Address): String = addr match {
    case Base(base, 0) => formatOperand(base)
    case Base(base, disp) => s"${formatOperand(base)} ${formatDisp(disp)}"

    case Index(None, index, scale, 0) => s"${formatOperand(index)} * $scale"
    case Index(None, index, scale, disp) => s"${formatOperand(index)} * $scale ${formatDisp(disp)}"
    case Index(Some(base), index, scale, 0) => s"${formatOperand(base)} + ${formatOperand(index)} * $scale"
    case Index(Some(base), index, scale, disp) =>
      s"${formatOperand(base)} + ${formatOperand(index)} * $scale ${formatDisp(disp)}"

    case RipRelative(label, 0) => s"rip + ${label.label}"
    case RipRelative(label, disp) => s"rip + ${label.label} ${formatDisp(disp)}"
  }

  private def formatDisp(disp: Int): String = if (disp < 0) s"- ${-disp}" else s"+ $disp"

  private def asmStringLength(value: String): Int = {
    var i = 0
    var length = 0

    while (i < value.length) {
      if (value.charAt(i) == '\\' && i + 1 < value.length) i += 2
      else i += 1
      length += 1
    }

    length
  }
}
