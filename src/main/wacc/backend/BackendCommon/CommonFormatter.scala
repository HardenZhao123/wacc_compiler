package wacc.backend.BackendCommon

import java.io.{BufferedOutputStream, OutputStream, OutputStreamWriter}

import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.MemAddress

// Shared formatter logic for lowering backend instructions to textual assembly.
abstract class CommonFormatter[InstrType, RegType <: Register] {

  val indent = ASM_INDENT

  // Serialises a full instruction stream to the provided output stream.
  def writeAssembly(instrs: List[InstrType], outputStream: OutputStream): Unit = {
    val writer = new OutputStreamWriter(new BufferedOutputStream(outputStream))
    try {
      instrs.foreach { instr =>
        writer.write(formatInstr(instr) + ASM_NEWLINE)
      }
    } finally {
      writer.flush()
      writer.close()
    }
  }

  // Formats architecture-independent assembly entities such as labels and data blocks.
  def formatAsmEntity(instr: AsmEntity): String = instr match {
    case Raw(line)  => line
    case Label(lab) => s"$lab:"
    case StringData(label, value, printAlign) =>
      val size = value.length
      val base =
        s"""${indent}$ASM_DIRECTIVE_WORD $size
           |${label.label}:
           |${indent}$ASM_DIRECTIVE_ASCIZ \"$value\"""".stripMargin
      if (printAlign) s"$base\n$ASM_DIRECTIVE_ALIGN_4"
      else base
    case WordData(label, value) =>
      s"${label.label}:\n${indent}$ASM_DIRECTIVE_WORD $value"
    case DataSeg(items) =>
      s"$ASM_DIRECTIVE_DATA\n" + items.map(formatAsmEntity).mkString("\n")
  }

  // Formats one backend instruction into assembly text.
  def formatInstr(instr: InstrType): String

  // Formats a generic operand by dispatching to the appropriate backend helper.
  def formatOperand(op: Any): String = op match {
    case Immediate(value) => formatImmediate(value)
    case str: String        => str
    case cond: Cond         => cond.toCondStr
    case shift: ShiftType   => shift.toShiftStr
    case addr: MemAddress[?] => formatMemAddress(addr)
    case reg: Register      => reg.toString
    case _                  => formatArmOperand(op)
  }

  // Formats an immediate literal using architecture-specific syntax.
  def formatImmediate(value: Long): String

  // Formats backend-specific operands that are not covered by the shared cases.
  def formatArmOperand(op: Any): String

  // Formats architecture-specific instruction variants.
  def formatArmAsmInstr(instr: InstrType): String

  // Builds a formatted opcode line with the shared indentation and operand separator.
  def formatOperation(opecode: String, operands: Any*): String = {
    val formattedOperands = operands.map(formatOperand).filter(_.nonEmpty).mkString(ASM_OPERAND_SEPARATOR)
    if (formattedOperands.isEmpty) s"${indent}${opecode}"
    else s"${indent}${opecode} ${formattedOperands}"
  }

  // Formats the shared memory-address shapes used across both backends.
  private def formatMemAddress[T <: Register](addr: MemAddress[T]): String = addr match {
    case MemAddress.BaseRegAddress(base) => s"[${formatOperand(base)}]"

    case MemAddress.RegOffsetAddress(base, offset) => s"[${formatOperand(base)}, ${formatOperand(offset)}]"

    case MemAddress.ShiftedRegister(r1, r2, shiftType, shiftAmount) =>
      s"[${formatOperand(r1)}, ${formatOperand(r2)}, ${shiftType.toShiftStr}, $shiftAmount]"

    case MemAddress.LabelAddress(label) => label.label

    case MemAddress.ImmOffsetAddress(base, offset, mode) =>
      val offsetText = offset match {
        case i: Int => s"#$i"
        case reg: Register => formatOperand(reg)
      }
      mode match {
        case PreIndex => s"[${formatOperand(base)}, $offsetText]!"
        case PostIndex => s"[${formatOperand(base)}], $offsetText"
        case NoIndex => s"[${formatOperand(base)}, $offsetText]"
      }
  }
}
