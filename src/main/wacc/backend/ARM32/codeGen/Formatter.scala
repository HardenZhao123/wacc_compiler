package wacc.backend.Arm32.codeGen

import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.target.A32Reg
import wacc.backend.BackendCommon.*
import wacc.backend.Arm32.target.A32ShiftedReg
import wacc.backend.BackendCommon.AsmEntity.*

/** ARM32 assembly formatter. */
object A32Formatter extends CommonFormatter[A32Instr, A32Reg] {

  // Format an ARM instruction or assembler entity
  override def formatInstr(instr: A32Instr): String = instr match {
    case asm: AsmInstr    => formatOperation(asm.opcode, asm.operands*)
    case instr: AsmEntity => formatAsmEntity(instr)
  }

  // Format ARM-specific operands that require special syntax
  override def formatArmOperand(op: Any): String = op match {
    case A32ShiftedReg(reg, shift) => s"${reg.toString}, ${shift.toShiftStr}"
    case label: Label              => label.label
    case registers: List[?] if registers.forall(_.isInstanceOf[A32Reg]) =>
      s"{${registers.asInstanceOf[List[A32Reg]].map(formatOperand).mkString(", ")}}"
  }

  // Format a standard ARM32 assembly instruction (e.g. add, str ...)
  override def formatArmAsmInstr(instr: A32Instr): String = instr match {
    case i: AsmInstr => formatOperation(i.opcode, i.operands)
    case _           => throw new Exception("cannot reach other cases")
  }

  // ARM32 immediate is prefixed with '#'.
  override def formatImmediate(value: Long): String = s"#$value"
}
