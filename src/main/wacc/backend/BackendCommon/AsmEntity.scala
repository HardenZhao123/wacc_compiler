package wacc.backend.BackendCommon

import wacc.backend.Arm32.codeGen.A32Instr
import wacc.backend.AArch64.codeGen.A64Instr

// Common assembly-level entities shared by both ARM32 and AArch64 backends.
trait AsmEntity extends A32Instr, A64Instr
object AsmEntity {
  // Emits a line verbatim without any extra formatting.
  case class Raw(line: String) extends AsmEntity

  // Represents a label in assembly.
  case class Label(label: String) extends AsmEntity

  // Base type for items placed in the data section.
  sealed trait DataItem extends AsmEntity {
    val label: Label
    val size: Int
  }

  // Null-terminated string literal together with its emitted byte size.
  case class StringData(label: Label, value: String, printAlign: Boolean = false) extends DataItem {
    override val size: Int = value.length
  }

  // Single machine word stored in the data section.
  case class WordData(label: Label, value: Int) extends DataItem {
    override val size: Int = 4
  }

  // Group of data items emitted under a `.data` segment.
  case class DataSeg(dataItems: Seq[DataItem]) extends AsmEntity
}
