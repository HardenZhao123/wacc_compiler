package wacc.backend.BackendCommon

import wacc.backend.BackendCommon.AsmEntity.Label

// Shared memory-address shapes understood by both backend formatters.
trait MemAddress[RegType <: Register]
object MemAddress {
  // Base-register addressing, e.g. [x0].
  case class BaseRegAddress[RegType <: Register](base: RegType) extends MemAddress[RegType]
  // Register-offset addressing, e.g. [x0, x1].
  case class RegOffsetAddress[RegType <: Register](base: RegType, offset: RegType) extends MemAddress[RegType]
  // Immediate/register offset addressing with explicit pre/post/no indexing behaviour.
  case class ImmOffsetAddress[RegType <: Register](base: RegType, offset: Int | RegType, mode: IndexMode)
    extends MemAddress[RegType]
  // Shifted-register addressing used by indexed loads/stores.
  case class ShiftedRegister[RegType <: Register](reg1: RegType, reg2: RegType, shiftType: ShiftType, shiftAmount: Int)
    extends MemAddress[RegType]
  // Address resolved through a named assembler label.
  case class LabelAddress[RegType <: Register](label: Label) extends MemAddress[RegType]
}
