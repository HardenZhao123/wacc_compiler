package wacc.backend.AArch64.target

object StackFrameConstant {

  // AArch64 ABI require SP 16-byte alignment
  val STACK_ALIGNMENT_BYTES = 16

  // Every slot(Temp / spill) in stack assign 8 bytes space
  val STACK_SLOT_BYTES = 8

  // In prologue of functions (stp fp, lr, [sp, #-16]!): storing FP/LR,takes 16 bytes
  val FRAME_HEADER_BYTES = 16

  // LDUR/STUR use simm9 offset (in range -256 to 255)
  val SIMM9_MIN_BYTES = -256
  val SIMM9_MAX_BYTES = 255

  // AArch64 calling convention: first 8 arguments go into x0..x7
  val ARG_REG_COUNT = 8

  // Spill area reserve for temp value
  val SPILL_TMP_AREA_SLOTS = 8

  // push FP/LR take 16 bytes each time
  val FP_LR_SAVE_BYTES = 16

  // movk shift only allow 0/16/32/48
  val MOVK_SHIFT_HALFWORD = 16

  // char/bool=1; int=4; ptr/pair/string=8.
  val SIZE_CHAR_BOOL_BYTES = 1
  val SIZE_INT_BYTES = 4
  val SIZE_PTR_BYTES = 8

  val NULL_PTR = 0L
}
