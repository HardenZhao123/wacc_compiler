package wacc.backend.BackendCommon

// Addressing mode used for indexed memory operands.
sealed trait IndexMode
case object PreIndex extends IndexMode  // [Reg, offset]!
case object PostIndex extends IndexMode // [Reg], offset
case object NoIndex extends IndexMode   // [Reg, #imm]
