package wacc.backend.AArch64.codeGen

import wacc.backend.BackendCommon.*
import wacc.backend.AArch64.preDefFunctions.{PreDefRunTimeError, PreDefHelpers}


final class GenRes extends CommonGenRes[A64Instr] {
  // Emits all referenced runtime errors and predefined helpers after the main body.
  override def preDefsErrs = PreDefRunTimeError.emit(usedErrors) ++ PreDefHelpers.emit(preDefs)
}
