package wacc.backend.X86.codeGen

import wacc.backend.X86.preDefFunctions.{PreDefHelpers, PreDefRunTimeError}
import wacc.backend.BackendCommon.*

/** Code generation result container for x86-64. */
final class GenRes extends CommonGenRes[X86Instr] {
  // Emits all referenced runtime errors and predefined helpers after the main body.
  override def preDefsErrs = PreDefRunTimeError.emit(usedErrors) ++ PreDefHelpers.emit(preDefs)
}
