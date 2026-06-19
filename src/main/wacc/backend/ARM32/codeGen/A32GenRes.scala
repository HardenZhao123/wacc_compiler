package wacc.backend.Arm32.codeGen

import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.preDefFunctions.{PreDefHelpers, PreDefRunTimeError}
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.label.LabelGen
import wacc.backend.Arm32.Arm32Constants.*

/** Code generation result container for ARM32. */
final class GenRes extends CommonGenRes[A32Instr] {

  // Generates unique labels used when inserting literal pools
  private val labelGen = new LabelGen()
  // Number of instructions emitted since the last literal pool
  private var emittedSincePool = 0
  // Maximum instruction distance before forcing a new literal pool
  private val poolInterval = POOP_INTERVAL

  // Emit an instruction and track instruction distance from the last literal pool
  override def emit(instr: A32Instr) = {
    val emitted = super.emit(instr)

    instr match {
      case _: DataSeg => ()
      case Raw(".ltorg") =>
        emittedSincePool = 0
      case _ =>
        emittedSincePool += 1
        if emittedSincePool >= poolInterval then emitLiteralPool()
    }

    emitted
  }

  // Emit a literal pool using `.ltorg`
  def emitLiteralPool(): Unit = {
    if emittedSincePool > 0 then
      val skip = Label(labelGen.fresh(A32_POOP_SKIP).name)
      super.emit(B(skip))
      super.emit(Raw(LTORG))
      super.emit(skip)
      emittedSincePool = 0
  }

  /** Emit all required predefined helpers and runtime error handlers 
   *  that are referenced during code generation. */
  def preDefsErrs = PreDefRunTimeError.emit(usedErrors) ++ PreDefHelpers.emit(preDefs)
}
