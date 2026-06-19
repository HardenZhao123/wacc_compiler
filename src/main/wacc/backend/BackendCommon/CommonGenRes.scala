package wacc.backend.BackendCommon

import scala.collection.mutable

trait CommonGenRes[InstrType] {

  // Builder for the generated instruction stream.
  private val _instrs: mutable.Builder[InstrType, List[InstrType]] = List.newBuilder

  // Builder for a set of predefined helpers
  private val _preDefs: mutable.Builder[String, Set[String]] = Set.newBuilder

  // Builder for a set of runtime errors that are actually used
  private val _usedErrs: mutable.Builder[String, Set[String]] = Set.newBuilder

  // Emit/add a single instruction to the list
  def emit(instr: InstrType) = _instrs.addOne(instr)

  // Records that a predefined helper routine must be emitted.
  def addPreDefs(preDef: String) = _preDefs.addOne(preDef)

  // Records that a runtime error helper must be emitted.
  def addUsedErrs(usedErr: String) = _usedErrs.addOne(usedErr)

  // Returns all generated instructions in emission order.
  def instrs: List[InstrType] = _instrs.result()

  // Returns the set of predefined helpers requested so far.
  def preDefs = _preDefs.result()

  // Returns the set of runtime error helpers requested so far.
  def usedErrors = _usedErrs.result()

  // Collects all added predefined helpers and runtime errors
  def preDefsErrs: Seq[InstrType]
}
