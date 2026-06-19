package wacc.backend.midir

import scala.collection.mutable
import wacc.backend.midir.TAC.*
import wacc.frontend.typedAST.*
import wacc.backend.label.Label
import wacc.backend.midir.LowerToTacConstant.*
import wacc.frontend.typedAST.SemanticType.*

private final class TempGen {
  private var next: Int = 0

  def fresh(len: BitLength): Temp = {
    val t = Temp(next, len); next += 1; t
  }
}

final class FuncContext(val isMain: Boolean, val returnType: Option[SemanticType]) {

  private val tempGen: TempGen = TempGen()
  private val _locals: mutable.Builder[Temp, List[Temp]] = List.newBuilder
  def locals: List[Temp] = _locals.result()

  // Variable names map to the TAC temp that currently stores their value in this function.
  private val env: mutable.Map[String, Temp] = mutable.Map()
  private val _instrs: mutable.Builder[Instr, List[Instr]] = List.newBuilder
  def instrs: List[Instr] = _instrs.result()

  // Loop and exception stacks let nested constructs lower break/continue/throw without global state.
  private val loopTargets: mutable.Stack[LoopTarget] = mutable.Stack.empty
  private val exceptionHandlers: mutable.Stack[Label] = mutable.Stack.empty

  def pushLoop(target: LoopTarget): Unit = loopTargets.push(target)
  def popLoop(): Unit = if (loopTargets.nonEmpty) loopTargets.pop()
  def currentLoop: Option[LoopTarget] = loopTargets.headOption

  def pushExceptionHandler(label: Label): Unit = exceptionHandlers.push(label)
  def popExceptionHandler(): Unit = if (exceptionHandlers.nonEmpty) exceptionHandlers.pop()
  def currentExceptionHandler: Option[Label] = exceptionHandlers.headOption

  def fresh(len: BitLength): Temp = {
    // Every temp is also recorded as a local so later backend stages know the full frame requirement.
    val t = tempGen.fresh(len)
    _locals.addOne(t)
    t
  }

  def declare(name: String, len: BitLength): Temp = {
    // Declarations allocate a fresh temp and overwrite the source-name binding for subsequent lookups.
    val temp = tempGen.fresh(len)
    _locals.addOne(temp)
    env(name) = temp
    temp
  }

  def lookup(name: String): Temp = env.getOrElse(name, throw new Exception(s"$ERR_UNDEFINED_VAR_PREFIX $name"))

  def emit(instr: Instr): Unit = _instrs.addOne(instr)

  def defaultReturnValue: Rhs = returnType match {
    // Non-void functions that fall off the end still need a backend value of the right machine width.
    case Some(SemInt) => ImmValue(0)
    case Some(SemBool) => ImmValue(0, BitLength._8)
    case Some(SemChar) => ImmValue(0, BitLength._8)
    case Some(_) => ImmValue(0, BitLength._ptr)
    case None => ImmValue(0)
  }
}
