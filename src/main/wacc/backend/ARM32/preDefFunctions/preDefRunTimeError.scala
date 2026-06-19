package wacc.backend.Arm32.preDefFunctions

import wacc.common.ExitCode
import wacc.backend.Arm32.preDefFunctions.preDefHelpersConstant.*
import wacc.backend.Arm32.codeGen.A32Instr
import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.target.*
import wacc.backend.BackendCommon.Immediate
import wacc.backend.Arm32.Arm32Constants.*
import wacc.backend.BackendCommon.AsmEntity.*

object PreDefRunTimeError {

  // Exit code used for all runtime failures
  private val FAIL: Int = ExitCode.RuntimeError

  // Labels that codegen should branch to / call.
  val ErrNullLabel = "_errNull"
  val ErrOverflowLabel = "_errOverflow"
  val ErrOutOfMemoryLabel = "_errOutOfMemory"
  val ErrDivZeroLabel = "_errDivZero"
  val ErrOutOfBoundsLabel = "_errOutOfBounds"
  val ErrBadCharLabel = "_errBadChar"

  // Error definitions table
  private val defs: List[(String, String, Boolean, String)] = List(
    (ErrNullLabel, LBL_ERRNULL_STR0, false, MSG_NULL_DEREF_OR_FREED),
    (ErrOverflowLabel, LBL_ERROVERFLOW_STR0, false, MSG_INT_OVERFLOW_OR_UNDERFLOW),
    (ErrOutOfMemoryLabel, LBL_ERROUTOFMEMORY_STR0, false, MSG_OUT_OF_MEMORY),
    (ErrDivZeroLabel, LBL_ERRDIVZERO_STR0, false, MSG_DIV_OR_MOD_BY_ZERO),
    (ErrOutOfBoundsLabel, LBL_ERROUTOFBOUNDS_STR0, true, MSG_ARRAY_INDEX_OOB_FMT),
    (ErrBadCharLabel, LBL_ERRBADCHAR_STR0, true, MSG_INT_NOT_ASCII_0_127_FMT)
  )

  // Prefix all runtime messages with "fatal error: " and append newline
  private val FatalPrefix = FATAL_PREFIX

  // Wrap the core message with fatal prefix and trailing newline
  private def fatalMsg(sOrFmt: String): String = FatalPrefix + sOrFmt + FATAL_SUFFIX_NEWLINE_ESC

  /**
   * Emit the data section entry for the error message and the label that
   * code generation branches to when this runtime error occurs.
   */
  private def emitTextHeader(labelName: String, dataLabel: String, msg: String): List[A32Instr] =
    List(
      StringData(Label(dataLabel), msg, true),
      Label(labelName)
    )

  // Helper function to construct runtime error pre-defined functions for non BadChar and ArrayOutOfBounds
  private def runTimeErrNonBadCharOutBounds(labelName: String, dataLabel: String, msg: String): List[A32Instr] =
    emitTextHeader(labelName, dataLabel, msg) ++
      List(
        Bic(SP, SP, Immediate(STACK_ALIGN_MASK)),
        Adr(RETURN_REG, Label(dataLabel)),
        Bl(Label(_PRINTS)),
        Mov(RETURN_REG, Immediate(FAIL)),
        Bl(Label(LIBC_EXIT))
      )

  // Helper function to construct runtime error pre-defined functions for BadChar and ArrayOutOfBounds
  private def runtTimeErrBadCharOutOfBounds(labelName: String, dataLabel: String, msg: String): List[A32Instr] =
    emitTextHeader(labelName, dataLabel, msg) ++
      List(
        Bic(SP, SP, Immediate(STACK_ALIGN_MASK)),
        Adr(RETURN_REG, Label(dataLabel)),
        Bl(Label(LIBC_PRINTF)),
        Mov(RETURN_REG, Immediate(MOVE_ZERO)),
        Bl(Label(LIBC_FFLUSH)),
        Mov(RETURN_REG, Immediate(FAIL)),
        Bl(Label(LIBC_EXIT))
      )

  private def gen(d: (String, String, Boolean, String)): List[A32Instr] =
    val (lbl, dataLbl, badCharOutBounds, core) = d
    if (badCharOutBounds) runtTimeErrBadCharOutOfBounds(lbl, dataLbl, fatalMsg(core))
    else runTimeErrNonBadCharOutBounds(lbl, dataLbl, fatalMsg(core))

  // stable order is defs order
  private val stableOrder: List[String] = defs.map(_._1)

  // Map to maintain String -> List[A32Instr] for each pre-defined runtime error
  private val table: Map[String, List[A32Instr]] =
    defs.iterator.map(d => d._1 -> gen(d)).toMap

  // Emit only the used runtime errors
  def emit(used: Set[String]): List[A32Instr] =
    stableOrder.filter(used.contains).flatMap(lbl => table.getOrElse(lbl, Nil))
}
