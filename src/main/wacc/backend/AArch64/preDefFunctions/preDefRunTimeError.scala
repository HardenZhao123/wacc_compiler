package wacc.backend.AArch64.preDefFunctions

import wacc.backend.BackendCommon.Immediate
import preDefHelpersConstant.*
import wacc.backend.AArch64.target.Reg.*
import wacc.common.ExitCode
import wacc.backend.AArch64.codeGen.A64Instr
import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.BackendCommon.AsmEntity.*

// Builds reusable AArch64 runtime-error routines and their backing string data.
object PreDefRunTimeError {

  private val FAIL: Int = ExitCode.RuntimeError

  // Labels that codegen should branch to / call.
  val ErrNullLabel        = "_errNull"
  val ErrOverflowLabel    = "_errOverflow"
  val ErrOutOfMemoryLabel = "_errOutOfMemory"
  val ErrDivZeroLabel     = "_errDivZero"
  val ErrOutOfBoundsLabel = "_errOutOfBounds"
  val ErrBadCharLabel     = "_errBadChar"

  // (label, dataLabel, takesInt, message)
  private val defs: List[(String, String, Boolean, String)] = List(
    (ErrNullLabel, LBL_ERRNULL_STR0, false, MSG_NULL_DEREF_OR_FREED),
    (ErrOverflowLabel, LBL_ERROVERFLOW_STR0, false, MSG_INT_OVERFLOW_OR_UNDERFLOW),
    (ErrOutOfMemoryLabel, LBL_ERROUTOFMEMORY_STR0, false, MSG_OUT_OF_MEMORY),
    (ErrDivZeroLabel, LBL_ERRDIVZERO_STR0, false, MSG_DIV_OR_MOD_BY_ZERO),
    (ErrOutOfBoundsLabel, LBL_ERROUTOFBOUNDS_STR0, true, MSG_ARRAY_INDEX_OOB_FMT),
    (ErrBadCharLabel, LBL_ERRBADCHAR_STR0, true, MSG_INT_NOT_ASCII_0_127_FMT)
  )

  // ---- shared text bits ----
  private val FatalPrefix = FATAL_PREFIX
  // Wraps the core runtime-error text with the shared fatal prefix/suffix.
  private def fatalMsg(sOrFmt: String): String = FatalPrefix + sOrFmt + FATAL_SUFFIX_NEWLINE_ESC

  // Emits the `.data` entry holding one runtime-error message string.
  private def emitStringData(label: String, str: String): List[A64Instr] =
    List(
      Raw(ASM_DIRECTIVE_DATA),
      Raw(s"$label:"),
      Raw(s"""$ASM_DIRECTIVE_ASCIZ_WITH_INDENT"$str"""")
    )

  // Emits the `.text` header and entry label for one runtime-error helper.
  private def emitTextHeader(labelName: String): List[A64Instr] =
    List(
      Raw(ASM_DIRECTIVE_TEXT),
      Label(labelName)
    )

  // only replace Raw where A64Instr exists
  private def loadAddrToX0(label: String): List[A64Instr] = {
    val l = Label(label)
    List(
      Adrp(X0, l),
      Add(X0, X0, MaskedLabel(12, l)) // => add x0, x0, :lo12:label
    )
  }

  // Shared tail that prints the message, flushes stdout, and terminates the program.
  private val runTimeErrorTail: List[A64Instr] = List(
    Bl(Label(LIBC_PRINTF)),
    Mov(X0, Immediate(0)),
    Bl(Label(LIBC_FFLUSH)),
    Mov(W0, Immediate(FAIL)),
    Bl(Label(LIBC_EXIT))
  )

  // Builds a runtime-error helper whose message takes no extra argument.
  private def runtimeNoArg(labelName: String, dataLabel: String, msg: String): List[A64Instr] =
    emitStringData(dataLabel, msg) ++
      emitTextHeader(labelName) ++
      loadAddrToX0(dataLabel) ++
      runTimeErrorTail

  // Builds a runtime-error helper that prints the integer currently held in `x0`.
  private def runtimeWithInt(labelName: String, dataLabel: String, fmt: String): List[A64Instr] =
    emitStringData(dataLabel, fmt) ++
      emitTextHeader(labelName) ++
      List(Mov(X1, X0)) ++
      loadAddrToX0(dataLabel) ++
      runTimeErrorTail

  // Expands one runtime-error definition tuple into its emitted instruction sequence.
  private def gen(d: (String, String, Boolean, String)): List[A64Instr] = {
    val (lbl, dataLbl, takesInt, core) = d
    if (takesInt) runtimeWithInt(lbl, dataLbl, fatalMsg(core))
    else          runtimeNoArg(lbl, dataLbl, fatalMsg(core))
  }

  // stable order is defs order
  private val stableOrder: List[String] = defs.map(_._1)

  // Lookup table from runtime-error label to its prebuilt helper body.
  private val table: Map[String, List[A64Instr]] =
    defs.iterator.map(d => d._1 -> gen(d)).toMap

  // Emit only the used runtime errors
  def emit(used: Set[String]): List[A64Instr] =
    stableOrder.filter(used.contains).flatMap(lbl => table.getOrElse(lbl, Nil))
}
