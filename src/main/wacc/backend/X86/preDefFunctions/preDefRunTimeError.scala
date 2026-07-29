package wacc.backend.X86.preDefFunctions

import wacc.common.ExitCode
import wacc.backend.X86.preDefFunctions.preDefHelpersConstant.*
import wacc.backend.X86.codeGen.X86Instr
import wacc.backend.X86.codeGen.X86Instr.*
import wacc.backend.X86.target.*
import wacc.backend.BackendCommon.Immediate
import wacc.backend.X86.X86Constants.*
import wacc.backend.BackendCommon.AsmEntity.*

object PreDefRunTimeError {

  // Exit code used for all runtime failures
  private val FAIL: Int = ExitCode.RuntimeError
  private val ASM_DIRECTIVE_RODATA = ".section .rodata"
  private val ASM_DIRECTIVE_TEXT = ".text"

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

  // ---- shared text bits ----
  private val FatalPrefix = FATAL_PREFIX

  // Wraps the core runtime-error text with the shared fatal prefix/suffix.
  private def fatalMsg(sOrFmt: String): String = FatalPrefix + sOrFmt + FATAL_SUFFIX_NEWLINE_ESC

  private def asmStringLength(value: String): Int = {
    var i = 0
    var length = 0

    while (i < value.length) {
      if (value.charAt(i) == '\\' && i + 1 < value.length) i += 2
      else i += 1
      length += 1
    }

    length
  }

  /**
   * Emit the data section entry for the error message and the label that
   * code generation branches to when this runtime error occurs.
   */
  private def emitTextHeader(labelName: String, dataLabel: String, msg: String): List[X86Instr] =
    List(
      Raw(ASM_DIRECTIVE_RODATA),
      Raw(s"# length of $dataLabel"),
      Raw(s"\t.int ${asmStringLength(msg)}"),
      Raw(s"$dataLabel:"),
      Raw("\t.asciz \"" + msg + "\""),
      Raw(ASM_DIRECTIVE_TEXT),
      Label(labelName),
      And(Reg64(RSP), Immediate(-STACK_ALIGN_BYTES))
    )

  private def leaRipToRdi(dataLabel: String): X86Instr =
    Lea(
      Reg64(RDI),
      Mem(X86Address.RipRelative(Label(dataLabel)), Some(X86Size.QWord))
    )

  private val exitFailure: List[X86Instr] = List(
    Mov(Reg32(RDI), Immediate(FAIL.toLong)),
    Call(Label(LIBC_EXIT))
  )

  private val afterPrintf: List[X86Instr] = List(
    Mov(Reg8(RAX), Immediate(MOVE_ZERO)),
    Call(Label(LIBC_PRINTF)),
    Mov(Reg64(RDI), Immediate(MOVE_ZERO)),
    Call(Label(LIBC_FFLUSH))
  )

  // Helper function to construct runtime error pre-defined functions for non BadChar and ArrayOutOfBounds.
  private def runTimeErrNonBadCharOutBounds(labelName: String, dataLabel: String, msg: String): List[X86Instr] =
    emitTextHeader(labelName, dataLabel, msg) ++
      List(
        leaRipToRdi(dataLabel),
        Call(Label(_PRINTS))
      ) ++ exitFailure

  // Helper function to construct runtime error pre-defined functions for BadChar and ArrayOutOfBounds.
  private def runtTimeErrBadCharOutOfBounds(labelName: String, dataLabel: String, msg: String): List[X86Instr] =
    emitTextHeader(labelName, dataLabel, msg) ++
      List(
        Mov(Reg32(RSI), Reg32(RDI)),
        leaRipToRdi(dataLabel)
      ) ++ afterPrintf ++ exitFailure

  private def gen(d: (String, String, Boolean, String)): List[X86Instr] =
    val (lbl, dataLbl, badCharOutBounds, core) = d
    if (badCharOutBounds) runtTimeErrBadCharOutOfBounds(lbl, dataLbl, fatalMsg(core))
    else runTimeErrNonBadCharOutBounds(lbl, dataLbl, fatalMsg(core))

  // stable order is defs order
  private val stableOrder: List[String] = defs.map(_._1)

  // Lookup table from runtime-error label to its prebuilt helper body.
  private val table: Map[String, List[X86Instr]] = defs.iterator.map(d => d._1 -> gen(d)).toMap

  // Emit only the used runtime errors
  def emit(used: Set[String]): List[X86Instr] =
    stableOrder.filter(used.contains).flatMap(lbl => table.getOrElse(lbl, Nil))
}
