package wacc.backend.Arm32.preDefFunctions

import wacc.backend.Arm32.target.*
import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.codeGen.A32Instr
import wacc.backend.BackendCommon.Immediate
import wacc.backend.Arm32.Arm32Constants.*
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.Arm32.preDefFunctions.preDefHelpersConstant.*
import wacc.backend.BackendCommon.BackendConstant.*

object PreDefHelpers {

  /** Common prologue/epilogue for helpers that call libc. */
  private def withFrame(body: List[A32Instr]): List[A32Instr] =
    List(
      Push(List(FP, LR)),
      Mov(FP, SP),
      Bic(SP, SP, Immediate(STACK_ALIGN_MASK))
    ) ++ body ++ List(
      Mov(SP, FP),
      Pop(List(FP, PC))
    )

  /** Common tail used by printf-based helpers: printf(...), then fflush(0). */
  private val afterPrintf: List[A32Instr] = List(
    Bl(Label(LIBC_PRINTF)),
    Mov(RETURN_REG, Immediate(MOVE_ZERO)),
    Bl(Label(LIBC_FFLUSH))
  )

  /** Common tail used by println helper: puts(...), then fflush(0). */
  private val afterPuts: List[A32Instr] = List(
    Bl(Label(LIBC_PUTS)),
    Mov(RETURN_REG, Immediate(MOVE_ZERO)),
    Bl(Label(LIBC_FFLUSH))
  )

  /** printf("%d"/"%c"/"%p") style: input in r0, printf expects r1=value and r0=format. */
  private def printfR0WithFmt(fmt: Label): List[A32Instr] =
    List(
      Mov(R1_REG, RETURN_REG),
      Adr(RETURN_REG, fmt)
    ) ++ afterPrintf

  /** printf("%.*s") style: expects chars in charsReg; len is loaded from [charsReg-STR_HDR]. */
  private def printfLenString(fmt: Label, charsReg: A32Reg): List[A32Instr] =
    List(
      Ldr(R1_REG, MemAddress.ImmOffsetAddress(charsReg, -STR_HDR, NoIndex)),
      Adr(RETURN_REG, fmt)
    ) ++ afterPrintf

  /**
   * Shared template for _readi/_readc.
   * Keep current behaviour: preserve old r0 by writing it to stack before scanf,
   * then always reload from stack after scanf.
   */
  private def readWithScanf(fmt: Label, storeOld: A32Instr, loadResult: A32Instr): List[A32Instr] =
    withFrame(
      List(
        Sub(SP, SP, Immediate(READ_STACK_BYTES)),
        storeOld,
        Mov(R1_REG, SP),
        Adr(RETURN_REG, fmt),
        Bl(Label(LIBC_SCANF)),
        loadResult,
        Add(SP, SP, Immediate(READ_STACK_BYTES))
      )
    )

  //  pre-defined function for exit
  private val _exit: List[A32Instr] =
    List(Label(_EXIT)) ++ withFrame(
      List(Bl(Label(LIBC_EXIT)))
    )

  // Helper function for pre-defined prints
  private def printHelper(dataName: String, printStr: String, labelName: String): List[A32Instr] =
    List(
      StringData(Label(dataName), printStr, true),
      Label(labelName)
    ) ++ withFrame(printfR0WithFmt(Label(dataName)))

  // pre-defined function to print integer
  private val printI: List[A32Instr] = printHelper(LBL_PRINTI_STR0, FMT_INT_D, _PRINTI)

  // pre-defined function to print character
  private val printC: List[A32Instr] = printHelper(LBL_PRINTC_STR0, FMT_CHAR_C, _PRINTC)

  // pre-defined function to print pointer
  private val printP: List[A32Instr] = printHelper(LBL_PRINTP_STR0, FMT_PTR_P, _PRINTP)

  // pre-defined function to print string
  private val printS: List[A32Instr] = List(
    StringData(Label(LBL_PRINTS_STR0), FMT_LEN_STR, true),
    Label(_PRINTS)
  ) ++ withFrame(
    List(
      Mov(R2_REG, RETURN_REG)
    ) ++ printfLenString(Label(LBL_PRINTS_STR0), R2_REG)
  )

  // pre-defined function to print boolean
  private val printB: List[A32Instr] = List(
    StringData(Label(LBL_PRINTB_STR_FALSE), STR_FALSE),
    StringData(Label(LBL_PRINTB_STR_TRUE), STR_TRUE),
    StringData(Label(LBL_PRINTB_FMT0), FMT_LEN_STR, true),
    Label(_PRINTB)
  ) ++ withFrame(
    List(
      Cmp(RETURN_REG, Immediate(COMPARE_ZERO)),
      BCond(Cond.NE, Label(LBL_PRINTB_TRUE)),
      Adr(R2_REG, Label(LBL_PRINTB_STR_FALSE)),
      B(Label(LBL_PRINTB_GO)),
      Label(LBL_PRINTB_TRUE),
      Adr(R2_REG, Label(LBL_PRINTB_STR_TRUE)),
      Label(LBL_PRINTB_GO)
    ) ++ printfLenString(Label(LBL_PRINTB_FMT0), R2_REG)
  )

  // pre-defined function to println
  private val printLn: List[A32Instr] = List(
    StringData(Label(LBL_PRINTLN_STR0), STR_EMPTY, true),
    Label(_PRINTLN)
  ) ++ withFrame(
    List(
      Adr(RETURN_REG, Label(LBL_PRINTLN_STR0))
    ) ++ afterPuts
  )

  // pre-defined function to read integer
  private val readI: List[A32Instr] =
    List(
      StringData(Label(LBL_READI_STR0), FMT_INT_D, true),
      Label(_READI)
    ) ++ readWithScanf(
      Label(LBL_READI_STR0),
      Str(RETURN_REG, MemAddress.ImmOffsetAddress(SP, 0, NoIndex)),
      Ldr(RETURN_REG, MemAddress.ImmOffsetAddress(SP, 0, NoIndex))
    )

  // pre-defined function to read character
  private val readC: List[A32Instr] =
    List(
      StringData(Label(LBL_READC_STR0), FMT_SCAN_CHAR_SPACE_C, true),
      Label(_READC)
    ) ++ readWithScanf(
      Label(LBL_READC_STR0),
      Strb(RETURN_REG, MemAddress.ImmOffsetAddress(SP, 0, NoIndex)),
      Ldrb(RETURN_REG, MemAddress.ImmOffsetAddress(SP, 0, NoIndex))
    )

  // pre-defined function to malloc array or pair
  private val malloc: List[A32Instr] =
    List(Label(_MALLOC)) ++ withFrame(
      List(
        Bl(Label(LIBC_MALLOC)),
        Cmp(RETURN_REG, Immediate(COMPARE_ZERO)),
        BlCond(Cond.EQ, Label(PreDefRunTimeError.ErrOutOfMemoryLabel))
      )
    )

  // pre-defined function to free of pair values
  private val freePair: List[A32Instr] =
    List(Label(_FREEPAIR)) ++ withFrame(
      List(
        Cmp(RETURN_REG, Immediate(COMPARE_ZERO)),
        BlCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)),
        Bl(Label(LIBC_FREE))
      )
    )

  // pre-defined function to free of arrays
  private val free: List[A32Instr] =
    List(Label(_FREE)) ++ withFrame(
      List(Bl(Label(LIBC_FREE)))
    )

  // pre-defined function for exception handler
  private val errUnhandled: List[A32Instr] = List(
    StringData(Label(LBL_ERR_UNHANDLED_PREFIX), "fatal error: ", true),
    Label(_ERR_UNHANDLED)
  ) ++ withFrame(
    List(
      Push(List(RETURN_REG)),
      Adr(RETURN_REG, Label(LBL_ERR_UNHANDLED_PREFIX)),
      Bl(Label(_PRINTS)),
      Pop(List(RETURN_REG)),
      Ldr(RETURN_REG, MemAddress.ImmOffsetAddress(RETURN_REG, A32_WORD_SIZE, NoIndex)),
      Bl(Label(_PRINTS)),
      Bl(Label(_PRINTLN)),
      Mov(RETURN_REG, Immediate(RUNTIME_ERROR_EXIT_CODE)),
      Bl(Label(LIBC_EXIT))
    )
  )

  // Map that maintains the String -> List[A32Instr] for each pre-defined function
  private val table: Map[String, List[A32Instr]] = Map(
    _EXIT -> _exit,
    _PRINTI -> printI,
    _PRINTB -> printB,
    _PRINTC -> printC,
    _PRINTS -> printS,
    _PRINTP -> printP,
    _PRINTLN -> printLn,
    _READI -> readI,
    _READC -> readC,
    _MALLOC -> malloc,
    _FREE -> free,
    _FREEPAIR -> freePair,
    _ERR_UNHANDLED -> errUnhandled
  )

  // Stable order for deterministic output
  private val stableOrder: List[String] = List(
    _EXIT, _PRINTI, _PRINTB, _PRINTC, _PRINTS, _PRINTP,
    _PRINTLN, _READI, _READC, _MALLOC, _FREE, _FREEPAIR,
    _ERR_UNHANDLED
  )

  /** Emit only the requested predefined helpers */
  def emit(used: Set[String]): Seq[A32Instr] =
    stableOrder.filter(used.contains).flatMap(lbl => table.getOrElse(lbl, Nil))
}
