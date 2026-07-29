package wacc.backend.X86.preDefFunctions

import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.BackendConstant.RUNTIME_ERROR_EXIT_CODE
import wacc.backend.BackendCommon.Immediate
import wacc.backend.X86.X86Constants.*
import wacc.backend.X86.codeGen.X86Instr
import wacc.backend.X86.codeGen.X86Instr.*
import wacc.backend.X86.preDefFunctions.preDefHelpersConstant.*
import wacc.backend.X86.target.*

object PreDefHelpers {

  /** Common prologue/epilogue for helpers that call libc. */
  private def withFrame(body: List[X86Instr]): List[X86Instr] =
    List(
      Push(Reg64(RBP)),
      Mov(Reg64(RBP), Reg64(RSP)),
      And(Reg64(RSP), Immediate(-STACK_ALIGN_BYTES))
    ) ++ body ++ List(
      Mov(Reg64(RSP), Reg64(RBP)),
      Pop(Reg64(RBP)),
      Ret
    )

  private def ripMem(label: Label): Mem =
    Mem(X86Address.RipRelative(label), Some(X86Size.QWord))

  private def baseMem(base: X86Reg, size: X86Size, disp: Int = 0): Mem =
    Mem(X86Address.Base(base, disp), Some(size))

  private def leaRip(dst: X86Operand, label: Label): X86Instr =
    Lea(dst, ripMem(label))

  /** Common setup for x86-64 variadic libc calls. */
  private def callVariadic(labelName: String, vectorArgCount: Int = MOVE_ZERO): List[X86Instr] =
    List(
      Mov(Reg8(RAX), Immediate(vectorArgCount.toLong)),
      Call(Label(labelName))
    )

  /** Common tail used by printf-based helpers: printf(...), then fflush(0). */
  private val afterPrintf: List[X86Instr] =
    callVariadic(LIBC_PRINTF) ++ List(
      Mov(Reg64(RDI), Immediate(MOVE_ZERO)),
      Call(Label(LIBC_FFLUSH))
    )

  private val afterPrintfFloat: List[X86Instr] =
    callVariadic(LIBC_PRINTF, FLOAT_VARIADIC_ARG_COUNT) ++ List(
      Mov(Reg64(RDI), Immediate(MOVE_ZERO)),
      Call(Label(LIBC_FFLUSH))
    )

  /** printf("%d"/"%c"/"%p") style: move the incoming value, then put the format in rdi. */
  private def printfRdiWithFmt(fmt: Label, moveArg: X86Instr): List[X86Instr] =
    List(
      moveArg,
      leaRip(Reg64(RDI), fmt)
    ) ++ afterPrintf

  /** printf("%.*s") style: expects chars in charsReg; len is loaded from [charsReg - STR_HDR]. */
  private def printfLenString(fmt: Label, charsReg: X86Reg): List[X86Instr] =
    List(
      Mov(Reg32(RSI), baseMem(charsReg, X86Size.DWord, -STR_HDR)),
      leaRip(Reg64(RDI), fmt)
    ) ++ afterPrintf

  /** Shared template for _readi/_readc. */
  private def readWithScanf(fmt: Label, storeOld: X86Instr, loadResult: X86Instr): List[X86Instr] =
    withFrame(
      List(
        Sub(Reg64(RSP), Immediate(STACK_ALIGN_BYTES)),
        storeOld,
        Lea(Reg64(RSI), baseMem(RSP, X86Size.QWord)),
        leaRip(Reg64(RDI), fmt)
      ) ++ callVariadic(LIBC_SCANF) ++ List(
        loadResult,
        Add(Reg64(RSP), Immediate(STACK_ALIGN_BYTES))
      )
    )

  // pre-defined function for exit
  private val _exit: List[X86Instr] =
    List(Label(_EXIT)) ++ withFrame(
      List(Call(Label(LIBC_EXIT)))
    )

  // Helper function for printf("%d"/"%c"/"%p") pre-defined prints.
  private def printHelper(dataName: String, printStr: String, labelName: String, moveArg: X86Instr): List[X86Instr] =
    List(
      StringData(Label(dataName), printStr, true),
      Label(labelName)
    ) ++ withFrame(printfRdiWithFmt(Label(dataName), moveArg))

  // pre-defined function to print integer
  private val printI: List[X86Instr] =
    printHelper(LBL_PRINTI_STR0, FMT_INT_D, _PRINTI, Mov(Reg32(RSI), Reg32(RDI)))

  // pre-defined function to print character
  private val printC: List[X86Instr] =
    printHelper(LBL_PRINTC_STR0, FMT_CHAR_C, _PRINTC, Mov(Reg8(RSI), Reg8(RDI)))

  // pre-defined function to print float
  private val printFL: List[X86Instr] =
    List(
      StringData(Label(LBL_PRINTFL_STR0), FMT_FLOAT_G, true),
      Label(_PRINTFL)
    ) ++ withFrame(
      List(
        Movd(XMM(0), Reg32(RDI)),
        Cvtss2sd(XMM(0), XMM(0)),
        leaRip(Reg64(RDI), Label(LBL_PRINTFL_STR0))
      ) ++ afterPrintfFloat
    )

  // pre-defined function to print pointer
  private val printP: List[X86Instr] =
    printHelper(LBL_PRINTP_STR0, FMT_PTR_P, _PRINTP, Mov(Reg64(RSI), Reg64(RDI)))

  // pre-defined function to print string
  private val printS: List[X86Instr] =
    List(
      StringData(Label(LBL_PRINTS_STR0), FMT_LEN_STR, true),
      Label(_PRINTS)
    ) ++ withFrame(
      List(
        Mov(Reg64(RDX), Reg64(RDI))
      ) ++ printfLenString(Label(LBL_PRINTS_STR0), RDX)
    )

  // pre-defined function to print boolean
  private val printB: List[X86Instr] =
    List(
      StringData(Label(LBL_PRINTB_STR_FALSE), STR_FALSE),
      StringData(Label(LBL_PRINTB_STR_TRUE), STR_TRUE),
      StringData(Label(LBL_PRINTB_FMT0), FMT_LEN_STR, true),
      Label(_PRINTB)
    ) ++ withFrame(
      List(
        Cmp(Reg8(RDI), Immediate(COMPARE_ZERO)),
        Jcc(X86Cond.NE, Label(LBL_PRINTB_TRUE)),
        leaRip(Reg64(RDX), Label(LBL_PRINTB_STR_FALSE)),
        Jmp(Label(LBL_PRINTB_GO)),
        Label(LBL_PRINTB_TRUE),
        leaRip(Reg64(RDX), Label(LBL_PRINTB_STR_TRUE)),
        Label(LBL_PRINTB_GO)
      ) ++ printfLenString(Label(LBL_PRINTB_FMT0), RDX)
    )

  // pre-defined function to println
  private val printLn: List[X86Instr] =
    List(Label(_PRINTLN)) ++ withFrame(
      List(
        Mov(Reg32(RDI), Immediate(NEWLINE_CHAR)),
        Call(Label(LIBC_PUTCHAR))
      )
    )

  // pre-defined function to read integer
  private val readI: List[X86Instr] =
    List(
      StringData(Label(LBL_READI_STR0), FMT_INT_D, true),
      Label(_READI)
    ) ++ readWithScanf(
      Label(LBL_READI_STR0),
      Mov(baseMem(RSP, X86Size.DWord), Reg32(RDI)),
      Mov(Reg32(RAX), baseMem(RSP, X86Size.DWord))
    )

  // pre-defined function to read character
  private val readC: List[X86Instr] =
    List(
      StringData(Label(LBL_READC_STR0), FMT_SCAN_CHAR_SPACE_C, true),
      Label(_READC)
    ) ++ readWithScanf(
      Label(LBL_READC_STR0),
      Mov(baseMem(RSP, X86Size.Byte), Reg8(RDI)),
      Mov(Reg8(RAX), baseMem(RSP, X86Size.Byte))
    )

  // pre-defined function to read a single-precision float
  private val readFL: List[X86Instr] =
    List(
      StringData(Label(LBL_READFL_STR0), FMT_FLOAT_F, true),
      Label(_READFL)
    ) ++ readWithScanf(
      Label(LBL_READFL_STR0),
      Mov(baseMem(RSP, X86Size.DWord), Reg32(RDI)),
      Mov(Reg32(RAX), baseMem(RSP, X86Size.DWord))
    )

  // pre-defined function to malloc array or pair
  private val malloc: List[X86Instr] =
    List(Label(_MALLOC)) ++ withFrame(
      List(
        Call(Label(LIBC_MALLOC)),
        Cmp(Reg64(RAX), Immediate(COMPARE_ZERO)),
        Jcc(X86Cond.E, Label(ERR_OUT_OF_MEMORY))
      )
    )

  // pre-defined function to free of arrays
  private val free: List[X86Instr] =
    List(Label(_FREE)) ++ withFrame(
      List(Call(Label(LIBC_FREE)))
    )

  // pre-defined function to free of pair values
  private val freePair: List[X86Instr] =
    List(Label(_FREEPAIR)) ++ withFrame(
      List(
        Cmp(Reg64(RDI), Immediate(COMPARE_ZERO)),
        Jcc(X86Cond.E, Label(ERR_NULL)),
        Call(Label(LIBC_FREE))
      )
    )

  // pre-defined function for exception handler
  private val errUnhandled: List[X86Instr] =
    List(
      StringData(Label(LBL_ERR_UNHANDLED_PREFIX), FATAL_PREFIX, true),
      Label(_ERR_UNHANDLED)
    ) ++ withFrame(
      List(
        Sub(Reg64(RSP), Immediate(STACK_ALIGN_BYTES)),
        Mov(baseMem(RSP, X86Size.QWord), Reg64(RDI)),
        leaRip(Reg64(RDI), Label(LBL_ERR_UNHANDLED_PREFIX)),
        Call(Label(_PRINTS)),
        Mov(Reg64(RDI), baseMem(RSP, X86Size.QWord)),
        Mov(Reg64(RDI), baseMem(RDI, X86Size.QWord, 8)),
        Call(Label(_PRINTS)),
        Call(Label(_PRINTLN)),
        Add(Reg64(RSP), Immediate(STACK_ALIGN_BYTES)),
        Mov(Reg32(RDI), Immediate(RUNTIME_ERROR_EXIT_CODE.toLong)),
        Call(Label(LIBC_EXIT))
      )
    )

  // Map that maintains the String -> List[X86Instr] for each pre-defined function
  private val table: Map[String, List[X86Instr]] = Map(
    _EXIT -> _exit,
    _PRINTI -> printI,
    _PRINTC -> printC,
    _PRINTB -> printB,
    _PRINTFL -> printFL,
    _PRINTS -> printS,
    _PRINTP -> printP,
    _PRINTLN -> printLn,
    _READI -> readI,
    _READC -> readC,
    _READFL -> readFL,
    _MALLOC -> malloc,
    _FREE -> free,
    _FREEPAIR -> freePair,
    _ERR_UNHANDLED -> errUnhandled
  )

  // Stable order for deterministic output
  private val stableOrder: List[String] = List(
    _EXIT, _PRINTI, _PRINTC, _PRINTB, _PRINTFL, _PRINTS,
    _PRINTP, _PRINTLN, _READI, _READC, _READFL, _MALLOC,
    _FREE, _FREEPAIR, _ERR_UNHANDLED
  )

  /** Emit only the requested predefined helpers */
  def emit(used: Set[String]): Seq[X86Instr] =
    stableOrder.filter(used.contains).flatMap(lbl => table.getOrElse(lbl, Nil))
}
