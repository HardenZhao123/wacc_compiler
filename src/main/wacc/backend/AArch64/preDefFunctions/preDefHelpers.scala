package wacc.backend.AArch64.preDefFunctions

import wacc.backend.BackendCommon.Immediate
import preDefHelpersConstant.*
import wacc.backend.AArch64.codeGen.A64Instr
import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.AArch64.target.{A64Reg, D, Reg, S}
import wacc.backend.AArch64.AArch64Constants.*
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.BackendConstant.*

// Emits the predefined AArch64 helper routines used by generated programs.
object PreDefHelpers {

  // Common prologue/epilogue for helpers that call libc.
  private def withLR(body: List[A64Instr]): List[A64Instr] =
    List(
      Stp(LINK_REGISTER, ZERO_REGISTER, MemAddress.ImmOffsetAddress(STACK_POINTER, -STACK_ALIGN, PreIndex))
    ) ++ body ++ List(
      Ldp(LINK_REGISTER, ZERO_REGISTER, MemAddress.ImmOffsetAddress(STACK_POINTER, STACK_ALIGN, PostIndex)),
      Ret
    )

  // Common tail used by most printing helpers: call printf, then fflush(0).
  private val afterPrintf: List[A64Instr] =
    List(
      Bl(Label(PRINTF)),
      Mov(RETURN_REG_64, Immediate(MOVE_ZERO)),
      Bl(Label(FFLUSH))
    )

  /** Common tail used by println helper: call puts, then fflush(0). */
  private val afterPuts: List[A64Instr] =
    List(
      Bl(Label(PUTS)),
      Mov(RETURN_REG_64, Immediate(MOVE_ZERO)),
      Bl(Label(FFLUSH))
    )

  // printf("%d"/"%c") style: input in w0, printf expects w1=value and x0=format.
  private def printfW0WithFmt(fmt: Label): List[A64Instr] =
    List(
      Mov(Reg.W1, RETURN_REG_32),
      Adr(RETURN_REG_64, fmt)
    ) ++ afterPrintf

  // printf("%p") style: input in x0, printf expects x1=value and x0=format.
  private def printfX0WithFmt(fmt: Label): List[A64Instr] =
    List(
      Mov(PREDEF_FUNC_REG_64, RETURN_REG_64), // x1 = value
      Adr(RETURN_REG_64, fmt)                 // x0 = format
    ) ++ afterPrintf

  /**
   * printf("%.*s") style: expects x2=chars; loads w1=len from [x2 - STR_HDR], sets x0=format.
   * (caller must ensure PRINT_HELP_REG_64 holds chars if using default arg)
   */
  private def printfLenString(fmt: Label, charsReg: A64Reg): List[A64Instr] =
    List(
      Ldr(Reg.W1, MemAddress.ImmOffsetAddress(charsReg, -STR_HDR, NoIndex)), // w1 = len
      Adr(RETURN_REG_64, fmt)                                     // x0 = format
    ) ++ afterPrintf

  /**
   * Shared template for _readi/_readc.
   * - Saves LR (via withLR)
   * - Preserves old w0 in w19
   * - Allocates STACK_ALIGN bytes on stack as temporary buffer
   * - scanf(fmt, sp)
   * - If scanf != 1: return old w0
   * - Else: load from [sp] into w0, then apply postLoad (e.g. mask for char)
   *
   * Labels are prefixed to avoid name collisions across helpers.
   */
  private def readWithScanf(prefix: String, fmt: Label, postLoad: List[A64Instr] = Nil): List[A64Instr] = {
    val fail = Label(s"$LBL_LOCAL_PREFIX${prefix}$LBL_SUFFIX_FAIL")
    val done = Label(s"$LBL_LOCAL_PREFIX${prefix}$LBL_SUFFIX_DONE")

    withLR(
      List(
        // save old value in w19 (callee-saved)
        Mov(Reg.W19, RETURN_REG_32),

        // allocate local (keeps alignment)
        Sub(STACK_POINTER, STACK_POINTER, Immediate(STACK_ALIGN.toLong)),

        // x1 = sp (address of tmp), x0 = format
        Mov(PREDEF_FUNC_REG_64, STACK_POINTER),
        Adr(RETURN_REG_64, fmt),
        Bl(Label(SCANF)),

        // scanf returns number of assigned items in w0; success for "%d"/" %c" is 1
        Cmp(RETURN_REG_32, Immediate(1)),
        BCond(Cond.NE, fail),

        // success: load -> w0
        Ldr(RETURN_REG_32, MemAddress.ImmOffsetAddress(STACK_POINTER, 0, NoIndex))
      ) ++ postLoad ++ List(
        B(done),

        // fail/EOF: return old value
        fail,
        Mov(RETURN_REG_32, Reg.W19),

        done,
        // deallocate local
        Add(STACK_POINTER, STACK_POINTER, Immediate(STACK_ALIGN.toLong))
      )
    )
  }

  // _printi: printf("%d", w0)  => x0=format, w1=value
  val printInt: List[A64Instr] =
    List(StringData(Label(LBL_PRINTI_STR0), FMT_INT_D, true), Label(_PRINTI)) ++
      withLR(printfW0WithFmt(Label(LBL_PRINTI_STR0)))

  // _printc: printf("%c", w0)  => x0=format, w1=value
  val printChar: List[A64Instr] =
    List(StringData(Label(LBL_PRINTC_STR0), FMT_CHAR_C, true), Label(_PRINTC)) ++
      withLR(printfW0WithFmt(Label(LBL_PRINTC_STR0)))

  // _printfl: promote the float bits in w0 to a double in d0 for printf("%g", ...).
  val printFloat: List[A64Instr] =
    List(StringData(Label(LBL_PRINTFL_STR0), FMT_FLOAT_G, true), Label(_PRINTFL)) ++
      withLR(
        List(
          FMov(S(0), RETURN_REG_32),
          FCvt(D(0), S(0)),
          Adr(RETURN_REG_64, Label(LBL_PRINTFL_STR0))
        ) ++ afterPrintf
      )

  // _printp: printf("%p", x0)  => x0=format, x1=value
  val printPointer: List[A64Instr] =
    List(StringData(Label(LBL_PRINTP_STR0), FMT_PTR_P, true), Label(_PRINTP)) ++
      withLR(printfX0WithFmt(Label(LBL_PRINTP_STR0)))

  // _prints: printf("%.*s", len, chars)
  // StringData layout: .word len ; label: .asciz ...
  // input x0 = chars ptr => len at [x0-ARR_HDR]
  // ABI: x0=format, w1=len, x2=char*
  val printString: List[A64Instr] =
    List(StringData(Label(LBL_PRINTS_STR0), FMT_LEN_STR, true), Label(_PRINTS)) ++
      withLR(
        List(
          // x2 = chars
          Mov(PRINT_HELP_REG_64, RETURN_REG_64)
        ) ++ printfLenString(Label(LBL_PRINTS_STR0), PRINT_HELP_REG_64)
      )

  // _printb: prints "true"/"false" using "%.*s"
  val printBool: List[A64Instr] =
    List(
      StringData(Label(LBL_PRINTB_STR_FALSE), STR_FALSE),
      StringData(Label(LBL_PRINTB_STR_TRUE), STR_TRUE),
      StringData(Label(LBL_PRINTB_FMT0), FMT_LEN_STR, true),
      Label(_PRINTB)
    ) ++ withLR(
      List(
        Cmp(RETURN_REG_32, Immediate(COMPARE_ZERO)),
        BCond(Cond.NE, Label(LBL_PRINTB_TRUE)),
        Adr(PRINT_HELP_REG_64, Label(LBL_PRINTB_STR_FALSE)), // x2 = "false" chars
        B(Label(LBL_PRINTB_GO)),

        Label(LBL_PRINTB_TRUE),
        Adr(PRINT_HELP_REG_64, Label(LBL_PRINTB_STR_TRUE)), // x2 = "true" chars

        Label(LBL_PRINTB_GO)
      ) ++ printfLenString(Label(LBL_PRINTB_FMT0), PRINT_HELP_REG_64)
    )

  // _println: puts("") + fflush
  val printLn: List[A64Instr] =
    List(
      StringData(Label(LBL_PRINTLN_STR0), STR_EMPTY, true),
      Label(_PRINTLN)
    ) ++ withLR(
      List(
        Adr(RETURN_REG_64, Label(LBL_PRINTLN_STR0))
      ) ++ afterPuts
    )

  // _readi: scanf("%d", &tmp); return tmp in w0.
  // If scanf fails/EOF, must return original w0 (do not alter variable).
  val readInt: List[A64Instr] =
    List(
      StringData(Label(LBL_READI_STR0), FMT_INT_D, true),
      Label(_READI)
    ) ++ readWithScanf(READ_PREFIX_INT, Label(LBL_READI_STR0))

  // _readc: scanf(" %c", &tmpByte); return tmp in w0 (low 8 bits used).
  // If scanf fails/EOF, must return original w0 (do not alter variable).
  val readChar: List[A64Instr] =
    List(
      StringData(Label(LBL_READC_STR0), FMT_SCAN_CHAR_SPACE_C, true),
      Label(_READC)
    ) ++ readWithScanf(
      READ_PREFIX_CHAR,
      Label(LBL_READC_STR0),
      postLoad = List(And(RETURN_REG_32, RETURN_REG_32, Immediate(0xFF)))
    )

  // _readfl: scanf("%f", &tmp); return the raw single-precision bits in w0.
  val readFloat: List[A64Instr] =
    List(
      StringData(Label(LBL_READFL_STR0), FMT_FLOAT_F, true),
      Label(_READFL)
    ) ++ readWithScanf(READ_PREFIX_FLOAT, Label(LBL_READFL_STR0))

  // Predefined helper that wraps `malloc` and branches to the OOM trap on failure.
  val malloc: List[A64Instr] =
    List(Label(_MALLOC)) ++
      withLR(List(
        Bl(Label(MALLOC)),
        Cmp(RETURN_REG_64, Immediate(COMPARE_ZERO)),
        BCond(Cond.EQ, Label(PreDefRunTimeError.ErrOutOfMemoryLabel))
      ))

  // Predefined helper for freeing arrays whose runtime pointer skips the length header.
  val freeArr: List[A64Instr] =
    List(Label(_FREEARR)) ++
      withLR(List(
        Sub(RETURN_REG_64, RETURN_REG_64, Immediate(ARR_HDR.toLong)),
        Bl(Label(FREE))
      ))

  // Predefined helper for freeing pairs with a null-pointer check.
  val freePair: List[A64Instr] =
    List(Label(_FREEPAIR)) ++
      withLR(List(
        Cmp(RETURN_REG_64, Immediate(0)),
        BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)),
        Bl(Label(FREE))
      ))

  // Predefined helper that reports an uncaught exception and terminates the program.
  val errUnhandled: List[A64Instr] = List(
    StringData(Label("LBL_ERR_UNHANDLED_PREFIX"), "fatal error: ", true),
    Label(_ERR_UNHANDLED)
  ) ++ withLR(
    List(
      // x0 currently holds the exPtr [ID, MsgPtr]
      // 1. Preserve exPtr on stack by moving to a callee-saved register
      // or pushing/popping. Since withLR doesn't save x19, let's use the stack.
      Sub(STACK_POINTER, STACK_POINTER, Immediate(STACK_ALIGN.toLong)),
      Str(RETURN_REG_64, MemAddress.ImmOffsetAddress(STACK_POINTER, 0, NoIndex)),

      // 2. Print "Fatal Error: " prefix using your existing _PRINTS helper
      Adr(RETURN_REG_64, Label("LBL_ERR_UNHANDLED_PREFIX")),
      Bl(Label(_PRINTS)),

      // 3. Recover exPtr from stack to x0, then load the MsgPtr at offset 8
      Ldr(RETURN_REG_64, MemAddress.ImmOffsetAddress(STACK_POINTER, 0, NoIndex)),
      Ldr(RETURN_REG_64, MemAddress.ImmOffsetAddress(RETURN_REG_64, A64_WORD_SIZE, NoIndex)),

      // 4. Print the actual message string stored in the exception
      Bl(Label(_PRINTS)),

      // 5. Final newline and exit
      Bl(Label(_PRINTLN)),

      // Deallocate local stack space before exiting
      Add(STACK_POINTER, STACK_POINTER, Immediate(STACK_ALIGN.toLong)),

      Mov(Reg.W0, Immediate(RUNTIME_ERROR_EXIT_CODE.toLong)),
      Bl(Label(EXIT))
    )
  )

  // Lookup table from helper label to emitted helper body.
  private val table: Map[String, List[A64Instr]] = Map(
    _PRINTI -> printInt,
    _PRINTC -> printChar,
    _PRINTP -> printPointer,
    _PRINTB -> printBool,
    _PRINTFL -> printFloat,
    _PRINTS -> printString,
    _PRINTLN -> printLn,
    _READI -> readInt,
    _READC -> readChar,
    _READFL -> readFloat,
    _FREEARR -> freeArr,
    _FREEPAIR -> freePair,
    _MALLOC -> malloc,
    _ERR_UNHANDLED -> errUnhandled
  )

  // Stable order for deterministic output
  private val stableOrder: List[String] = List(
    _PRINTI, _PRINTC, _PRINTP, _PRINTS, _PRINTB, _PRINTFL, _PRINTLN,
    _READI, _READC, _READFL, _FREEARR, _FREEPAIR, _MALLOC, _ERR_UNHANDLED
  )

  /** Emit only the requested predefined helpers */
  def emit(used: Set[String]): Seq[A64Instr] =
    stableOrder.filter(used.contains).flatMap(lbl => table.getOrElse(lbl, Nil))
}
