package wacc.backend.Arm32.codeGen

import scala.collection.mutable

import wacc.backend.midir.TAC.*
import wacc.backend.midir.TAC
import wacc.backend.Arm32.target.A32StackFrame
import wacc.backend.Arm32.target.*
import wacc.backend.Arm32.Arm32Constants.*
import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.preDefFunctions.PreDefRunTimeError
import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.fromTACCondOpToCond.*
import wacc.backend.Arm32.codeGen.A32MemAddress
import wacc.backend.BackendCommon.AsmEntity.*

/** Converts TACProgram and TAC instructions into ARM32 assembly (A32Instr). */
object A32Generator {

  // Labels for storing exceptions in global memory
  private val ExceptionFlagLabel = WACC_EXCEPTION_FLAG
  private val ExceptionValueLabel = WACC_EXCEPTION_VALUE

  /** Main entry: convert TACProgram into a list of ARM32 instructions */
  def genA32Program(program: TACProgram): List[A32Instr] = {

    // Generate string data labels for string literals in the program
    val strs = program.strs.zipWithIndex.map { case (str, id) =>
      val label = Label(s"$STRING_LABEL_PREFIX$id")
      StringData(label, str)
    }

    // Initialise global variables for exception handling
    val globals = Seq(
      WordData(Label(ExceptionFlagLabel), 0),
      WordData(Label(ExceptionValueLabel), 0)
    )

    // MAIN FUNCTION
    val mainFrame = new A32StackFrame(program.locals)

    given gr: GenRes = new GenRes()
    given cs: A32CodegenState = new A32CodegenState(mainFrame)

    // Header for assembly program
    val header: List[A32Instr] = List(
      DataSeg(strs ++ globals),
      Raw(BackendConstant.ASM_DIRECTIVE_ALIGN_4),
      Raw(ASM_DIRECTIVE_TEXT),
      Raw(ASM_DIRECTIVE_GLOBAL_MAIN)
    )

    // Emit main label and stack frame setup
    gr.emit(Label(LABEL_MAIN))
    mainFrame.addFrame().foreach(gr.emit)

    // Generate instructions for program body
    program.body.foreach(genA32Instr(_)(using cs, gr))

    // Set return value to 0 and remove stack frame
    gr.emit(Mov(RETURN_REG, Immediate(0)))
    mainFrame.removeFrame().foreach(gr.emit)

    // Ensures constants used in main are reachable.
    gr.emitLiteralPool()

    // Generate functions
    program.funcs.foreach { f => emitFunctionInto(f)(using gr) }

    // Combine header, generated instructions, and pre-defined runtime/error routines
    val result: mutable.Builder[A32Instr, List[A32Instr]] = List.newBuilder
    result ++= header
    result ++= gr.instrs
    result ++= gr.preDefsErrs

    result.result()
  }

  /** Generate ARM32 instructions for a single TAC instruction */
  def genA32Instr(instr: TAC.Instr)(using cs: A32CodegenState, gr: GenRes): Unit = instr match {

    // Exit
    case TACExit(code) =>
      cs.withEval(code) { reg =>
        gr.emit(Mov(RETURN_REG, reg))
        gr.emit(Bl(Label(_EXIT)))
        gr.addPreDefs(_EXIT)
      }

    // Skip: do nothing
    case TACSkip() => ()

    // Emit label for jump targets
    case Mark(l) => gr.emit(Label(l.name))

    // Unconditional jump
    case Jmp(to) => gr.emit(B(Label(to.name)))

    // Conditional jump
    case CmpJmp(cond, lhs, rhs, to) =>
      cs.withEval2(lhs, rhs) { (reg1, reg2) =>
        gr.emit(Cmp(reg1, reg2))
        gr.emit(BCond(toCond(cond), Label(to.name)))
      }

    // Return from function
    case TACReturn(v) =>
      cs.frame.loadRhs(v, RETURN_REG, cs.ra)
      cs.frame.removeFrame().foreach(gr.emit)

    // Exception handling: store exception value globally
    case TACStoreException(value) =>
      cs.withEval(value) { valueReg =>
        storeGlobalWord(ExceptionFlagLabel, Immediate(1))
        storeGlobalWord(ExceptionValueLabel, valueReg)
      }

    case TACLoadExceptionFlag(dst) => loadGlobalWordInto(dst, ExceptionFlagLabel)

    case TACLoadExceptionValue(dst) => loadGlobalWordInto(dst, ExceptionValueLabel)

    case TACClearException() =>
      storeGlobalWord(ExceptionFlagLabel, Immediate(0))
      storeGlobalWord(ExceptionValueLabel, Immediate(0))

    // Runtime error reporting
    case TACReportError(exPtr) =>
      cs.frame.loadRhs(exPtr, RETURN_REG, cs.ra)
      gr.emit(Bl(Label(_ERR_UNHANDLED)))
      gr.addPreDefs(_ERR_UNHANDLED)
      gr.addPreDefs(_PRINTS)
      gr.addPreDefs(_PRINTLN)

    // Check for binary operations
    case TAC.CheckedBinOp(dst, op, lhs, rhs, onOverflow) =>
      cs.withEval2(lhs, rhs) { (lreg, rreg) =>
        op match {
          case TAC.ArithOp.Add => emitCheckedOverflowInstr(Adds.apply, lreg, rreg, onOverflow)
          case TAC.ArithOp.Sub => emitCheckedOverflowInstr(Subs.apply, lreg, rreg, onOverflow)

          case TAC.ArithOp.Mul =>
            cs.ra.withNewRegisters2 { (tmpReg, cmpReg) =>
              // SMULL produces 64-bit result: check for overflow
              gr.emit(SMull(lreg, tmpReg, lreg, rreg))
              gr.emit(Asr(cmpReg, lreg, Immediate(OverflowCheck)))
              gr.emit(Cmp(tmpReg, cmpReg))
              gr.emit(BCond(Cond.NE, Label(onOverflow.name)))
            }

          case unsupported => throw new IllegalArgumentException(s"Unsupported checked arithmetic op: $unsupported")
        }

        cs.frame.storeTemp(dst, lreg)
      }

    // Check for unary operations
    case TAC.CheckedUnOp(dest, op, x, onOverflow) =>
      cs.withEval(x) { reg =>
        op match {
          case TAC.UnaryOp.Neg =>
            gr.emit(Rsbs(reg, reg, Immediate(0))) // reg = 0 - reg
            gr.emit(BCond(Cond.VS, Label(onOverflow.name))) // Branch on overflow
          case unsupported => throw new IllegalArgumentException(s"Unsupported checked unary op: $unsupported")
        }

        cs.frame.storeTemp(dest, reg)
      }

    // Binary operations
    case TAC.BinOp(dst, op, lhs, rhs) =>
      cs.withEval2(lhs, rhs) { (lreg, rreg) =>
        op match {
          // Arithmetic ops with overflow 
          case a: TAC.ArithOp =>
            a match {
              case TAC.ArithOp.Add => emitOverflowInstr(Adds.apply, lreg, rreg)
              case TAC.ArithOp.Sub => emitOverflowInstr(Subs.apply, lreg, rreg)

              case TAC.ArithOp.Mul =>
                cs.ra.withNewRegisters2 { (tmpReg, cmpReg) =>
                  gr.emit(SMull(lreg, tmpReg, lreg, rreg))
                  gr.emit(Asr(cmpReg, lreg, Immediate(OverflowCheck)))
                  gr.emit(Cmp(tmpReg, cmpReg))
                  gr.emit(BCond(Cond.NE, Label(PreDefRunTimeError.ErrOverflowLabel)))
                  gr.addPreDefs(_PRINTS)
                  gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)
                }

              case TAC.ArithOp.Div =>
                emitDivModInstr(lreg, rreg)
                gr.emit(Mov(lreg, RETURN_REG))
              case TAC.ArithOp.Mod =>
                emitDivModInstr(lreg, rreg)
                gr.emit(Mov(lreg, R1_REG))
            }

          // Boolean ops 
          case b: TAC.BoolOp =>
            b match {
              case TAC.BoolOp.And => gr.emit(And(lreg, lreg, rreg))
              case TAC.BoolOp.Or => gr.emit(Orr(lreg, lreg, rreg))
            }

          // Comparison ops: result 0/1 
          case c: TAC.CondOp => emitComparison(c, lreg, rreg)

          // Bitwise operations 
          case TAC.BitwiseOp.BitAnd => emitOverflowInstr(And.apply, lreg, rreg)
          case TAC.BitwiseOp.BitOr => emitOverflowInstr(Orr.apply, lreg, rreg)
        }

        cs.frame.storeTemp(dst, lreg)
      }

    // Unary operations
    case TAC.UnOp(dest, op, x) =>
      cs.withEval(x) { reg =>
        op match {
          case TAC.UnaryOp.Neg =>
            gr.emit(Rsbs(reg, reg, Immediate(0)))
            gr.emit(BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)))
            gr.addPreDefs(_PRINTS)
            gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)

          case TAC.UnaryOp.Not =>
            // Convert 0/1 boolean
            gr.emit(Cmp(reg, Immediate(1)))
            gr.emit(MovCond(reg, Immediate(0), Cond.EQ))
            gr.emit(MovCond(reg, Immediate(1), Cond.NE))

          case TAC.UnaryOp.BitNot =>
            gr.emit(Mvn(reg, reg))
            gr.emit(BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)))
            gr.addPreDefs(_PRINTS)
            gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)

          case TAC.UnaryOp.Ord => ()

          case TAC.UnaryOp.Chr =>
            // Check ASCII range
            val badCharLabel = Label(PreDefRunTimeError.ErrBadCharLabel)
            gr.emit(Cmp(reg, Immediate(ASCII_MIN)))
            gr.emit(BCond(Cond.LT, badCharLabel))
            gr.emit(Cmp(reg, Immediate(ASCII_MAX)))
            gr.emit(BCond(Cond.GT, badCharLabel))

            gr.addUsedErrs(PreDefRunTimeError.ErrBadCharLabel)

          case TAC.UnaryOp.Len =>
            // Check null pointer
            gr.emit(Cmp(reg, Immediate(0)))
            gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)))
            gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)

            gr.emit(Ldr(reg, MemAddress.ImmOffsetAddress(reg, -ARR_HDR, NoIndex)))
        }

        cs.frame.storeTemp(dest, reg)
      }

    // Println
    case PrintLn() =>
      gr.emit(Bl(Label(_PRINTLN)))
      gr.addPreDefs(_PRINTLN)

    case print: Print =>
      cs.withEval(print.content) { reg =>
        gr.emit(Mov(RETURN_REG, reg))

        print match {
          case _: PrintInt     => emitPrint(_PRINTI) // Print integer
          case _: PrintBool    => emitPrint(_PRINTB) // Print boolean
          case _: PrintChar    => emitPrint(_PRINTC) // Print character
          case _: PrintStr     => emitPrint(_PRINTS) // Print string
          case _: PrintPointer => emitPrint(_PRINTP) // Print pointer
        }
      }

    // Read
    case TACRead(dest, t) =>
      cs.frame.loadRhs(dest, RETURN_REG, cs.ra)

      t match {
        case TAC.ReadType.Int =>
          gr.emit(Bl(Label(_READI)))
          gr.addPreDefs(_READI)
        case TAC.ReadType.Char =>
          gr.emit(Bl(Label(_READC)))
          gr.addPreDefs(_READC)
      }

      cs.frame.storeTemp(dest, RETURN_REG)

    // Free memory
    case TACFree(x, isPair) =>
      cs.withEval(x) { reg =>
        if (isPair) {
          gr.emit(Mov(RETURN_REG, reg))
          gr.emit(Bl(Label(_FREEPAIR)))
          gr.addPreDefs(_FREEPAIR)
          gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
        } else {
          gr.emit(Sub(reg, reg, Immediate(ARR_HDR)))
          gr.emit(Mov(RETURN_REG, reg))
          gr.emit(Bl(Label(_FREE)))
          gr.addPreDefs(_FREE)
        }

        gr.addPreDefs(_PRINTS)
      }

    // Function call
    case TACCall(dest, func, args) =>
      val stackBytes = emitCallArgs(args) // Push arguments on stack if needed
      gr.emit(Bl(Label(func.name))) // Branch to function
      if (stackBytes > 0) gr.emit(Add(SP, SP, Immediate(stackBytes.toLong))) // Restore stack

      cs.frame.storeTemp(dest, RETURN_REG)

    // Assignment
    case TACAssign(lhs, rhs) => cs.withEval(rhs) { reg => cs.frame.storeTemp(lhs, reg) }
  }

  // Helper: get a register holding address of global label 
  private def withGlobalAddress[A](labelName: String)(f: A32Reg => A)(using cs: A32CodegenState, gr: GenRes): A =
    cs.ra.withNewRegister { addrReg =>
      gr.emit(LdrFuncPtr(addrReg, MemAddress.LabelAddress(Label(s"=$labelName"))))
      f(addrReg)
    }

  // Store a word to a global variable 
  private def storeGlobalWord(labelName: String, value: A32Operand)(using cs: A32CodegenState, gr: GenRes): Unit =
    withGlobalAddress(labelName) { addr =>
      cs.ra.withNewRegister { dataReg =>
        gr.emit(Mov(dataReg, value))
        gr.emit(Str(dataReg, MemAddress.BaseRegAddress(addr)))
      }
    }

  // Load a word from a global variable into a temporary 
  private def loadGlobalWordInto(dst: TAC.Temp, labelName: String)(using cs: A32CodegenState, gr: GenRes): Unit =
    withGlobalAddress(labelName) { addr =>
      cs.ra.withNewRegister { dataReg =>
        gr.emit(Ldr(dataReg, MemAddress.BaseRegAddress(addr)))
        cs.frame.storeTemp(dst, dataReg)
      }
    }

  // Emit a call to a print function 
  private def emitPrint(printLabel: String)(using gr: GenRes): Unit = {
    gr.emit(Bl(Label(printLabel)))
    gr.addPreDefs(printLabel)
  }

  // Emit arithmetic instruction and check for overflow 
  private def emitCheckedOverflowInstr(instr: (A32Reg, A32Reg, A32Reg) => A32Instr, l: A32Reg, r: A32Reg, onOverflow: wacc.backend.label.Label)
                                      (using gr: GenRes): Unit = {
    gr.emit(instr(l, l, r))
    gr.emit(BCond(Cond.VS, Label(onOverflow.name)))
  }

  // Emit arithmetic instruction and branch on predefined overflow error 
  private def emitOverflowInstr(instr: (A32Reg, A32Reg, A32Reg) => A32Instr, l: A32Reg, r: A32Reg)
                               (using gr: GenRes): Unit = {
    gr.emit(instr(l, l, r))
    gr.emit(BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)))
    gr.addPreDefs(_PRINTS)
    gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)
  }

  // Emit division/modulo instructions with runtime check for division by zero 
  private def emitDivModInstr(l: A32Reg, r: A32Reg)(using gr: GenRes): Unit = {
    gr.emit(Cmp(r, Immediate(0)))
    gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrDivZeroLabel)))
    gr.emit(Mov(RETURN_REG, l))
    gr.emit(Mov(R1_REG, r))
    gr.emit(Bl(Label(AEABI_IDIVMOD)))
    gr.addUsedErrs(PreDefRunTimeError.ErrDivZeroLabel)
    gr.addPreDefs(_PRINTS)
  }

  // Emit comparison resulting in 0/1 boolean 
  private def emitComparison(cond: TAC.CondOp, reg1: A32Reg, reg2: A32Reg)(using gr: GenRes): Unit = {
    val (trueCond, falseCond) = cond match {
      case TAC.CondOp.EQ  => (Cond.EQ, Cond.NE)
      case TAC.CondOp.NEQ => (Cond.NE, Cond.EQ)
      case TAC.CondOp.LT  => (Cond.LT, Cond.GE)
      case TAC.CondOp.GT  => (Cond.GT, Cond.LE)
      case TAC.CondOp.LEQ => (Cond.LE, Cond.GT)
      case TAC.CondOp.GEQ => (Cond.GE, Cond.LT)
    }

    gr.emit(Cmp(reg1, reg2))
    gr.emit(MovCond(reg1, Immediate(1), trueCond))
    gr.emit(MovCond(reg1, Immediate(0), falseCond))
  }

  // Align stack size to 8 bytes 
  private def align8(n: Int): Int = {
    val r = n % STACK_ALLIGNMENT
    if (r == 0) n else n + (STACK_ALLIGNMENT - r)
  }

  // Push arguments for function call on stack or into registers
  private def emitCallArgs(args: List[TAC.Rhs])(using cs: A32CodegenState, gr: GenRes): Int = {
    val n = args.length
    val nStack = math.max(0, n - ARG_REG_COUNT)
    val stackBytes = align8(nStack * SLOT_SIZE)

    if (stackBytes > 0) gr.emit(Sub(SP, SP, Immediate(stackBytes.toLong)))

    // Stack arguments
    for (i <- ARG_REG_COUNT until n) {
      val arg = args(i)
      cs.ra.withNewRegister { reg =>
        cs.frame.loadRhs(arg, reg, cs.ra)
        val off = SLOT_SIZE * (i - ARG_REG_COUNT)
        val immOffsetAddr: A32MemAddress = MemAddress.ImmOffsetAddress(SP, off, NoIndex)
        val size = arg.len.convertToBytes(A32_WORD_SIZE)
        if (size == BoolCharSize) gr.emit(Strb(reg, immOffsetAddr))
        else gr.emit(Str(reg, immOffsetAddr))
      }
    }

    // Register arguments (R0-R3)
    for (i <- 0 until math.min(ARG_REG_COUNT, n)) {
      val arg = args(i)
      cs.frame.loadRhs(arg, R(i), cs.ra)
    }

    stackBytes
  }

  // Load incoming parameters from registers/stack into frame 
  private def initParams(frame: A32StackFrame, params: List[TAC.Temp])
                        (using cs: A32CodegenState, gr: GenRes): Unit = {
    val scratch = SCRATCH_REG_START

    for ((t, i) <- params.zipWithIndex) {
      if (i < ARG_REG_COUNT) {
        frame.storeTemp(t, R(i))
      } else {
        val off = frame.incomingArgOffset(i)
        val size = t.len.convertToBytes(A32_WORD_SIZE)
        val immOffsetAddr: A32MemAddress = MemAddress.ImmOffsetAddress(FP, off, NoIndex)
        if (size == BoolCharSize) gr.emit(Ldrb(scratch, immOffsetAddr))
        else gr.emit(Ldr(scratch, immOffsetAddr))
        frame.storeTemp(t, scratch)
      }
    }
  }

  // Emit function information
  private def emitFunctionInto(f: TACFunction)(using gr: GenRes): Unit = {
    val frame = new A32StackFrame(f.locals)
    given cs: A32CodegenState = new A32CodegenState(frame)

    gr.emit(Label(f.name.name))
    frame.addFrame().foreach(gr.emit)

    initParams(frame, f.params)
    f.body.foreach(genA32Instr)

    gr.emitLiteralPool() // Drops a pool after every function
  }
}
