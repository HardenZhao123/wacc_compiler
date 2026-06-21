package wacc.backend.AArch64.codeGen

import scala.collection.mutable

import wacc.backend.BackendCommon.*
import wacc.backend.*
import A64CodeGenConstant.*
import A64Instr.*
import wacc.backend.midir.TAC
import wacc.backend.midir.TAC.*
import wacc.backend.AArch64.target.Reg.*
import wacc.backend.AArch64.target.StackFrameConstant.{ARG_REG_COUNT, STACK_ALIGNMENT_BYTES, STACK_SLOT_BYTES}
import wacc.backend.AArch64.target.*
import wacc.backend.AArch64.preDefFunctions.PreDefRunTimeError
import wacc.backend.AArch64.AArch64Constants.*
import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.fromTACCondOpToCond.*
import wacc.backend.BackendCommon.AsmEntity.*

object A64Generator {

  private val ExceptionFlagLabel = "wacc_exception_flag"
  private val ExceptionValueLabel = "wacc_exception_value"

  // Generates the full AArch64 assembly program, including data, functions, and helpers.
  def genA64Program(program: TACProgram): List[A64Instr] = {
    val strs = program.strs.zipWithIndex.map { case (str, id) =>
      val label = Label(s"$STRING_LABEL_PREFIX$id")
      StringData(label, str)
    }

    val globals = Seq(
      WordData(Label(ExceptionFlagLabel), 0),
      WordData(Label(ExceptionValueLabel), 0)
    )

    val header: List[A64Instr] = List(
      DataSeg(strs ++ globals),
      Raw(BackendConstant.ASM_DIRECTIVE_ALIGN_4),
      Raw(ASM_DIRECTIVE_TEXT),
      Raw(ASM_DIRECTIVE_GLOBAL_MAIN)
    )

    // MAIN FUNCTION
    val mainFrame = new StackFrame(program.locals)

    given cs: A64CodegenState = new A64CodegenState(mainFrame)
    given gr: GenRes = new GenRes()

    gr.emit(Label(LABEL_MAIN))
    mainFrame.addFrame().foreach(gr.emit)

    program.body.foreach(genA64Instr(_)(using cs, gr))

    gr.emit(Mov(RETURN_REG_64, Immediate(0)))
    mainFrame.removeFrame().foreach(gr.emit)

    // FUNCTIONS
    program.funcs.foreach { f =>
      emitFunctionInto(f)(using gr)
    }

    val result: mutable.Builder[A64Instr, List[A64Instr]] = List.newBuilder
    result ++= header
    result ++= gr.instrs
    result ++= gr.preDefsErrs
    result.result()
  }

  // Lowers one TAC instruction into AArch64 instructions appended to the current result.
  def genA64Instr(instr: TAC.Instr)(using cs: A64CodegenState, gr: GenRes): Unit = instr match {

    case TACExit(code) =>
      cs.withEval(code) { reg =>
        gr.emit(Mov(RETURN_REG_64, reg))
        gr.emit(Bl(Label(LIBC_EXIT_SYMBOL)))
      }

    // skip is a no-op statement: generate nothing
    case TACSkip() => ()

    case Mark(l) => gr.emit(Label(l.name))

    case Jmp(to) => gr.emit(B(Label(to.name)))

    case CmpJmp(cond, lhs, rhs, to) =>
      cs.withEval2(lhs, rhs) { (reg1, reg2) =>
        gr.emit(Cmp(reg1, reg2))
        gr.emit(BCond(toCond(cond), Label(to.name)))
      }

    // Return from function
    case TACReturn(v) =>
      val wantX = v.len == BitLength._ptr || v.len == BitLength._64
      val dst = if (wantX) RETURN_REG_64 else RETURN_REG_32
      cs.frame.loadRhs(v, dst, cs.ra)
      cs.frame.removeFrame().foreach(gr.emit)

    case TACStoreException(value) =>
      cs.withEval(value) { valueReg =>
        storeGlobalWord(ExceptionFlagLabel, Immediate(1))
        storeGlobalWord(ExceptionValueLabel, valueReg)
      }

    case TACLoadExceptionFlag(dst) =>
      loadGlobalWordInto(dst, ExceptionFlagLabel)

    case TACLoadExceptionValue(dst) =>
      loadGlobalWordInto(dst, ExceptionValueLabel)

    case TACClearException() =>
      storeGlobalWord(ExceptionFlagLabel, Immediate(0))
      storeGlobalWord(ExceptionValueLabel, Immediate(0))

    case TACReportError(exPtr) =>
      cs.frame.loadRhs(exPtr, RETURN_REG_64, cs.ra)
      gr.emit(Bl(Label(_ERR_UNHANDLED)))
      gr.addPreDefs(_ERR_UNHANDLED)
      gr.addPreDefs(_PRINTS)
      gr.addPreDefs(_PRINTLN)

    case CheckedBinOp(dst, op, lhs, rhs, onOverflow) =>
      op match {
        case TAC.ArithOp.Mul =>
          emitCheckedMul(dst, lhs, rhs, onOverflow)

        case _ =>
          cs.withEval2(lhs, rhs) { (lreg, rreg) =>
            val lregW = toW(lreg)
            val rregW = toW(rreg)

            op match {
              case TAC.ArithOp.Add => emitCheckedOverflowInstr(Adds.apply, lregW, rregW, onOverflow)
              case TAC.ArithOp.Sub => emitCheckedOverflowInstr(Subs.apply, lregW, rregW, onOverflow)
              case unsupported      => throw new IllegalArgumentException(s"Unsupported checked arithmetic op: $unsupported")
            }

            cs.frame.storeTemp(dst, lreg, cs.ra)
          }
      }

    case CheckedUnOp(dest, op, x, onOverflow) =>
      cs.withEval(x) { reg =>
        val regW = toW(reg)

        op match {
          case TAC.UnaryOp.Neg => emitCheckedNegBitNot(Negs.apply, regW, onOverflow)
          case unsupported     => throw new IllegalArgumentException(s"Unsupported checked unary op: $unsupported")
        }

        cs.frame.storeTemp(dest, reg, cs.ra)
      }

    case IntToFloat(dst, value) =>
      cs.withEval(value) { reg =>
        val valueW = toW(reg)
        val converted = S(0)
        gr.emit(SCvtf(converted, valueW))
        gr.emit(FMov(valueW, converted))
        cs.frame.storeTemp(dst, valueW, cs.ra)
      }

    case BinOp(dst, op, lhs, rhs) =>
      op match {
        case f: TAC.FloatArithOp =>
          emitFloatArithmetic(dst, f, lhs, rhs)

        case c: TAC.FloatCondOp =>
          emitFloatComparison(dst, c, lhs, rhs)

        case TAC.ArithOp.Mul =>
          emitMul(dst, lhs, rhs)

        case _ =>
          cs.withEval2(lhs, rhs) { (lreg, rreg) =>
            val lregW = toW(lreg)
            val rregW = toW(rreg)

            op match {
              case TAC.ArithOp.Add => emitOverflowInstr(Adds.apply, lregW, rregW)
              case TAC.ArithOp.Sub => emitOverflowInstr(Subs.apply, lregW, rregW)

              case TAC.ArithOp.Div => emitDivInstr(SDiv.apply, lreg, rreg)
              case TAC.ArithOp.Mod => emitModInstr(lreg, rreg)

              case TAC.BoolOp.And => gr.emit(And(lregW, lregW, rregW))
              case TAC.BoolOp.Or  => gr.emit(Orr(lregW, lregW, rregW))

              case TAC.CondOp.GT =>
                gr.emit(Cmp(lregW, rregW))
                gr.emit(Cset(lregW, Cond.GT))

              case TAC.BitwiseOp.BitAnd => emitOverflowInstr(And.apply, lregW, rregW)
              case TAC.BitwiseOp.BitOr => emitOverflowInstr(Orr.apply, lregW, rregW)

              case op =>
                gr.emit(Cmp(lregW, rregW))
                val cond = op match {
                  case TAC.CondOp.EQ  => Cond.EQ
                  case TAC.CondOp.NEQ => Cond.NE
                  case TAC.CondOp.LT  => Cond.LT
                  case TAC.CondOp.LEQ => Cond.LE
                  case TAC.CondOp.GEQ => Cond.GE
                  case _              => throw new RuntimeException(ERR_UNREACHABLE)
                }
                gr.emit(Cset(lregW, cond))
            }

            cs.frame.storeTemp(dst, lreg, cs.ra)
          }
      }

    // Unary operations
    case TAC.UnOp(dest, op, x) =>
      cs.withEval(x) { reg =>
        val regW = toW(reg)

        op match {
          case TAC.UnaryOp.Neg => emitNegBitNot(Negs.apply, regW)

          case TAC.UnaryOp.Not =>
            gr.emit(Cmp(regW, Immediate(1)))
            gr.emit(Cset(regW, Cond.NE))

          case TAC.UnaryOp.Ord => ()

          case TAC.UnaryOp.Chr =>
            val badCharLabel = Label(PreDefRunTimeError.ErrBadCharLabel)
            gr.emit(Cmp(regW, Immediate(ASCII_MIN)))
            gr.emit(BCond(Cond.LT, badCharLabel))
            gr.emit(Cmp(regW, Immediate(ASCII_MAX)))
            gr.emit(BCond(Cond.GT, badCharLabel))

            gr.addUsedErrs(PreDefRunTimeError.ErrBadCharLabel)

          case TAC.UnaryOp.BitNot => emitNegBitNot(Mvn.apply, regW)

          case TAC.UnaryOp.Len =>
            // len expects a pointer array
            // Runtime: layout is [len:int32][padding] then element0 pointer = base + ARR_HDR.
            // Here base is elem0 ptr, so len is at (base - ARR_HDR).
            val xBase: A64Reg = reg match {
              case xr: X => xr
              case wr: W => toX(wr)
              case other => throw new IllegalArgumentException(s"len expects pointer reg, got $other")
            }

            // null check
            gr.emit(Cmp(xBase, Immediate(0)))
            gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)))
            gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)

            // load length (int32) from header
            val wRes: A64Reg = toW(xBase)
            gr.emit(Ldr(wRes, MemAddress.ImmOffsetAddress(xBase, -ARR_HDR, NoIndex)))
        }

        cs.frame.storeTemp(dest, reg, cs.ra)
      }

    // Println
    case PrintLn() =>
      gr.emit(Bl(Label(_PRINTLN)))
      gr.addPreDefs(_PRINTLN)

    // Prints
    case print: Print =>
      cs.withEval(print.content) { reg =>

        gr.emit(Mov(RETURN_REG_64, reg))
        print match {
          case _: PrintInt     => emitPrint(_PRINTI)
          case _: PrintBool    => emitPrint(_PRINTB)
          case _: PrintChar    => emitPrint(_PRINTC)
          case _: PrintFloat   => emitPrint(_PRINTFL)
          case _: PrintStr     => emitPrint(_PRINTS)
          case _: PrintPointer => emitPrint(_PRINTP)
        }
      }

    case TACRead(dst, t) =>
      // load current value (default) into w0
      // even for char we still use w0; _readc returns in x0 but w0 low bits ok
      cs.frame.loadRhs(dst, RETURN_REG_32, cs.ra)

      // call helper; it returns the read value in x0/w0
      t match {
        case TAC.ReadType.Int =>
          gr.emit(Bl(Label(_READI)))
          gr.addPreDefs(_READI)
        case TAC.ReadType.Char =>
          gr.emit(Bl(Label(_READC)))
          gr.addPreDefs(_READC)
        case TAC.ReadType.Float =>
          gr.emit(Bl(Label(_READFL)))
          gr.addPreDefs(_READFL)
      }

      //store result back
      cs.frame.storeTemp(dst, RETURN_REG_32, cs.ra)

    // Free memory
    case TACFree(x, isPair) =>
      cs.withEval(x) { reg =>
        gr.emit(Mov(RETURN_REG_64, reg))

        if (isPair) {
          gr.emit(Bl(Label(_FREEPAIR)))
          gr.addPreDefs(_FREEPAIR)
          gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
        } else {
          gr.emit(Bl(Label(_FREEARR)))
          gr.addPreDefs(_FREEARR)
        }
      }

    // Function call
    case TACCall(dst, func, args) =>
      val stackBytes = emitCallArgs(args)
      gr.emit(Bl(Label(func.name)))
      if (stackBytes > 0) gr.emit(Add(SP, SP, Immediate(stackBytes.toLong)))

      val wantX = dst.len == TAC.BitLength._ptr || dst.len == TAC.BitLength._64
      if (wantX) cs.frame.storeTemp(dst, RETURN_REG_64, cs.ra)
      else cs.frame.storeTemp(dst, RETURN_REG_32, cs.ra)

    case TACAssign(lhs, rhs) => cs.withEval(rhs) { reg => cs.frame.storeTemp(lhs, reg, cs.ra) }
  }

  // Materialises the address of a global word label into a scratch register.
  private def withGlobalAddress[A](labelName: String)(f: X => A)(using cs: A64CodegenState, gr: GenRes): A =
    cs.ra.withNewRegister { addrReg =>
      val addrX: X = addrReg match {
        case x: X => x
        case w: W => toX(w).asInstanceOf[X]
        case _    => throw new Exception("Special registers cannot be allocated for temporaries")
      }
      val label = Label(labelName)
      gr.emit(Adrp(addrX, label))
      gr.emit(Add(addrX, addrX, MaskedLabel(LO_12, label)))
      f(addrX)
    }

  // Stores one machine word into a global slot.
  private def storeGlobalWord(labelName: String, value: A64Operand)(using cs: A64CodegenState, gr: GenRes): Unit =
    withGlobalAddress(labelName) { addr =>
      cs.ra.withNewRegister { dataReg =>
        gr.emit(Mov(dataReg, value))
        gr.emit(Str(dataReg, MemAddress.BaseRegAddress(addr)))
      }
    }

  // Loads one machine word from a global slot and stores it into a TAC temporary.
  private def loadGlobalWordInto(dst: TAC.Temp, labelName: String)(using cs: A64CodegenState, gr: GenRes): Unit =
    withGlobalAddress(labelName) { addr =>
      cs.ra.withNewRegister { dataReg =>
        gr.emit(Ldr(dataReg, MemAddress.BaseRegAddress(addr)))
        cs.frame.storeTemp(dst, dataReg, cs.ra)
      }
    }

  private def emitFloatArithmetic(dst: TAC.Temp, op: TAC.FloatArithOp, lhs: TAC.Rhs, rhs: TAC.Rhs)
                                 (using cs: A64CodegenState, gr: GenRes): Unit =
    cs.withEval2(lhs, rhs) { (lhsReg, rhsReg) =>
      val lhsW = toW(lhsReg)
      val rhsW = toW(rhsReg)
      val result = S(0)
      val operand = S(1)

      gr.emit(FMov(result, lhsW))
      gr.emit(FMov(operand, rhsW))
      op match {
        case TAC.FloatArithOp.Add => gr.emit(FAdd(result, result, operand))
        case TAC.FloatArithOp.Sub => gr.emit(FSub(result, result, operand))
        case TAC.FloatArithOp.Mul => gr.emit(FMul(result, result, operand))
        case TAC.FloatArithOp.Div => gr.emit(FDiv(result, result, operand))
      }
      gr.emit(FMov(lhsW, result))
      cs.frame.storeTemp(dst, lhsW, cs.ra)
    }

  private def emitFloatComparison(dst: TAC.Temp, op: TAC.FloatCondOp, lhs: TAC.Rhs, rhs: TAC.Rhs)
                                 (using cs: A64CodegenState, gr: GenRes): Unit =
    cs.withEval2(lhs, rhs) { (lhsReg, rhsReg) =>
      val lhsW = toW(lhsReg)
      val left = S(0)
      val right = S(1)

      gr.emit(FMov(left, lhsW))
      gr.emit(FMov(right, toW(rhsReg)))
      gr.emit(FCmp(left, right))

      val cond = op match {
        case TAC.FloatCondOp.EQ => Cond.EQ
        case TAC.FloatCondOp.NEQ => Cond.NE
        case TAC.FloatCondOp.LT => Cond.MI
        case TAC.FloatCondOp.LEQ => Cond.LS
        case TAC.FloatCondOp.GT => Cond.GT
        case TAC.FloatCondOp.GEQ => Cond.GE
      }
      gr.emit(Cset(lhsW, cond))
      cs.frame.storeTemp(dst, lhsW, cs.ra)
    }

  // Rounds stack allocation size up to the AArch64 16-byte alignment rule.
  private def align16(n: Int): Int = {
    val r = n % STACK_ALIGNMENT_BYTES
    if (r == 0) n else n + (STACK_ALIGNMENT_BYTES - r)
  }

  // Emit code to pass function call arguments according to AArch64 calling conventions.
  // Arguments are placed in registers first, then spilled to the stack if necessary.
  private def emitCallArgs(args: List[TAC.Rhs])(using cs: A64CodegenState, gr: GenRes): Int = {
    val n = args.length
    val nStack = math.max(0, n - ARG_REG_COUNT)
    val stackBytes = align16(nStack * ARG_REG_COUNT)

    if (stackBytes > 0) gr.emit(Sub(SP, SP, Immediate(stackBytes.toLong)))

    // Store overflow arguments to stack
    for (i <- ARG_REG_COUNT until n) {
      val arg = args(i)
      cs.ra.withNewRegister { tmpX =>
        cs.frame.loadRhs(arg, tmpX, cs.ra)
        val off = STACK_SLOT_BYTES  * (i - ARG_REG_COUNT)
        gr.emit(A64Instr.Str(tmpX, MemAddress.ImmOffsetAddress(SP, off, NoIndex)))
      }
    }

    // Load the first few arguments into argument registers
    for (i <- 0 until math.min(ARG_REG_COUNT, n)) {
      val arg = args(i)
      val wantX = arg.len == TAC.BitLength._ptr || arg.len == TAC.BitLength._64

      if (wantX) cs.frame.loadRhs(arg, X(i), cs.ra)
      else cs.frame.loadRhs(arg, W(i), cs.ra)
    }

    stackBytes
  }

  // Initialize function parameters at the start of a function.
  private def initParams(frame: StackFrame, params: List[TAC.Temp])
                        (using cs: A64CodegenState, gr: GenRes): Unit = {
    val scratch = SCRATCH_REG_START

    // Iterate over each parameter and its index
    for ((t, i) <- params.zipWithIndex) {
      if (i < ARG_REG_COUNT) {
        // Parameter passed in a register (X0-X7)
        // Store it into the corresponding temporary in the frame
        frame.storeTemp(t, X(i), cs.ra)
      } else {
        // Parameter passed on the stack (overflow)
        // Compute its offset in the stack frame
        val off = frame.incomingArgOffset(i)
        gr.emit(Ldr(scratch, MemAddress.ImmOffsetAddress(FP, off, NoIndex)))
        frame.storeTemp(t, scratch, cs.ra)
      }
    }
  }

  // Emit AArch64 instructions for a single TAC function.
  private def emitFunctionInto(f: TACFunction)(using gr: GenRes): Unit = {
    val frame = new StackFrame(f.locals)

    given cs: A64CodegenState = new A64CodegenState(frame)

    gr.emit(Label(f.name.name))
    frame.addFrame().foreach(gr.emit)
    initParams(frame, f.params)

    f.body.foreach(genA64Instr)
  }

  private def emitGenericMul(dst: TAC.Temp, lhs: TAC.Rhs, rhs: TAC.Rhs)
                            (using cs: A64CodegenState, gr: GenRes): Unit = {
    cs.ra.withNewRegisters2 { (lhsReg, rhsReg) =>
      val xL = lhsReg match {
        case x: X => x
        case w: W => toX(w).asInstanceOf[X]
        case _    => throw new Exception("Special registers cannot be allocated for temporaries")
      }
      val wL = toW(lhsReg)
      val wR = toW(rhsReg)

      cs.frame.loadRhs(lhs, wL, cs.ra)
      cs.frame.loadRhs(rhs, wR, cs.ra)

      gr.emit(SMull(xL, wL, wR))
      gr.emit(Cmp(xL, wL, true))
      gr.emit(BCond(Cond.NE, Label(PreDefRunTimeError.ErrOverflowLabel)))
      markOverflowHelperUsed()

      cs.frame.storeTemp(dst, wL, cs.ra)
    }
  }

  // Extracts an in-range 32-bit immediate integer from a TAC RHS when possible.
  private def extractImmInt(rhs: TAC.Rhs): Option[Int] = rhs match {
    case TAC.ImmValue(v, TAC.BitLength._32)
      if v >= Int.MinValue && v <= Int.MaxValue => Some(v.toInt)
    case _ => None
  }

  // Recognises multiplication constants that have specialised lowering patterns.
  private def extractMulConst(rhs: TAC.Rhs): Option[MulConst] =
    extractImmInt(rhs).flatMap(MulConst.fromInt)

  // Emits checked multiply code that branches to a caller-provided overflow label.
  private def emitCheckedGenericMul(dst: TAC.Temp, lhs: TAC.Rhs, rhs: TAC.Rhs, onOverflow: wacc.backend.label.Label)
                                    (using cs: A64CodegenState, gr: GenRes): Unit = {
    cs.ra.withNewRegisters2 { (lhsReg, rhsReg) =>
      val xL = lhsReg match {
        case x: X => x
        case w: W => toX(w).asInstanceOf[X]
        case _    => throw new Exception("Special registers cannot be allocated for temporaries")
      }
      val wL = toW(lhsReg)
      val wR = toW(rhsReg)

      cs.frame.loadRhs(lhs, wL, cs.ra)
      cs.frame.loadRhs(rhs, wR, cs.ra)

      gr.emit(SMull(xL, wL, wR))
      gr.emit(Cmp(xL, wL, true))
      gr.emit(BCond(Cond.NE, Label(onOverflow.name)))

      cs.frame.storeTemp(dst, wL, cs.ra)
    }
  }

  // Marks the shared overflow runtime helper as required by the current function.
  private def markOverflowHelperUsed()(using gr: GenRes): Unit = {
    gr.addPreDefs(_PRINTS)
    gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)
  }

  // Emits a branch to a caller-provided overflow label when the V flag is set.
  private def emitCheckedOverflowBranch(onOverflow: wacc.backend.label.Label)(using gr: GenRes): Unit = {
    gr.emit(BCond(Cond.VS, Label(onOverflow.name)))
  }

  // Emits a branch to the shared overflow runtime helper when the V flag is set.
  private def emitOverflowBranch()(using gr: GenRes): Unit = {
    gr.emit(BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)))
    markOverflowHelperUsed()
  }

  // Lowers checked multiplication, specialising small constant factors when available.
  private def emitCheckedMul(dst: TAC.Temp, lhs: TAC.Rhs, rhs: TAC.Rhs, onOverflow: wacc.backend.label.Label)
                            (using cs: A64CodegenState, gr: GenRes): Unit = {
    (extractMulConst(lhs), extractMulConst(rhs)) match {
      case (Some(mulConst), None) =>
        cs.withEval(rhs) { srcReg =>
          emitCheckedMulByConstInto(dst, srcReg, mulConst, onOverflow)
        }

      case (None, Some(mulConst)) =>
        cs.withEval(lhs) { srcReg =>
          emitCheckedMulByConstInto(dst, srcReg, mulConst, onOverflow)
        }

      case _ =>
        emitCheckedGenericMul(dst, lhs, rhs, onOverflow)
    }
  }

  // Lowers unchecked multiplication, specialising small constant factors when available.
  private def emitMul(dst: TAC.Temp, lhs: TAC.Rhs, rhs: TAC.Rhs)
                     (using cs: A64CodegenState, gr: GenRes): Unit = {
    (extractMulConst(lhs), extractMulConst(rhs)) match {
      case (Some(mulConst), None) =>
        cs.withEval(rhs) { srcReg =>
          emitMulByConstInto(dst, srcReg, mulConst)
        }

      case (None, Some(mulConst)) =>
        cs.withEval(lhs) { srcReg =>
          emitMulByConstInto(dst, srcReg, mulConst)
        }

      case _ =>
        emitGenericMul(dst, lhs, rhs)
    }
  }

  // Emits overflow-checked multiplication by a recognised small constant.
  private def emitCheckedMulByConstInto(dst: TAC.Temp, srcReg: A64Reg, mulConst: MulConst, onOverflow: wacc.backend.label.Label)
                                       (using cs: A64CodegenState, gr: GenRes): Unit = {
    val wSrc = toW(srcReg)

    mulConst match {
      case MulConst.Zero =>
        gr.emit(Mov(wSrc, Immediate(0)))
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.One =>
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.NegOne =>
        gr.emit(Negs(wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.Two =>
        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.Three =>
        cs.ra.withNewRegister { tmp =>
          val wTmp = toW(tmp)

          gr.emit(Adds(wTmp, wSrc, wSrc))
          emitCheckedOverflowBranch(onOverflow)

          gr.emit(Adds(wSrc, wTmp, wSrc))
          emitCheckedOverflowBranch(onOverflow)

          cs.frame.storeTemp(dst, srcReg, cs.ra)
        }

      case MulConst.Four =>
        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)

        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)

        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.Five =>
        cs.ra.withNewRegister { tmp =>
          val wTmp = toW(tmp)

          gr.emit(Adds(wTmp, wSrc, wSrc))
          emitCheckedOverflowBranch(onOverflow)

          gr.emit(Adds(wTmp, wTmp, wTmp))
          emitCheckedOverflowBranch(onOverflow)

          gr.emit(Adds(wSrc, wTmp, wSrc))
          emitCheckedOverflowBranch(onOverflow)

          cs.frame.storeTemp(dst, srcReg, cs.ra)
        }

      case MulConst.Eight =>
        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)

        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)

        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitCheckedOverflowBranch(onOverflow)

        cs.frame.storeTemp(dst, srcReg, cs.ra)
    }
  }

  // Emits multiplication by a recognised small constant using add/neg sequences.
  private def emitMulByConstInto(dst: TAC.Temp, srcReg: A64Reg, mulConst: MulConst)
                                (using cs: A64CodegenState, gr: GenRes): Unit = {
    val wSrc = toW(srcReg)

    mulConst match {
      case MulConst.Zero =>
        gr.emit(Mov(wSrc, Immediate(0)))
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.One =>
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.NegOne =>
        gr.emit(Negs(wSrc, wSrc))
        emitOverflowBranch()
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.Two =>
        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitOverflowBranch()
        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.Three =>
        cs.ra.withNewRegister { tmp =>
          val wTmp = toW(tmp)

          gr.emit(Adds(wTmp, wSrc, wSrc))
          emitOverflowBranch()

          gr.emit(Adds(wSrc, wTmp, wSrc))
          emitOverflowBranch()

          cs.frame.storeTemp(dst, srcReg, cs.ra)
        }

      case MulConst.Four =>
        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitOverflowBranch()

        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitOverflowBranch()

        cs.frame.storeTemp(dst, srcReg, cs.ra)

      case MulConst.Five =>
        cs.ra.withNewRegister { tmp =>
          val wTmp = toW(tmp)

          gr.emit(Adds(wTmp, wSrc, wSrc))
          emitOverflowBranch()

          gr.emit(Adds(wTmp, wTmp, wTmp))
          emitOverflowBranch()

          gr.emit(Adds(wSrc, wTmp, wSrc))
          emitOverflowBranch()

          cs.frame.storeTemp(dst, srcReg, cs.ra)
        }

      case MulConst.Eight =>
        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitOverflowBranch()

        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitOverflowBranch()

        gr.emit(Adds(wSrc, wSrc, wSrc))
        emitOverflowBranch()

        cs.frame.storeTemp(dst, srcReg, cs.ra)
    }
  }

  // Emits a checked binary instruction and branches to the supplied overflow label on overflow.
  private def emitCheckedOverflowInstr(instr: (A64Reg, A64Reg, A64Reg) => A64Instr, l: A64Reg, r: A64Reg, onOverflow: wacc.backend.label.Label)
                                      (using gr: GenRes): Unit = {
    gr.emit(instr(l, l, r))
    gr.emit(BCond(Cond.VS, Label(onOverflow.name)))
  }

  // Emits a checked unary instruction and branches to the supplied overflow label on overflow.
  private def emitCheckedNegBitNot(instr: (A64Reg, A64Reg) => A64Instr, reg: A64Reg, onOverflow: wacc.backend.label.Label)
                                 (using gr: GenRes): Unit = {
    gr.emit(instr(reg, reg))
    gr.emit(BCond(Cond.VS, Label(onOverflow.name)))
  }

  // Emits an overflow-checked arithmetic instruction
  private def emitOverflowInstr(instr: (A64Reg, A64Reg, A64Reg) => A64Instr, l: A64Reg, r: A64Reg)
                               (using gr: GenRes): Unit = {
    gr.emit(instr(l, l, r))
    gr.emit(BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)))
    gr.addPreDefs(_PRINTS)
    gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)
  }

  // Emits a divide or modulo with zero-check
  private def emitDivInstr(divInstr: (A64Reg, A64Reg, A64Reg) => A64Instr, l: A64Reg, r: A64Reg)
                          (using gr: GenRes): Unit = {
    val wL = toW(l)
    val wR = toW(r)
    gr.emit(Cmp(wR, Immediate(0)))
    gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrDivZeroLabel)))
    gr.emit(divInstr(wL, wL, wR))
    gr.addPreDefs(_PRINTS)
    gr.addUsedErrs(PreDefRunTimeError.ErrDivZeroLabel)
  }

  // Emits modulo using SDiv + MSub
  private def emitModInstr(l: A64Reg, r: A64Reg)
                          (using cs: A64CodegenState, gr: GenRes): Unit = {
    cs.ra.withNewRegister { tmp =>
      val wL = toW(l)
      val wR = toW(r)
      val wTmp = toW(tmp)

      gr.emit(Cmp(wR, Immediate(0)))
      gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrDivZeroLabel)))

      gr.emit(SDiv(wTmp, wL, wR))
      gr.emit(MSub(wL, wTmp, wR, wL))

      gr.addPreDefs(_PRINTS)
      gr.addUsedErrs(PreDefRunTimeError.ErrDivZeroLabel)
    }
  }

  private def emitPrint(printLabel: String)(using gr: GenRes): Unit = {
    gr.emit(Bl(Label(printLabel)))
    gr.addPreDefs(printLabel)
  }

  // Emits a unary negate/bitwise-not instruction followed by the shared overflow trap.
  private def emitNegBitNot(instr: (A64Reg, A64Reg) => A64Instr, reg: A64Reg)
                           (using gr: GenRes): Unit = {
    gr.emit(instr(reg, reg))
    gr.emit(BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)))
    gr.addPreDefs(_PRINTS)
    gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)
  }
}
