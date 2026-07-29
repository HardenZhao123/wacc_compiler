package wacc.backend.X86.codeGen

import wacc.backend.midir.TAC.*
import wacc.backend.midir.TAC
import wacc.backend.X86.target.X86StackFrame
import wacc.backend.X86.target.*
import wacc.backend.X86.target.X86CallingConvention.argRegisters
import wacc.backend.X86.X86Constants.*
import wacc.backend.X86.codeGen.X86Instr.*
import wacc.backend.X86.preDefFunctions.PreDefRunTimeError
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.BackendConstant.*

/** Converts TACProgram and TAC instructions into x86-64 assembly (X86Instr). */
object X86Generator extends CommonGenerator[X86Instr, X86StackFrame, X86CodegenState, GenRes] {

  override protected def directivesBeforeText: Seq[AsmEntity] =
    Seq(Raw(".intel_syntax noprefix"))

  override protected def exceptionGlobals: Seq[DataItem] =
    Seq(
      WordData(Label(ExceptionFlagLabel), 0),
      QuadData(Label(ExceptionValueLabel), 0)
    )

  def genX86Program(program: TACProgram): List[X86Instr] = genProgram(program)

  override protected def newFrame(locals: Seq[Temp]): X86StackFrame =
    new X86StackFrame(locals)

  override protected def newCodegenState(frame: X86StackFrame): X86CodegenState =
    new X86CodegenState(frame)

  override protected def newGenRes(): GenRes = new GenRes()

  override protected def addFrame(frame: X86StackFrame)(using X86CodegenState): List[X86Instr] =
    frame.addFrame()

  override protected def emitMainReturn(frame: X86StackFrame)(using X86CodegenState, GenRes): Unit = {
    summon[GenRes].emit(Mov(Reg32(RETURN_REG), Immediate(0)))
    frame.removeFrame().foreach(summon[GenRes].emit)
  }

  override protected def genInstr(instr: TAC.Instr)(using X86CodegenState, GenRes): Unit =
    genX86Instr(instr)

  def genX86Instr(instr: TAC.Instr)(using cs: X86CodegenState, gr: GenRes): Unit = instr match {

    // Exit
    case TACExit(code) =>
      cs.frame.loadRhs(code, FIRST_ARG_REG, cs.ra)
      gr.emit(Call(Label(_EXIT)))
      gr.addPreDefs(_EXIT)

    case TACSkip() => ()

    case Mark(l) => gr.emit(Label(l.name))

    case TAC.Jmp(to) => gr.emit(X86Instr.Jmp(Label(to.name)))

    case CmpJmp(cond, lhs, rhs, to) =>
      cs.withEval2(lhs, rhs) { (lreg, rreg) =>
        gr.emit(Cmp(cmpOperand(lreg, lhs.len, rhs.len), cmpOperand(rreg, lhs.len, rhs.len)))
        gr.emit(Jcc(toX86Cond(cond), Label(to.name)))
      }

    case TACReturn(v) =>
      cs.frame.loadRhs(v, RETURN_REG, cs.ra)
      cs.frame.removeFrame().foreach(gr.emit)

    case TACStoreException(value) =>
      cs.withEval(value) { valueReg =>
        storeGlobal(ExceptionFlagLabel, X86Size.DWord, Immediate(1))
        storeGlobal(ExceptionValueLabel, X86Size.QWord, Reg64(valueReg))
      }

    case TACLoadExceptionFlag(dst) =>
      cs.ra.withNewRegister { scratch =>
        gr.emit(Mov(Reg32(scratch), globalMem(ExceptionFlagLabel, X86Size.DWord)))
        cs.frame.storeTemp(dst, scratch)
      }

    case TACLoadExceptionValue(dst) =>
      cs.ra.withNewRegister { scratch =>
        gr.emit(Mov(Reg64(scratch), globalMem(ExceptionValueLabel, X86Size.QWord)))
        cs.frame.storeTemp(dst, scratch)
      }

    case TACClearException() =>
      storeGlobal(ExceptionFlagLabel, X86Size.DWord, Immediate(0))
      storeGlobal(ExceptionValueLabel, X86Size.QWord, Immediate(0))

    case TACReportError(exPtr) =>
      cs.frame.loadRhs(exPtr, FIRST_ARG_REG, cs.ra)
      gr.emit(Call(Label(_ERR_UNHANDLED)))
      gr.addPreDefs(_ERR_UNHANDLED)
      gr.addPreDefs(_PRINTS)
      gr.addPreDefs(_PRINTLN)

    case TAC.CheckedBinOp(dst, op, lhs, rhs, onOverflow) =>
      emitCheckedBinOp(dst, op, lhs, rhs, Label(onOverflow.name))

    case TAC.CheckedUnOp(dest, op, x, onOverflow) =>
      emitCheckedUnOp(dest, op, x, Label(onOverflow.name))

    case TAC.IntToFloat(dst, value) =>
      cs.withEval(value) { reg =>
        gr.emit(Cvtsi2ss(XMM(0), Reg32(reg)))
        gr.emit(Movd(Reg32(reg), XMM(0)))
        cs.frame.storeTemp(dst, reg)
      }

    case TAC.BinOp(dst, op, lhs, rhs) =>
      emitBinOp(dst, op, lhs, rhs)

    case TAC.UnOp(dest, op, x) =>
      emitUnOp(dest, op, x)

    case PrintLn() =>
      gr.emit(Call(Label(_PRINTLN)))
      gr.addPreDefs(_PRINTLN)

    case print: Print =>
      cs.frame.loadRhs(print.content, FIRST_ARG_REG, cs.ra)
      print match {
        case _: PrintInt     => emitPrint(_PRINTI)
        case _: PrintBool    => emitPrint(_PRINTB)
        case _: PrintChar    => emitPrint(_PRINTC)
        case _: PrintFloat   => emitPrint(_PRINTFL)
        case _: PrintStr     => emitPrint(_PRINTS)
        case _: PrintPointer => emitPrint(_PRINTP)
      }

    case TACRead(dest, t) =>
      cs.frame.loadRhs(dest, FIRST_ARG_REG, cs.ra)
      t match {
        case TAC.ReadType.Int =>
          gr.emit(Call(Label(_READI)))
          gr.addPreDefs(_READI)
        case TAC.ReadType.Char =>
          gr.emit(Call(Label(_READC)))
          gr.addPreDefs(_READC)
        case TAC.ReadType.Float =>
          gr.emit(Call(Label(_READFL)))
          gr.addPreDefs(_READFL)
      }
      cs.frame.storeTemp(dest, RETURN_REG)

    case TACFree(x, isPair) =>
      cs.withEval(x) { reg =>
        if (isPair) {
          gr.emit(Mov(Reg64(FIRST_ARG_REG), Reg64(reg)))
          gr.emit(Call(Label(_FREEPAIR)))
          gr.addPreDefs(_FREEPAIR)
          gr.addPreDefs(_PRINTS)
          gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
        } else {
          gr.emit(Sub(Reg64(reg), Immediate(ARR_HDR.toLong)))
          gr.emit(Mov(Reg64(FIRST_ARG_REG), Reg64(reg)))
          gr.emit(Call(Label(_FREE)))
          gr.addPreDefs(_FREE)
        }
      }

    case TACCall(dest, func, args) =>
      val stackBytes = emitCallArgs(args)
      gr.emit(Call(Label(func.name)))
      if (stackBytes > 0) gr.emit(Add(Reg64(RSP), Immediate(stackBytes.toLong)))
      cs.frame.storeTemp(dest, RETURN_REG)

    case TACAssign(lhs, rhs) =>
      cs.withEval(rhs) { reg => cs.frame.storeTemp(lhs, reg) }
  }

  private def emitCheckedBinOp(dst: Temp, op: ArithOp, lhs: Rhs, rhs: Rhs, onOverflow: Label)
                              (using cs: X86CodegenState, gr: GenRes): Unit =
    op match {
      case ArithOp.Add | ArithOp.Sub | ArithOp.Mul =>
        cs.withEval2(lhs, rhs) { (lreg, rreg) =>
          op match {
            case ArithOp.Add => gr.emit(Add(Reg32(lreg), Reg32(rreg)))
            case ArithOp.Sub => gr.emit(Sub(Reg32(lreg), Reg32(rreg)))
            case ArithOp.Mul => gr.emit(Imul(Reg32(lreg), Reg32(rreg)))
            case _           => throw new IllegalArgumentException(s"unsupported checked arithmetic op: $op")
          }
          gr.emit(Jcc(X86Cond.O, onOverflow))
          cs.frame.storeTemp(dst, lreg)
        }

      case unsupported =>
        throw new IllegalArgumentException(s"unsupported checked arithmetic op: $unsupported")
    }

  private def emitCheckedUnOp(dst: Temp, op: UnaryOp, x: Rhs, onOverflow: Label)
                             (using cs: X86CodegenState, gr: GenRes): Unit =
    cs.withEval(x) { reg =>
      op match {
        case UnaryOp.Neg =>
          gr.emit(Neg(Reg32(reg)))
          gr.emit(Jcc(X86Cond.O, onOverflow))
          cs.frame.storeTemp(dst, reg)

        case unsupported =>
          throw new IllegalArgumentException(s"unsupported checked unary op: $unsupported")
      }
    }

  private def emitBinOp(dst: Temp, op: BinaryOp, lhs: Rhs, rhs: Rhs)
                       (using cs: X86CodegenState, gr: GenRes): Unit =
    op match {
      case f: FloatArithOp =>
        emitFloatArithmetic(dst, f, lhs, rhs)

      case f: FloatCondOp =>
        emitFloatComparison(dst, f, lhs, rhs)

      case ArithOp.Div =>
        cs.withEval2(lhs, rhs) { (lreg, rreg) => emitDivMod(dst, lreg, rreg, wantRemainder = false) }

      case ArithOp.Mod =>
        cs.withEval2(lhs, rhs) { (lreg, rreg) => emitDivMod(dst, lreg, rreg, wantRemainder = true) }

      case _ =>
        cs.withEval2(lhs, rhs) { (lreg, rreg) =>
          op match {
            case ArithOp.Add =>
              gr.emit(Add(Reg32(lreg), Reg32(rreg)))
              emitOverflowBranch()

            case ArithOp.Sub =>
              gr.emit(Sub(Reg32(lreg), Reg32(rreg)))
              emitOverflowBranch()

            case ArithOp.Mul =>
              gr.emit(Imul(Reg32(lreg), Reg32(rreg)))
              emitOverflowBranch()

            case BoolOp.And =>
              gr.emit(And(Reg8(lreg), Reg8(rreg)))

            case BoolOp.Or =>
              gr.emit(Or(Reg8(lreg), Reg8(rreg)))

            case BitwiseOp.BitAnd =>
              gr.emit(And(Reg32(lreg), Reg32(rreg)))

            case BitwiseOp.BitOr =>
              gr.emit(Or(Reg32(lreg), Reg32(rreg)))

            case cond: CondOp =>
              emitComparison(cond, lreg, rreg, lhs.len, rhs.len)

            case unsupported =>
              throw new IllegalArgumentException(s"unsupported binary op: $unsupported")
          }

          cs.frame.storeTemp(dst, lreg)
        }
    }

  private def emitFloatArithmetic(dst: Temp, op: FloatArithOp, lhs: Rhs, rhs: Rhs)
                                 (using cs: X86CodegenState, gr: GenRes): Unit =
    cs.withEval2(lhs, rhs) { (lreg, rreg) =>
      gr.emit(Movd(XMM(0), Reg32(lreg)))
      gr.emit(Movd(XMM(1), Reg32(rreg)))
      op match {
        case FloatArithOp.Add => gr.emit(Addss(XMM(0), XMM(1)))
        case FloatArithOp.Sub => gr.emit(Subss(XMM(0), XMM(1)))
        case FloatArithOp.Mul => gr.emit(Mulss(XMM(0), XMM(1)))
        case FloatArithOp.Div => gr.emit(Divss(XMM(0), XMM(1)))
      }
      gr.emit(Movd(Reg32(lreg), XMM(0)))
      cs.frame.storeTemp(dst, lreg)
    }

  private def emitFloatComparison(dst: Temp, op: FloatCondOp, lhs: Rhs, rhs: Rhs)
                                  (using cs: X86CodegenState, gr: GenRes): Unit =
    cs.withEval2(lhs, rhs) { (lreg, rreg) =>
      gr.emit(Movd(XMM(0), Reg32(lreg)))
      gr.emit(Movd(XMM(1), Reg32(rreg)))
      gr.emit(Ucomiss(XMM(0), XMM(1)))
      gr.emit(Setcc(toX86FloatCond(op), lreg))
      gr.emit(Movzx(lreg, Reg8(lreg)))
      cs.frame.storeTemp(dst, lreg)
    }

  private def emitUnOp(dst: Temp, op: UnaryOp, x: Rhs)
                      (using cs: X86CodegenState, gr: GenRes): Unit =
    cs.withEval(x) { reg =>
      op match {
        case UnaryOp.Neg =>
          gr.emit(Neg(Reg32(reg)))
          emitOverflowBranch()

        case UnaryOp.Not =>
          gr.emit(Xor(Reg8(reg), Immediate(1)))

        case UnaryOp.BitNot =>
          gr.emit(Not(Reg32(reg)))

        case UnaryOp.Ord =>
          ()

        case UnaryOp.Chr =>
          gr.emit(Cmp(Reg32(reg), Immediate(ASCII_MIN)))
          gr.emit(Jcc(X86Cond.L, Label(PreDefRunTimeError.ErrBadCharLabel)))
          gr.emit(Cmp(Reg32(reg), Immediate(ASCII_MAX)))
          gr.emit(Jcc(X86Cond.G, Label(PreDefRunTimeError.ErrBadCharLabel)))
          gr.addUsedErrs(PreDefRunTimeError.ErrBadCharLabel)

        case UnaryOp.Len =>
          gr.emit(Cmp(Reg64(reg), Immediate(NULL_PTR)))
          gr.emit(Jcc(X86Cond.E, Label(PreDefRunTimeError.ErrNullLabel)))
          gr.emit(Mov(Reg32(reg), Mem(X86Address.Base(reg, -ARR_HDR), Some(X86Size.DWord))))
          gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
          gr.addPreDefs(_PRINTS)
      }

      cs.frame.storeTemp(dst, reg)
    }

  private def emitDivMod(dst: Temp, lhs: X86Reg, rhs: X86Reg, wantRemainder: Boolean)
                        (using cs: X86CodegenState, gr: GenRes): Unit = {
    gr.emit(Cmp(Reg32(rhs), Immediate(0)))
    gr.emit(Jcc(X86Cond.E, Label(PreDefRunTimeError.ErrDivZeroLabel)))
    gr.addUsedErrs(PreDefRunTimeError.ErrDivZeroLabel)
    gr.addPreDefs(_PRINTS)

    withDivisorScratch { divisor =>
      gr.emit(Mov(Reg32(divisor), Reg32(rhs)))
      gr.emit(Mov(Reg32(RETURN_REG), Reg32(lhs)))
      gr.emit(Cdq)
      gr.emit(Idiv(Reg32(divisor)))
    }

    if (wantRemainder) cs.frame.storeTemp(dst, RDX)
    else cs.frame.storeTemp(dst, RETURN_REG)
  }

  private def withDivisorScratch[A](f: X86Reg => A)(using cs: X86CodegenState): A =
    cs.ra.withNewRegister { reg =>
      if (reg == RDX) cs.ra.withNewRegister(f)
      else f(reg)
    }

  private def emitComparison(cond: CondOp, lhs: X86Reg, rhs: X86Reg, lhsLen: BitLength, rhsLen: BitLength)
                            (using gr: GenRes): Unit = {
    gr.emit(Cmp(cmpOperand(lhs, lhsLen, rhsLen), cmpOperand(rhs, lhsLen, rhsLen)))
    gr.emit(Setcc(toX86Cond(cond), lhs))
    gr.emit(Movzx(lhs, Reg8(lhs)))
  }

  private def emitOverflowBranch()(using gr: GenRes): Unit = {
    gr.emit(Jcc(X86Cond.O, Label(PreDefRunTimeError.ErrOverflowLabel)))
    gr.addUsedErrs(PreDefRunTimeError.ErrOverflowLabel)
    gr.addPreDefs(_PRINTS)
  }

  private def emitPrint(printLabel: String)(using gr: GenRes): Unit = {
    gr.emit(Call(Label(printLabel)))
    gr.addPreDefs(printLabel)
  }

  private def emitCallArgs(args: List[Rhs])(using cs: X86CodegenState, gr: GenRes): Int = {
    val regArgCount = math.min(ARG_REG_COUNT, args.length)
    val stackArgCount = math.max(0, args.length - ARG_REG_COUNT)
    val stackArgBytes = stackArgCount * SLOT_SIZE
    val stagingBytes = regArgCount * SLOT_SIZE
    val totalBytes = align16(stackArgBytes + stagingBytes)
    val stagingBase = stackArgBytes

    if (totalBytes > 0) gr.emit(Sub(Reg64(RSP), Immediate(totalBytes.toLong)))

    args.zipWithIndex.foreach { case (arg, i) =>
      cs.ra.withNewRegister { scratch =>
        cs.frame.loadRhs(arg, scratch, cs.ra)
        val slotOffset =
          if (i < ARG_REG_COUNT) stagingBase + i * SLOT_SIZE
          else (i - ARG_REG_COUNT) * SLOT_SIZE
        storeCallSlot(slotOffset, arg.len, scratch)
      }
    }

    for (i <- 0 until regArgCount) {
      loadCallSlot(stagingBase + i * SLOT_SIZE, args(i).len, argRegisters(i))
    }

    totalBytes
  }

  private def storeCallSlot(offset: Int, len: BitLength, from: X86Reg)(using gr: GenRes): Unit =
    gr.emit(Mov(stackSlot(offset, len), regOperand(from, sizeOf(len))))

  private def loadCallSlot(offset: Int, len: BitLength, into: X86Reg)(using gr: GenRes): Unit =
    sizeOf(len) match {
      case SIZE_CHAR_BOOL_BYTES => gr.emit(Movzx(into, stackSlot(offset, len)))
      case size                 => gr.emit(Mov(regOperand(into, size), stackSlot(offset, len)))
    }

  private def storeGlobal(labelName: String, size: X86Size, value: X86Operand)(using gr: GenRes): Unit =
    gr.emit(Mov(globalMem(labelName, size), value))

  private def globalMem(labelName: String, size: X86Size): Mem =
    Mem(X86Address.RipRelative(Label(labelName)), Some(size))

  private def stackSlot(offset: Int, len: BitLength): Mem =
    Mem(X86Address.Base(RSP, offset), Some(sizeToX86(sizeOf(len))))

  private def cmpOperand(reg: X86Reg, lhsLen: BitLength, rhsLen: BitLength): X86Operand =
    regOperand(reg, math.max(cmpSize(lhsLen), cmpSize(rhsLen)))

  private def cmpSize(len: BitLength): Int =
    math.max(sizeOf(len), SIZE_INT_BYTES)

  private def sizeOf(len: BitLength): Int =
    len.convertToBytes(SLOT_SIZE)

  private def regOperand(reg: X86Reg, size: Int): X86Operand = size match {
    case SIZE_CHAR_BOOL_BYTES => Reg8(reg)
    case SIZE_INT_BYTES       => Reg32(reg)
    case SIZE_PTR_BYTES       => Reg64(reg)
    case _ => throw new UnsupportedOperationException(s"unsupported x86 register access size: $size")
  }

  private def toX86Cond(cond: CondOp): X86Cond = cond match {
    case CondOp.EQ  => X86Cond.E
    case CondOp.NEQ => X86Cond.NE
    case CondOp.LT  => X86Cond.L
    case CondOp.LEQ => X86Cond.LE
    case CondOp.GT  => X86Cond.G
    case CondOp.GEQ => X86Cond.GE
  }

  private def toX86FloatCond(cond: FloatCondOp): X86Cond = cond match {
    case FloatCondOp.EQ  => X86Cond.E
    case FloatCondOp.NEQ => X86Cond.NE
    case FloatCondOp.LT  => X86Cond.B
    case FloatCondOp.LEQ => X86Cond.BE
    case FloatCondOp.GT  => X86Cond.A
    case FloatCondOp.GEQ => X86Cond.AE
  }

  private def align16(n: Int): Int = {
    val r = n % STACK_ALLIGNMENT
    if (r == 0) n else n + (STACK_ALLIGNMENT - r)
  }

  // Load incoming parameters from registers/stack into frame
  override protected def initParams(frame: X86StackFrame, params: List[TAC.Temp])
                                   (using cs: X86CodegenState, gr: GenRes): Unit = {
    for ((t, i) <- params.zipWithIndex) {
      if (i < ARG_REG_COUNT) {
        frame.storeTemp(t, argRegisters(i))
      } else {
        val off = frame.incomingArgOffset(i)
        val size = t.len.convertToBytes(SLOT_SIZE)

        cs.ra.withNewRegister { scratch =>
          loadStackParam(scratch, off, size)
          frame.storeTemp(t, scratch)
        }
      }
    }
  }

  private def loadStackParam(into: X86Reg, offset: Int, size: Int)(using gr: GenRes): Unit =
    val src = Mem(X86Address.Base(RBP, offset), Some(sizeToX86(size)))
    size match {
      case SIZE_CHAR_BOOL_BYTES => gr.emit(Movzx(into, src))
      case SIZE_INT_BYTES       => gr.emit(Mov(Reg32(into), src))
      case SIZE_PTR_BYTES       => gr.emit(Mov(Reg64(into), src))
      case _ => throw new UnsupportedOperationException(s"unsupported x86 parameter size: $size")
    }

  private def sizeToX86(size: Int): X86Size = size match {
    case SIZE_CHAR_BOOL_BYTES => X86Size.Byte
    case SIZE_INT_BYTES       => X86Size.DWord
    case SIZE_PTR_BYTES       => X86Size.QWord
    case _ => throw new UnsupportedOperationException(s"unsupported x86 memory access size: $size")
  }
}
