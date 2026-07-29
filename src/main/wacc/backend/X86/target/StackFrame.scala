package wacc.backend.X86.target

import scala.collection.mutable

import wacc.backend.midir.TAC.*
import wacc.backend.midir.TAC
import wacc.backend.X86.X86Constants.*
import wacc.backend.X86.codeGen.*
import wacc.backend.X86.codeGen.X86Instr.*
import wacc.backend.BackendCommon.Immediate
import wacc.backend.BackendCommon.*
import wacc.backend.X86.preDefFunctions.PreDefRunTimeError
import wacc.backend.BackendCommon.AsmEntity.*

final class X86StackFrame(stackSlots: Seq[Temp]) extends CommonStackFrame {

  override val stackAlignment = STACK_ALLIGNMENT // SysV x86-64 requires 16-byte stack alignment before calls
  override val slotSize = SLOT_SIZE // size of each spill slot
  override val argRegCount = ARG_REG_COUNT // number of integer/pointer argument registers
  override val spillTmpAreaSlots = SPILL_TMP_AREA_SLOTS // reserved area for spills
  override val frameOverhead = FRAME_HEADER_BYTES // saved RBP + return address
  override val slots = stackSlots

  buildTempSlots()
  reserveArgSpillArea()

  def addFrame(): List[X86Instr] = {
    val alloc = localsAreaSize
    val buf = mutable.ListBuffer[X86Instr]()

    buf += Push(Reg64(RBP))
    buf += Mov(Reg64(RBP), Reg64(RSP))
    if (alloc > 0) buf += Sub(Reg64(RSP), Immediate(alloc.toLong))

    buf.toList
  }

  def removeFrame(): List[X86Instr] =
    List(
      Mov(Reg64(RSP), Reg64(RBP)),
      Pop(Reg64(RBP)),
      Ret
    )

  def loadRhs(rhs: Rhs, into: X86Reg, ra: X86RegisterAllocator)
             (using cs: X86CodegenState, gr: GenRes): Unit = rhs match {
    case ImmValue(v, len) =>
      val size = sizeOf(len)
      gr.emit(Mov(regOperand(into, math.max(size, SIZE_INT_BYTES)), Immediate(v)))

    case f: FloatValue =>
      gr.emit(Mov(Reg32(into), Immediate(f.rawBitsAsLong)))

    case TACStr(id, _) =>
      gr.emit(Lea(Reg64(into), ripMem(Label(s".str_$id"))))

    case Pair(fst, snd, _) =>
      ra.withNewRegister { pairReg =>
        gr.emit(Mov(Reg64(FIRST_ARG_REG), Immediate(PAIR_SIZE)))
        gr.emit(Call(Label(_MALLOC)))
        gr.emit(Mov(Reg64(pairReg), Reg64(RETURN_REG)))

        gr.addPreDefs(_MALLOC)
        gr.addPreDefs(_PRINTS)
        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfMemoryLabel)

        pairCreateHelper(pairReg, fst, true, ra)
        pairCreateHelper(pairReg, snd, false, ra)
        gr.emit(Mov(Reg64(into), Reg64(pairReg)))
      }

    case Array(exprs, len) =>
      val nElems = exprs.length
      val elemSize = sizeOf(len)
      val totalSize = nElems * elemSize + ARR_HDR

      ra.withNewRegister { elemReg =>
        gr.emit(Mov(Reg64(FIRST_ARG_REG), Immediate(totalSize.toLong)))
        gr.emit(Call(Label(_MALLOC)))
        gr.addPreDefs(_MALLOC)
        gr.addPreDefs(_PRINTS)
        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfMemoryLabel)

        gr.emit(Mov(Reg64(into), Reg64(RETURN_REG)))
        gr.emit(Mov(mem(RAX, X86Size.DWord), Immediate(nElems.toLong)))
        gr.emit(Add(Reg64(into), Immediate(ARR_HDR.toLong)))

        exprs.zipWithIndex.foreach { case (e, i) =>
          loadRhs(e, elemReg, ra)
          storeToMem(mem(into, sizeToX86(elemSize), i * elemSize), elemReg, elemSize)
        }
      }

    case t: Temp => loadSpill(t, into)

    case it: IndirectTemp =>
      if it.isArray then arrayELemAccess(it, into, it.base, it.offset, true)
      else withPairFieldAddr(it, into, true)
  }

  def storeTemp(lhs: TAC.Val, from: X86Reg)
               (using cs: X86CodegenState, gr: GenRes): Unit = lhs match {
    case t: Temp => storeSpill(t, from)

    case it: IndirectTemp =>
      if it.isArray then arrayELemAccess(it, from, it.base, it.offset, false)
      else withPairFieldAddr(it, from, false)
  }

  private def storeSpill(t: Temp, from: X86Reg)(using gr: GenRes): Unit = {
    val size = sizeOf(t.len)
    storeToMem(mem(RBP, sizeToX86(size), offsetOf(t)), from, size)
  }

  private def loadSpill(t: Temp, into: X86Reg)(using gr: GenRes): Unit = {
    val size = sizeOf(t.len)
    loadFromMem(into, mem(RBP, sizeToX86(size), offsetOf(t)), size)
  }

  private def pairCreateHelper(pairReg: X86Reg, elem: Rhs, isFirst: Boolean, ra: X86RegisterAllocator)
                              (using cs: X86CodegenState, gr: GenRes): Unit =
    val elemOffset = if (isFirst) FST_OFF else SND_OFF
    ra.withNewRegister { elemReg =>
      loadRhs(elem, elemReg, ra)
      storeToMem(mem(pairReg, X86Size.QWord, elemOffset), elemReg, SIZE_PTR_BYTES)
    }

  private def withPairFieldAddr(it: IndirectTemp, reg: X86Reg, isLoad: Boolean)
                               (using cs: X86CodegenState, gr: GenRes): Unit =
    cs.ra.withNewRegisters2 { (baseReg, offsetReg) =>
      emitNullCheck(it, baseReg, cs.ra)

      val size = sizeOf(it.len)
      val accessMem = it.offset match {
        case imm: ImmValue =>
          mem(baseReg, sizeToX86(size), imm.value.toInt)

        case tmp: Temp =>
          loadRhs(tmp, offsetReg, cs.ra)
          Mem(X86Address.Index(Some(baseReg), offsetReg, 1), Some(sizeToX86(size)))
      }

      if isLoad then loadFromMem(reg, accessMem, size)
      else storeToMem(accessMem, reg, size)
    }

  private def emitNullCheck(it: IndirectTemp, baseReg: X86Reg, ra: X86RegisterAllocator)
                           (using cs: X86CodegenState, gr: GenRes): Unit = {
    loadRhs(it.base, baseReg, ra)
    gr.emit(Cmp(Reg64(baseReg), Immediate(NULL_PTR)))
    gr.emit(Jcc(X86Cond.E, Label(PreDefRunTimeError.ErrNullLabel)))

    gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
    gr.addPreDefs(_PRINTS)
  }

  private def arrayELemAccess(it: IndirectTemp, reg: X86Reg, base: Rhs, index: Rhs, isLoad: Boolean)
                             (using cs: X86CodegenState, gr: GenRes): Unit = {
    val elemSize = sizeOf(it.len)
    withArrayElemAddr(base, index, elemSize) { addr =>
      val accessMem = mem(addr, sizeToX86(elemSize))
      if isLoad then loadFromMem(reg, accessMem, elemSize)
      else storeToMem(accessMem, reg, elemSize)
    }
  }

  private def withArrayElemAddr[A](base: Rhs, index: Rhs, elemSizeBytes: Int)(k: X86Reg => A)
                                  (using cs: X86CodegenState, gr: GenRes): A =
    cs.ra.withNewRegisters2 { (rBase, rIdx) =>
      require(Set(1, 2, 4, 8).contains(elemSizeBytes), s"invalid x86 scale: $elemSizeBytes")

      cs.ra.withNewRegister { rLen =>
        cs.frame.loadRhs(base, rBase, cs.ra)

        gr.emit(Cmp(Reg64(rBase), Immediate(NULL_PTR)))
        gr.emit(Jcc(X86Cond.E, Label(PreDefRunTimeError.ErrNullLabel)))
        gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
        gr.addPreDefs(_PRINTS)

        cs.frame.loadRhs(index, rIdx, cs.ra)
        gr.emit(Mov(Reg32(rLen), mem(rBase, X86Size.DWord, -ARR_HDR)))

        gr.emit(Cmp(Reg32(rIdx), Immediate(0)))
        gr.emit(Mov(Reg32(FIRST_ARG_REG), Reg32(rIdx)))
        gr.emit(Jcc(X86Cond.L, Label(PreDefRunTimeError.ErrOutOfBoundsLabel)))

        gr.emit(Cmp(Reg32(rIdx), Reg32(rLen)))
        gr.emit(Mov(Reg32(FIRST_ARG_REG), Reg32(rIdx)))
        gr.emit(Jcc(X86Cond.GE, Label(PreDefRunTimeError.ErrOutOfBoundsLabel)))

        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfBoundsLabel)

        gr.emit(Lea(Reg64(rBase), Mem(X86Address.Index(Some(rBase), rIdx, elemSizeBytes))))

        k(rBase)
      }
    }

  private def sizeOf(len: BitLength): Int =
    len.convertToBytes(SLOT_SIZE)

  private def sizeToX86(size: Int): X86Size = size match {
    case SIZE_CHAR_BOOL_BYTES => X86Size.Byte
    case SIZE_INT_BYTES       => X86Size.DWord
    case SIZE_PTR_BYTES       => X86Size.QWord
    case _ => throw new UnsupportedOperationException(s"unsupported x86 memory access size: $size")
  }

  private def regOperand(reg: X86Reg, size: Int): X86Operand = size match {
    case SIZE_CHAR_BOOL_BYTES => Reg8(reg)
    case SIZE_INT_BYTES       => Reg32(reg)
    case SIZE_PTR_BYTES       => Reg64(reg)
    case _ => throw new UnsupportedOperationException(s"unsupported x86 register access size: $size")
  }

  private def mem(base: X86Reg, size: X86Size, disp: Int = 0): Mem =
    Mem(X86Address.Base(base, disp), Some(size))

  private def ripMem(label: Label): Mem =
    Mem(X86Address.RipRelative(label), Some(X86Size.QWord))

  private def loadFromMem(into: X86Reg, from: Mem, size: Int)(using gr: GenRes): Unit =
    size match {
      case SIZE_CHAR_BOOL_BYTES => gr.emit(Movzx(into, from))
      case _                    => gr.emit(Mov(regOperand(into, size), from))
    }

  private def storeToMem(to: Mem, from: X86Reg, size: Int)(using gr: GenRes): Unit =
    gr.emit(Mov(to, regOperand(from, size)))
}
