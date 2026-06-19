package wacc.backend.Arm32.target

import scala.collection.mutable

import wacc.backend.midir.TAC.*
import wacc.backend.midir.TAC
import wacc.backend.Arm32.Arm32Constants.*
import wacc.backend.Arm32.codeGen.*
import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.target.*
import wacc.backend.BackendCommon.Immediate
import wacc.backend.Arm32.codeGen.A32MemAddress
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.Arm32.preDefFunctions.PreDefRunTimeError
import wacc.backend.BackendCommon.AsmEntity.*

final class A32StackFrame(stackSlots: Seq[Temp]) extends CommonStackFrame {

  override val stackAlignment = STACK_ALLIGNMENT        // required stack alignment for Arm32
  override val slotSize = SLOT_SIZE                     // size of each spill slot
  override val argRegCount = ARG_REG_COUNT              // number of argument registers (r0–r3)
  override val spillTmpAreaSlots = SPILL_TMP_AREA_SLOTS // reserved area for spills
  override val frameOverhead  = FRAME_HEADER_BYTES      // saved FP + LR
  override val slots = stackSlots

  buildTempSlots()
  reserveArgSpillArea()

  /** Function prologue. Creates a new stack frame: save caller frame pointer and return address,
   *  establish new stack pointer, and allocate space for local temporaries */
  def addFrame()(using cs: A32CodegenState): List[A32Instr] =
    cs.ra.withNewRegister { reg =>
      val buf = mutable.ListBuffer[A32Instr]()
      val alloc = localsAreaSize
      buf += Push(List(FP, LR))
      buf += Mov(FP, SP)

      if (alloc > 0) {
        if (math.abs(alloc) < MAX_INT_ARM32) buf += Sub(SP, SP, Immediate(alloc))
        else
          buf += LdrFuncPtr(reg, MemAddress.LabelAddress(Label(s"=${alloc.toString}")))
          buf += Sub(SP, SP, reg)
      }

      buf.toList
    }

  /** Function epilogue. Restores stack pointer and returns to caller */
  def removeFrame()(using cs: A32CodegenState): List[A32Instr] =
    cs.ra.withNewRegister { reg =>
      val buf = mutable.ListBuffer[A32Instr]()
      val alloc = localsAreaSize

      if (alloc > 0) {
        if (math.abs(alloc) < MAX_INT_ARM32) buf += Add(SP, SP, Immediate(alloc))
        else
          buf += LdrFuncPtr(reg, MemAddress.LabelAddress(Label(s"=${alloc.toString}")))
          buf += Add(SP, SP, reg)
      }

      buf += Pop(List(FP, PC))
      buf.toList
    }

  /** Load a TAC right-hand-side value into a register. */
  def loadRhs(rhs: Rhs, into: A32Reg, ra: A32RegisterAllocator)
             (using cs: A32CodegenState, gr: GenRes): Unit = rhs match {
    // Immediate values
    case ImmValue(v, _) =>
      if (math.abs(v) < MAX_INT_ARM32) then gr.emit(Mov(into, Immediate(v)))
      else gr.emit(LdrFuncPtr(into, MemAddress.LabelAddress(Label(s"=${v.toString}"))))

    // Strings in TAC
    case TACStr(id, _) =>
      val label = Label(s"=.str_$id")
      gr.emit(LdrFuncPtr(into, MemAddress.LabelAddress(label)))

    // Pairs
    case Pair(fst, snd, len) =>
      ra.withNewRegister { pairReg =>
        gr.emit(Mov(RETURN_REG, Immediate(PAIR_SIZE)))
        gr.emit(Bl(Label(_MALLOC)))
        gr.emit(Mov(pairReg, RETURN_REG))

        gr.addPreDefs(_MALLOC)
        gr.addPreDefs(_PRINTS)
        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfMemoryLabel)

        pairCreateHelper(pairReg, fst, true, ra)
        pairCreateHelper(pairReg, snd, false, ra)
        gr.emit(Mov(into, pairReg))
      }

    // Arrays
    case Array(exprs, len) =>
      val nElems = exprs.length
      val elemSize = len.convertToBytes(A32_WORD_SIZE)
      val totalSize = nElems * elemSize + ARR_HDR

      ra.withNewRegister { elemReg =>
        gr.emit(Mov(RETURN_REG, Immediate(totalSize)))
        gr.emit(Bl(Label(_MALLOC)))
        gr.addPreDefs(_MALLOC)
        gr.addPreDefs(_PRINTS)
        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfMemoryLabel)

        gr.emit(Mov(into, RETURN_REG))

        gr.emit(Mov(RETURN_REG, Immediate(nElems)))
        gr.emit(Str(RETURN_REG, MemAddress.ImmOffsetAddress(into, 0, NoIndex)))

        gr.emit(Add(into, into, Immediate(ARR_HDR)))

        exprs.zipWithIndex.foreach { case (e, i) =>
          loadRhs(e, elemReg, ra)

          val offset = i * elemSize
          val immOffsetAddr = MemAddress.ImmOffsetAddress(into, offset, NoIndex)
          if (elemSize == BoolCharSize) then gr.emit(Strb(elemReg, immOffsetAddr))
          else gr.emit(Str(elemReg, immOffsetAddr))
        }
      }

    // Load of temporaries
    case t: Temp => loadSpill(t, into)

    // Load of indirect temporaries, for array or pair
    case it: IndirectTemp =>
      if it.isArray then arrayELemAccess(it, into, it.base, it.offset, true)
      else withPairFieldAddr(it, into, true)
  }

  /** Store temporaries and indirect temporaries into the stack frame */
  def storeTemp(lhs: TAC.Val, from: A32Reg)
               (using cs: A32CodegenState, gr: GenRes): Unit = lhs match {
    case t: Temp => storeSpill(t, from)

    case it: IndirectTemp =>
      if it.isArray then arrayELemAccess(it, from, it.base, it.offset, false)
      else withPairFieldAddr(it, from, false)
  }

  // Store spilled temporary to stack slot
  private def storeSpill(t: Temp, from: A32Reg)(using gr: GenRes): Unit = {
    val off = offsetOf(t)
    val size = t.len.convertToBytes(A32_WORD_SIZE)
    val immOffsetAddr: A32MemAddress = MemAddress.ImmOffsetAddress(FP, off, NoIndex)
    if (size == BoolCharSize) then gr.emit(Strb(from, immOffsetAddr))
    else gr.emit(Str(from, immOffsetAddr))
  }

  // Load spilled temporary from stack slot
  private def loadSpill(t: Temp, into: A32Reg)(using gr: GenRes): Unit = {
    val off = offsetOf(t)
    val size = t.len.convertToBytes(A32_WORD_SIZE)
    val immOffsetAddr: A32MemAddress = MemAddress.ImmOffsetAddress(FP, off, NoIndex)
    if (size == BoolCharSize) then gr.emit(Ldrb(into, immOffsetAddr))
    else gr.emit(Ldr(into, immOffsetAddr))
  }

  // Helper to initialise one element of a pair in memory.
  private def pairCreateHelper(pairReg: A32Reg, elem: Rhs, isFirst: Boolean, ra: A32RegisterAllocator)
                              (using cs: A32CodegenState, gr: GenRes): Unit =
    val elemOffset = if (isFirst) FST_OFF else SND_OFF
    ra.withNewRegister { elemReg =>
      loadRhs(elem, elemReg, ra)
      gr.emit(Str(elemReg, MemAddress.ImmOffsetAddress(pairReg, elemOffset, NoIndex)))
    }

  // Compute the address of a field within a pair and optionally load or store it.
  private def withPairFieldAddr(it: IndirectTemp, reg: A32Reg, isLoad: Boolean)
                               (using cs: A32CodegenState, gr: GenRes): Unit =
    cs.ra.withRegisters2 { (baseReg, offsetReg) =>
      emitNullCheck(it, baseReg, cs.ra)
      it.offset match {
        case imm: ImmValue => gr.emit(Mov(offsetReg, Immediate(imm.value)))
        case tmp: Temp => loadRhs(tmp, offsetReg, cs.ra)
      }

      if isLoad then gr.emit(Ldr(reg, MemAddress.RegOffsetAddress(baseReg, offsetReg)))
      else gr.emit(Str(reg, MemAddress.RegOffsetAddress(baseReg, offsetReg)))
    }

  // Emit a runtime null pointer check for an indirect temporary.
  private def emitNullCheck(it: IndirectTemp, baseReg: A32Reg, ra: A32RegisterAllocator)
                           (using cs: A32CodegenState, gr: GenRes): Unit = {
    loadRhs(it.base, baseReg, ra)
    gr.emit(Cmp(baseReg, Immediate(NULL_PTR)))
    gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)))

    gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
    gr.addPreDefs(_PRINTS)
  }

  // Generate code to access (load or store) an element of an array.
  private def arrayELemAccess(it: IndirectTemp, reg: A32Reg, base: Rhs, index: Rhs, isLoad: Boolean)
                             (using cs: A32CodegenState, gr: GenRes): Unit =
    val elemSize = it.len.convertToBytes(A32_WORD_SIZE)
    withArrayElemAddr(base, index, elemSize) { addr =>
      val baseRegAddr: A32MemAddress = MemAddress.BaseRegAddress(addr)
      (elemSize, isLoad) match {
        case (BoolCharSize, true)  => gr.emit(Ldrb(reg, baseRegAddr))
        case (BoolCharSize, false) => gr.emit(Strb(reg, baseRegAddr))
        case (_, true)             => gr.emit(Ldr(reg, baseRegAddr))
        case (_, false)            => gr.emit(Str(reg, baseRegAddr))
      }
    }

  // Compute the address of an array element, with runtime null and bounds checks.
  private def withArrayElemAddr[A](base: Rhs, index: Rhs, elemSizeBytes: Int)(k: R => A)
                                  (using cs: A32CodegenState, gr: GenRes): A =
    cs.ra.withNewRegisters6 { (rBase, rIdx, rLen, rMul, rScale, rAddr) =>
      cs.frame.loadRhs(base, rBase, cs.ra)

      gr.emit(Cmp(rBase, Immediate(NULL_PTR)))
      gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)))
      gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)

      cs.frame.loadRhs(index, rIdx, cs.ra)

      gr.emit(Ldr(rLen, MemAddress.ImmOffsetAddress(rBase, -ARR_HDR, NoIndex)))

      gr.emit(Cmp(rIdx, Immediate(0)))
      gr.emit(Mov(RETURN_REG, rIdx))
      gr.emit(BCond(Cond.LT, Label(PreDefRunTimeError.ErrOutOfBoundsLabel)))

      gr.emit(Cmp(rIdx, rLen))
      gr.emit(Mov(RETURN_REG, rIdx))
      gr.emit(BCond(Cond.GE, Label(PreDefRunTimeError.ErrOutOfBoundsLabel)))

      gr.addUsedErrs(PreDefRunTimeError.ErrOutOfBoundsLabel)

      gr.emit(Mov(rAddr, rBase))
      gr.emit(Mov(rMul, rIdx))
      gr.emit(Mov(rScale, Immediate(elemSizeBytes.toLong)))
      gr.emit(Mul(rMul, rMul, rScale))
      gr.emit(Add(rAddr, rAddr, rMul))

      k(rAddr.asInstanceOf[R])
    }
}
