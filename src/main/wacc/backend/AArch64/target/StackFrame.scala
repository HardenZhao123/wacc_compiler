package wacc.backend.AArch64.target

import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.ShiftType.LSL
import wacc.backend.midir.TAC
import wacc.backend.midir.TAC.*
import Reg.{toW, toX}
import wacc.backend.AArch64.target.StackFrameConstant.*
import wacc.backend.AArch64.codeGen.{A64CodegenState, A64Instr, GenRes}
import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.AArch64.preDefFunctions.PreDefRunTimeError
import wacc.backend.AArch64.AArch64Constants.*
import wacc.backend.BackendCommon.MemAddress
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.AsmEntity.*

import scala.collection.mutable

/**
 * - Prologue/Epilogue use FP (x29) + LR (x30) saving.
 * - Maintains 16-byte SP alignment.
 * - Allocates stack slots for locals/temps/spills at negative FP-relative offsets.
 * - Incoming spilled parameters (arg8+) are read from [fp, #16 + (i-8)*8] (ABI rule).
 *
 * Parameter strategy (what you decided):
 * - Keep x0..x7 for parameters 0..7 .
 * - BUT: before any nested call (bl ...), save x0..x7 into the frame and restore them after the call
 *
 * Notice:
 * - Slot size is fixed at 8 bytes
 */
final class StackFrame(stackSlots: Seq[Temp]) extends CommonStackFrame {

  override val stackAlignment    = STACK_ALIGNMENT_BYTES // AArch64 ABI require SP 16-byte alignment
  override val slotSize          = STACK_SLOT_BYTES
  override val argRegCount       = ARG_REG_COUNT
  override val spillTmpAreaSlots = SPILL_TMP_AREA_SLOTS
  override val slots             = stackSlots
  override val frameOverhead = FRAME_HEADER_BYTES // saved x29,x30 at [fp+0..15]

  // LDUR/STUR use simm9 offset (in range -256 to 255)
  private val SIGNED_9_MIN = SIMM9_MIN_BYTES
  private val SIGNED_9_MAX = SIMM9_MAX_BYTES

  buildTempSlots()
  reserveArgSpillArea() // IMPORTANT: ensure frame size covers x0..x7 spill slots

  // Checks whether an FP-relative offset can use the short signed-immediate form.
  private def fitsSigned9(off: Int): Boolean = off >= SIGNED_9_MIN && off <= SIGNED_9_MAX

  // Materialises an FP-relative address in a scratch register when direct offset encoding is unavailable.
  private def materializeAddrFromFP(off: Int, scratch: X)(using gr: GenRes): Unit =
    if (off == 0) ()
    else if (off < 0) gr.emit(Sub(scratch, FP, Immediate((-off).toLong)))
    else gr.emit(A64Instr.Add(scratch, FP, Immediate(off.toLong)))

  //prologue / epilogue
  // Emits the function prologue that saves FP/LR and allocates local stack space.
  def addFrame(): List[A64Instr] = {
    val alloc = localsAreaSize
    val buf = mutable.ListBuffer[A64Instr]()

    // stp x29, x30, [sp, #-16]!
    buf += Stp(FP, LR, MemAddress.ImmOffsetAddress(SP, -FP_LR_SAVE_BYTES, PreIndex))
    // mov x29, sp
    buf += Mov(FP, SP)
    // sub sp, sp, #alloc
    if (alloc > 0) buf += Sub(SP, SP, Immediate(alloc))

    buf.toList
  }

  // Emits the function epilogue that restores FP/LR and returns to the caller.
  def removeFrame(): List[A64Instr] =
    List(
      // mov sp, x29
      Mov(SP, FP),
      // ldp x29, x30, [sp], #16
      Ldp(FP, LR, MemAddress.ImmOffsetAddress(SP, FP_LR_SAVE_BYTES, PostIndex)),
      Ret
    )

  // Loads a TAC right-hand side into the requested target register.
  def loadRhs(rhs: Rhs, into: A64Reg, ra: A64RegisterAllocator)
             (using cs: A64CodegenState, gr: GenRes): Unit = rhs match {
    case ImmValue(v, _)  =>
      val low16  = v & MASK_16
      val high16 = (v >> SHIFT_16) & MASK_16

      if (high16 == 0) {
        // small immediate fits in lower 16 bits
        gr.emit(Mov(toW(into), Immediate(v)))
      } else {
        // general 32-bit immediate
        gr.emit(Mov(toW(into), Immediate(low16)))
        gr.emit(Movk(toW(into), Immediate(high16), LSL(Immediate(MOVK_SHIFT_HALFWORD))))
      }

    case TACStr(id, _) =>
      val label = Label(s".str_$id")

      // String is a pointer, so materialize into an X register.
      into match {
        case _: W =>
          throw new IllegalArgumentException(s"loadRhs: Str($id) must be loaded into X-reg, got $into")

        case _ =>
          gr.emit(Adrp(into, label))
          gr.emit(Add(into, into, MaskedLabel(LO_12, label))) // :lo12:label
      }

    case Pair(fst, snd, len) =>
      ra.withNewRegisters2 { (pairReg, _) =>
        val pairX = toX(pairReg).asInstanceOf[X]
        val intoX = toX(into).asInstanceOf[X]
        gr.emit(Mov(RETURN_REG_64, Immediate(PAIR_SIZE)))
        gr.emit(Bl(Label(_MALLOC)))
        gr.emit(Mov(pairX, RETURN_REG_64))

        gr.addPreDefs(_MALLOC)
        gr.addPreDefs(_PRINTS)
        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfMemoryLabel)

        pairCreateHelper(pairX, fst, true, ra)
        pairCreateHelper(pairX, snd, false, ra)
        gr.emit(Mov(intoX, pairX))
      }

    case Array(exprs, len) =>
      val nElems = exprs.length
      val elemSize = len.convertToBytes(A64_WORD_SIZE)
      val totalSize = nElems * elemSize + ARR_HDR

      ra.withNewRegister { elemReg =>
        val intoX = toX(into).asInstanceOf[X]
        gr.emit(Mov(RETURN_REG_64, Immediate(totalSize)))
        gr.emit(Bl(Label(_MALLOC)))
        gr.addPreDefs(_MALLOC)
        gr.addPreDefs(_PRINTS)
        gr.addUsedErrs(PreDefRunTimeError.ErrOutOfMemoryLabel)

        gr.emit(Mov(intoX, RETURN_REG_64))

        gr.emit(Mov(RETURN_REG_32, Immediate(nElems)))
        gr.emit(Str(RETURN_REG_32, MemAddress.ImmOffsetAddress(intoX, 0, NoIndex)))

        gr.emit(Add(intoX, intoX, Immediate(ARR_HDR)))

        exprs.zipWithIndex.foreach { case (e, i) =>
          loadRhs(e, elemReg, ra)

          val offset = i * elemSize
          elemSize match
            case SIZE_CHAR_BOOL_BYTES => gr.emit(Strb(toW(elemReg), MemAddress.ImmOffsetAddress(intoX, offset, NoIndex)))
            case SIZE_INT_BYTES => gr.emit(Str(toW(elemReg), MemAddress.ImmOffsetAddress(intoX, offset, NoIndex)))
            case SIZE_PTR_BYTES => gr.emit(Str(elemReg, MemAddress.ImmOffsetAddress(intoX, offset, NoIndex)))
            case _ => throw new UnsupportedOperationException(s"Unsupported array element size: $elemSize")
        }
      }

    case t @ Temp(_, _) => loadSpill(t, into, cs.ra)

    case it: IndirectTemp =>
      if it.isArray then arrayElemAccess(it, into, it.base, it.offset, true)
      else withPairFieldAddr(it, into, true)
  }

  // Stores a computed register value into a TAC destination.
  def storeTemp(lhs: TAC.Val , from: A64Reg, ra: A64RegisterAllocator)
               (using cs: A64CodegenState, gr: GenRes): Unit = lhs match {
    case t: Temp => storeSpill(t, from, ra)

    case it: IndirectTemp =>
      if it.isArray then arrayElemAccess(it, from, it.base, it.offset, false)
      else withPairFieldAddr(it, from, false)
  }

  // Store a temporary variable to its spill location in the stack frame.
  private def storeSpill(t: Temp, from: A64Reg, ra: A64RegisterAllocator)
                        (using gr: GenRes): Unit = {
    val off = offsetOf(t)
    if (fitsSigned9(off)) {
      // If the offset fits in a signed 9-bit immediate, we can use STUR directly
      gr.emit(Stur(from, MemAddress.ImmOffsetAddress(FP, off, NoIndex)))
    } else {
      ra.withNewRegister { addr =>
        val addrX = toX(addr).asInstanceOf[X]
        materializeAddrFromFP(off, addrX)
        gr.emit(Str(from, MemAddress.BaseRegAddress(addrX)))
      }
    }
  }

  // Load a spilled temporary variable from the stack into a register.
  private def loadSpill(t: Temp, into: A64Reg, ra: A64RegisterAllocator)
               (using gr: GenRes): Unit = {
    val off = offsetOf(t)
    if (fitsSigned9(off)) {
      // Offset fits in 9-bit immediate: load directly
      gr.emit(Ldur(into, MemAddress.ImmOffsetAddress(FP, off, NoIndex)))
    } else {
      ra.withNewRegister { addr =>
        val addrX = toX(addr).asInstanceOf[X]
        materializeAddrFromFP(off, addrX)
        gr.emit(Ldr(into, MemAddress.BaseRegAddress(addrX)))
      }
    }
  }

  // Helper to initialize one element of a pair in memory.
  private def pairCreateHelper(pairReg: X, elem: Rhs, isFirst: Boolean, ra: A64RegisterAllocator)
                              (using cs: A64CodegenState, gr: GenRes): Unit =
    val elemOffset = if (isFirst) FST_OFF else SND_OFF
    ra.withNewRegister { elemReg =>
      loadRhs(elem, elemReg, ra)
      gr.emit(Str(elemReg, MemAddress.ImmOffsetAddress(pairReg, elemOffset, NoIndex)))
    }

  // Emit a runtime null pointer check for an indirect temporary.
  private def emitNullCheck(it: IndirectTemp, baseReg: X, ra: A64RegisterAllocator)
                           (using cs: A64CodegenState, gr: GenRes): Unit = {
    loadRhs(it.base, baseReg, ra)
    gr.emit(Cmp(baseReg, Immediate(NULL_PTR)))
    gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)))

    gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)
  }

  // Generate code to access (load or store) an element of an array.
  private def arrayElemAccess(it: IndirectTemp, reg: A64Reg, base: Rhs, index: Rhs, isLoad: Boolean)
                             (using cs: A64CodegenState, gr: GenRes): Unit = {
    val elemSize = it.len.convertToBytes(A64_WORD_SIZE)
    withArrayElemAddr(base, index, elemSize) { addr =>
      (elemSize, isLoad) match
        case (SIZE_CHAR_BOOL_BYTES, true) =>
          // Load 8-bit value (char/bool) and widen to 32-bit register
          gr.emit(Ldrb(toW(reg), MemAddress.BaseRegAddress(addr)))
        case (SIZE_INT_BYTES, true) =>
          // Load 32-bit integer and widen to 32-bit register
          gr.emit(Ldr(toW(reg), MemAddress.BaseRegAddress(addr)))
        case (SIZE_PTR_BYTES, true) =>
          // Load 64-bit pointer
          gr.emit(Ldr(reg, MemAddress.BaseRegAddress(addr)))
        case (SIZE_CHAR_BOOL_BYTES, false) =>
          // Store 8-bit value (char/bool)
          gr.emit(Strb(toW(reg), MemAddress.BaseRegAddress(addr)))
        case (SIZE_INT_BYTES, false) =>
          // Store 32-bit integer
          gr.emit(Str(toW(reg), MemAddress.BaseRegAddress(addr)))
        case (SIZE_PTR_BYTES, false) =>
          // Store 64-bit pointer
          gr.emit(Str(reg, MemAddress.BaseRegAddress(addr)))
        case _ => throw new UnsupportedOperationException(s"Unsupported array element size: $elemSize")
    }
  }

  // Compute the address of a field within a pair and optionally load or store it.
  private def withPairFieldAddr(it: IndirectTemp, reg: A64Reg, isLoad: Boolean)
                               (using cs: A64CodegenState, gr: GenRes): Unit = {
    cs.ra.withNewRegisters2 { (baseReg, offsetReg) =>
      val baseX = toX(baseReg).asInstanceOf[X]
      val offsetX = toX(offsetReg).asInstanceOf[X]
      emitNullCheck(it, baseX, cs.ra)
      it.offset match {
        case imm: ImmValue => gr.emit(Mov(offsetX, Immediate(imm.value)))
        case tmp: Temp => loadRhs(tmp, offsetX, cs.ra)
      }

      if isLoad then gr.emit(Ldr(reg, MemAddress.RegOffsetAddress(baseX, offsetX)))
      else gr.emit(Str(reg, MemAddress.RegOffsetAddress(baseX, offsetX)))
    }
  }

  // Compute the address of an array element, with runtime null and bounds checks.
  private def withArrayElemAddr[A](base: Rhs, index: Rhs, elemSizeBytes: Int)(k: X => A)
                                  (using cs: A64CodegenState, gr: GenRes): A =
    cs.ra.withNewRegisters6 { (rBase, rIdx, rLen, rMul, rScale, rAddr) =>
      val xBase = toX(rBase).asInstanceOf[X]
      val xMul = toX(rMul).asInstanceOf[X]
      val xScale = toX(rScale).asInstanceOf[X]
      val xAddr = toX(rAddr).asInstanceOf[X]
      val wIdx = toW(rIdx)
      val wLen = toW(rLen)
      val wMul = toW(rMul)

      // base -> xBase
      cs.frame.loadRhs(base, xBase, cs.ra)

      // null check
      gr.emit(Cmp(xBase, Immediate(NULL_PTR)))
      gr.emit(BCond(Cond.EQ, Label(PreDefRunTimeError.ErrNullLabel)))
      gr.addUsedErrs(PreDefRunTimeError.ErrNullLabel)

      // index -> wIdx
      cs.frame.loadRhs(index, wIdx, cs.ra)

      // len -> wLen
      gr.emit(Ldr(wLen, MemAddress.ImmOffsetAddress(xBase, -ARR_HDR, NoIndex)))

      // bounds
      // idx < 0
      gr.emit(Cmp(wIdx, Immediate(0)))
      gr.emit(Mov(RETURN_REG_32, wIdx))
      gr.emit(BCond(Cond.LT, Label(PreDefRunTimeError.ErrOutOfBoundsLabel)))

      // idx >= len
      gr.emit(Cmp(wIdx, wLen))
      gr.emit(Mov(RETURN_REG_32, wIdx))
      gr.emit(BCond(Cond.GE, Label(PreDefRunTimeError.ErrOutOfBoundsLabel)))

      gr.addUsedErrs(PreDefRunTimeError.ErrOutOfBoundsLabel)

      // addr = base + (u64)idx * elemSizeBytes
      gr.emit(Mov(xAddr, xBase))
      gr.emit(Mov(wMul, wIdx)) // write into wMul; AArch64 zeroes upper 32 bits of xMul
      gr.emit(Mov(xScale, Immediate(elemSizeBytes.toLong)))
      gr.emit(Mul(xMul, xMul, xScale))
      gr.emit(Add(xAddr, xAddr, xMul))

      k(xAddr)
    }
}
