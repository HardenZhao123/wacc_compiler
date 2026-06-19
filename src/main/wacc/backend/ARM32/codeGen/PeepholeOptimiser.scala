package wacc.backend.Arm32.codeGen

import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.codeGen.A32MemAddress
import wacc.backend.Arm32.target.{A32Reg, FP}
import wacc.backend.BackendCommon.Immediate
import wacc.backend.BackendCommon.NoIndex
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.PeepholeOptimiserCommon
import wacc.backend.BackendCommon.PeepholeOptimiserCommon.{MemoryAccess, Rewrite}

object A32PeepholeOptimiser {

  private val segmentConfig = PeepholeOptimiserCommon.SegmentConfig[A32Instr](
    isAsmInstr = _.isInstanceOf[AsmInstr],
    isUnconditionalBranch = {
      case _: B => true
      case _    => false
    },
    isConditionalBranch = {
      case _: BCond => true
      case _        => false
    },
    isCall = {
      case _: Bl     => true
      case _: BlCond => true
      case _         => false
    }
  )

  private type A32MemInfo = MemoryAccess[A32Reg, A32MemAddress, MemWidth, Unit]
  def optimise(instructions: List[A32Instr]): List[A32Instr] =
    PeepholeOptimiserCommon.optimiseAsmOnlySegments(instructions, segmentConfig, optimiseBlock)

  // ===========================================================================
  // Address safety / classification
  // ===========================================================================

  // Only optimise memory traffic on simple FP-relative local stack slots.
  // This keeps the pass conservative and avoids aliasing trouble.
  private def isSafeLocalSlotAddr(a: A32MemAddress): Boolean = a match {
    case MemAddress.ImmOffsetAddress(FP, _: Int, NoIndex) => true
    case _                                     => false
  }

  // ===========================================================================
  // Width classification
  // ===========================================================================

  private sealed trait MemWidth
  private object MemWidth {
    case object Byte extends MemWidth
    case object Word extends MemWidth
  }

  // ===========================================================================
  // Memory info helpers
  // ===========================================================================

  private def storeKey(i: A32Instr): Option[(A32MemAddress, MemWidth)] = i match {
    case Str(_, a)  if isSafeLocalSlotAddr(a) => Some((a, MemWidth.Word))
    case Strb(_, a) if isSafeLocalSlotAddr(a) => Some((a, MemWidth.Byte))
    case _                                    => None
  }

  private def loadAddr(i: A32Instr): Option[A32MemAddress] = i match {
    case Ldr(_, a)  if isSafeLocalSlotAddr(a) => Some(a)
    case Ldrb(_, a) if isSafeLocalSlotAddr(a) => Some(a)
    case _                                    => None
  }

  private def storeInfo(i: A32Instr): Option[A32MemInfo] = i match {
    case Str(r, a) if isSafeLocalSlotAddr(a)  => Some(MemoryAccess(r, a, MemWidth.Word, ()))
    case Strb(r, a) if isSafeLocalSlotAddr(a) => Some(MemoryAccess(r, a, MemWidth.Byte, ()))
    case _                                     => None
  }

  private def loadInfo(i: A32Instr): Option[A32MemInfo] = i match {
    case Ldr(r, a) if isSafeLocalSlotAddr(a)  => Some(MemoryAccess(r, a, MemWidth.Word, ()))
    case Ldrb(r, a) if isSafeLocalSlotAddr(a) => Some(MemoryAccess(r, a, MemWidth.Byte, ()))
    case _                                     => None
  }

  private def tryRepeatedLoad(cur: A32Instr, next: A32Instr): Option[Rewrite[A32Instr]] =
    (loadInfo(cur), loadInfo(next)) match {
      case (Some(l1), Some(l2))
        if l1.addr == l2.addr && l1.width == l2.width && l1.reg == l2.reg =>
        // Adjacent identical loads read the same value, so the second is redundant.
        Some(Rewrite(2, List(cur)))

      case _ =>
        None
    }

  // ===========================================================================
  // Pass 2: peephole rewrites
  // ===========================================================================

  private def tryRewrite1(w: Vector[A32Instr], i: Int): Option[Rewrite[A32Instr]] = {
    PeepholeOptimiserCommon
      .rewriteSelfMoveToNoop(w, i, {
        case Mov(dst, src: A32Reg) => dst == src
        case _                     => false
      })
      .orElse {
        w(i) match {
          // add rD, rX, #0  => mov rD, rX   (or delete if same reg)
          case Add(dst, src, Immediate(0)) =>
            if (dst == src) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          // sub rD, rX, #0  => mov rD, rX   (or delete if same reg)
          case Sub(dst, src, Immediate(0)) =>
            if (dst == src) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          // lsl/lsr/asr by 0 => mov
          case Lsl(dst, src, Immediate(0)) =>
            if (dst == src) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          case Lsr(dst, src, Immediate(0)) =>
            if (dst == src) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          case Asr(dst, src, Immediate(0)) =>
            if (dst == src) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          case _ =>
            None
        }
      }
      .orElse {
        if (i + 1 >= w.length) None
        else {
          (w(i), w(i + 1)) match {
            case (Mov(mid, imm: Immediate), Mov(dst, src: A32Reg))
              if src == mid && dst != mid =>
              // Sink the literal directly into the final destination register.
              Some(Rewrite(2, List(Mov(dst, imm))))

            case (LdrFuncPtr(mid, addr), Mov(dst, src: A32Reg))
              if src == mid && dst != mid =>
              // Same idea for literal-pool addresses: retarget the load instead of moving it.
              Some(Rewrite(2, List(LdrFuncPtr(dst, addr))))

            case (Mov(dst1, imm1: Immediate), Mov(dst2, imm2: Immediate))
              if dst1 == dst2 && imm1 == imm2 =>
              Some(Rewrite(2, List(Mov(dst1, imm1))))

            case (Push(List(src)), Pop(List(dst))) =>
              if (src == dst) Some(Rewrite(2, Nil))
              else Some(Rewrite(2, List(Mov(dst, src))))

            case _ =>
              None
          }
        }
      }
  }

  private def tryRewrite2(w: Vector[A32Instr], i: Int): Option[Rewrite[A32Instr]] = {
    if (i + 1 >= w.length) return None

    val cur = w(i)
    val next = w(i + 1)

    // Prefer memory simplifications before generic move cleanups on 2-instr windows.
    tryRepeatedLoad(cur, next)
      .orElse(tryStoreLoadForward(cur, next))
      .orElse(tryMoveChain(cur, next))
      .orElse(tryRedundantMoveOverwrite(cur, next))
  }

  private def isNonClobberingStore(i: A32Instr): Boolean = i match {
    case Str(_, MemAddress.ImmOffsetAddress(_, _, NoIndex))  => true
    case Strb(_, MemAddress.ImmOffsetAddress(_, _, NoIndex)) => true
    case Str(_, MemAddress.BaseRegAddress(_))                => true
    case Strb(_, MemAddress.BaseRegAddress(_))               => true
    case Str(_, MemAddress.RegOffsetAddress(_, _))           => true
    case Strb(_, MemAddress.RegOffsetAddress(_, _))          => true
    case _                                                   => false
  }

  private def tryRewrite3(w: Vector[A32Instr], i: Int): Option[Rewrite[A32Instr]] = {
    if (i + 2 >= w.length) return None

    (w(i), w(i + 1), w(i + 2)) match {
      case (Mov(tmp, Immediate(0)), Cmp(lhs, rhs: A32Reg), third)
        if rhs == tmp =>
        // Keep the zero materialisation in case tmp is still needed after the compare.
        Some(Rewrite(3, List(Mov(tmp, Immediate(0)), Cmp(lhs, Immediate(0)), third)))

      case (Mov(dst1, imm1: Immediate), middle, Mov(dst2, imm2: Immediate))
        if dst1 == dst2 && imm1 == imm2 && isNonClobberingStore(middle) =>
        Some(Rewrite(3, List(Mov(dst1, imm1), middle)))

      case _ =>
        None
    }
  }

  private def tryRewriteN(w: Vector[A32Instr], i: Int): Option[Rewrite[A32Instr]] = {
    w(i) match {
      case first @ Mov(dst1, imm1: Immediate) =>
        var j = i + 1
        while (j < w.length && isNonClobberingStore(w(j))) {
          // These stores read registers / addresses but do not overwrite dst1.
          j += 1
        }

        if (j < w.length) {
          w(j) match {
            case Mov(dst2, imm2: Immediate) if dst1 == dst2 && imm1 == imm2 && j > i + 2 =>
              Some(Rewrite(j - i + 1, first :: w.slice(i + 1, j).toList))
            case _ =>
              None
          }
        } else None

      case _ =>
        None
    }
  }

  // str/strb [slot] ; ldr/ldrb same [slot]
  // =>
  // word: store only if same reg, else store ; mov loadReg, storeReg
  // byte: must preserve ldrb zero-extension, so rewrite to and #255 instead of mov
  private def tryStoreLoadForward(cur: A32Instr, next: A32Instr): Option[Rewrite[A32Instr]] =
    (storeInfo(cur), loadInfo(next)) match {
      case (
        Some(MemoryAccess(src, addr1, MemWidth.Word, ())),
        Some(MemoryAccess(dst, addr2, MemWidth.Word, ()))
        ) if addr1 == addr2 =>
        if (dst == src) Some(Rewrite(2, List(cur)))
        else Some(Rewrite(2, List(cur, Mov(dst, src))))

      case (
        Some(MemoryAccess(src, addr1, MemWidth.Byte, ())),
        Some(MemoryAccess(dst, addr2, MemWidth.Byte, ()))
        ) if addr1 == addr2 =>
        // LDRB also zero-extends on ARM32, so preserve that with an AND mask.
        Some(Rewrite(2, List(cur, And(dst, src, Immediate(255)))))

      case _ =>
        None
    }

  // mov mid, src ; mov dst, mid  => mov mid, src ; mov dst, src
  // Helps shorten dependency chains and may expose mov r, r later.
  private def tryMoveChain(cur: A32Instr, next: A32Instr): Option[Rewrite[A32Instr]] = {
    (cur, next) match {
      case (Mov(mid, src: A32Reg), Mov(dst, src2: A32Reg))
        if src2 == mid && dst != mid =>
        Some(Rewrite(2, List(Mov(mid, src), Mov(dst, src))))

      case _ =>
        None
    }
  }

  // mov a, x ; mov a, y  => mov a, y
  // because the first assignment is immediately overwritten
  private def tryRedundantMoveOverwrite(cur: A32Instr, next: A32Instr): Option[Rewrite[A32Instr]] = {
    (cur, next) match {
      case (Mov(dst1, _), Mov(dst2, src2))
        if dst1 == dst2 =>
        Some(Rewrite(2, List(Mov(dst2, src2))))

      case _ =>
        None
    }
  }

  private def optimiseBlock(block: List[A32Instr]): List[A32Instr] = {
    val kept = PeepholeOptimiserCommon.eliminateDeadStores(block.toVector, loadAddr, storeKey)

    PeepholeOptimiserCommon.applyRewriteRulesInOrder(
      kept,
      List(tryRewrite1, tryRewriteN, tryRewrite3, tryRewrite2)
    )
  }
}
