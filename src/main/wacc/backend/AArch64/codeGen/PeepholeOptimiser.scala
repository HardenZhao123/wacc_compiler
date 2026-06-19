package wacc.backend.AArch64.codeGen

import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.AArch64.codeGen.A64MemAddress
import wacc.backend.AArch64.target.{A64Reg, FP, Reg, SP, XZR}
import wacc.backend.BackendCommon.Immediate
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.NoIndex
import wacc.backend.BackendCommon.{PreIndex, PostIndex}
import wacc.backend.BackendCommon.MemAddress
import wacc.backend.BackendCommon.PeepholeOptimiserCommon
import wacc.backend.BackendCommon.PeepholeOptimiserCommon.{MemoryAccess, Rewrite}

// AArch64-specific peephole rewrites layered on top of the shared optimiser helpers.
object A64PeepholeOptimiser {

  private val segmentConfig = PeepholeOptimiserCommon.SegmentConfig[A64Instr](
    isAsmInstr = _.isInstanceOf[AsmInstr],
    isUnconditionalBranch = {
      case _: B => true
      case _    => false
    },
    isConditionalBranch = {
      case _: BCond => true
      case _: Cbz   => true
      case _: Cbnz  => true
      case _        => false
    },
    isCall = {
      case _: Bl => true
      case _     => false
    },
    isReturn = {
      case Ret => true
      case _   => false
    }
  )

  // Canonical AArch64 memory-access descriptor used by the rewrite helpers below.
  private type A64MemInfo = MemoryAccess[A64Reg, A64MemAddress, StoreWidth, MemKind]

  // Optimises the instruction stream block-by-block while respecting label/data boundaries.
  def optimise(instructions: List[A64Instr]): List[A64Instr] =
    PeepholeOptimiserCommon.optimiseAsmOnlySegments(instructions, segmentConfig, optimiseBlock)

  // Address safety / classification

  // Conservative: only FP-relative simple stack slots.
  // Use for DSE / forwarding / 4-instr load-store-load-store -> stp rewrite.
  private def isSafeLocalSlotAddr(a: A64MemAddress): Boolean = a match {
    case MemAddress.ImmOffsetAddress(FP, _: Int, NoIndex) => true
    case _                                                => false
  }

  // More permissive: any simple base + immediate address.
  // Use for pair-store formation on heap/object writes too.
  private def isPairableAddr(a: A64MemAddress): Boolean = a match {
    case MemAddress.ImmOffsetAddress(_: A64Reg, _: Int, NoIndex) => true
    case _                                                       => false
  }

  private def immNoIndexParts(a: A64MemAddress): Option[(A64Reg, Int)] = a match {
    case MemAddress.ImmOffsetAddress(base: A64Reg, off: Int, NoIndex) => Some((base, off))
    case _                                                             => None
  }

  // Extracts the base register used by a pairable store address.
  private def pairStoreBase(a: A64MemAddress): Option[A64Reg] = a match {
    case MemAddress.ImmOffsetAddress(base: A64Reg, _: Int, NoIndex) => Some(base)
    case _                                                           => None
  }

  // Width classification
  private sealed trait StoreWidth { def bytes: Int }
  private object StoreWidth {
    case object B extends StoreWidth { val bytes: Int = 1 }
    case object W extends StoreWidth { val bytes: Int = 4 }
    case object X extends StoreWidth { val bytes: Int = 8 }
  }

  private def regWidth(r: A64Reg): StoreWidth = r match {
    case _: wacc.backend.AArch64.target.W => StoreWidth.W
    case _                                => StoreWidth.X
  }

  // Memory info helpers
  private sealed trait MemKind
  private object MemKind {
    case object Normal extends MemKind   // Str / Ldr
    case object Unscaled extends MemKind // Stur / Ldur
  }

  private def storeKey(i: A64Instr): Option[(A64MemAddress, StoreWidth)] = i match {
    case Strb(_: wacc.backend.AArch64.target.W, a) if isSafeLocalSlotAddr(a) => Some((a, StoreWidth.B))
    case Str(r: A64Reg, a) if isSafeLocalSlotAddr(a)  => Some((a, regWidth(r)))
    case Stur(r: A64Reg, a) if isSafeLocalSlotAddr(a) => Some((a, regWidth(r)))
    case _                                            => None
  }

  // Extracts the address read by a load instruction when it is safe for local-slot rewrites.
  private def loadAddr(i: A64Instr): Option[A64MemAddress] = i match {
    case Ldrb(_: wacc.backend.AArch64.target.W, a) if isSafeLocalSlotAddr(a) => Some(a)
    case Ldr(_, a) if isSafeLocalSlotAddr(a)  => Some(a)
    case Ldur(_, a) if isSafeLocalSlotAddr(a) => Some(a)
    case _                                    => None
  }

  // Normalises a store instruction into the common memory-access descriptor.
  private def storeInfo(i: A64Instr): Option[A64MemInfo] = i match {
    case Str(r: A64Reg, a) if isSafeLocalSlotAddr(a)  => Some(MemoryAccess(r, a, regWidth(r), MemKind.Normal))
    case Stur(r: A64Reg, a) if isSafeLocalSlotAddr(a) => Some(MemoryAccess(r, a, regWidth(r), MemKind.Unscaled))
    case _                                            => None
  }

  // Normalises a load instruction into the common memory-access descriptor.
  private def loadInfo(i: A64Instr): Option[A64MemInfo] = i match {
    case Ldr(r: A64Reg, a) if isSafeLocalSlotAddr(a)  => Some(MemoryAccess(r, a, regWidth(r), MemKind.Normal))
    case Ldur(r: A64Reg, a) if isSafeLocalSlotAddr(a) => Some(MemoryAccess(r, a, regWidth(r), MemKind.Unscaled))
    case _                                            => None
  }

  // Describes byte stores that are eligible for adjacent forwarding rewrites.
  private def byteStoreInfo(i: A64Instr): Option[MemoryAccess[wacc.backend.AArch64.target.W, A64MemAddress, StoreWidth, MemKind]] = i match {
    case Strb(r: wacc.backend.AArch64.target.W, a) if isSafeLocalSlotAddr(a) =>
      Some(MemoryAccess(r, a, StoreWidth.B, MemKind.Normal))
    case _ =>
      None
  }

  // Describes byte loads that are eligible for adjacent forwarding rewrites.
  private def byteLoadInfo(i: A64Instr): Option[MemoryAccess[wacc.backend.AArch64.target.W, A64MemAddress, StoreWidth, MemKind]] = i match {
    case Ldrb(r: wacc.backend.AArch64.target.W, a) if isSafeLocalSlotAddr(a) =>
      Some(MemoryAccess(r, a, StoreWidth.B, MemKind.Normal))
    case _ =>
      None
  }

  // Normalises stores that are eligible for STP pairing, including heap/object writes.
  private def pairableStoreInfo(i: A64Instr): Option[A64MemInfo] = i match {
    case Str(r: A64Reg, a) if isPairableAddr(a)  => Some(MemoryAccess(r, a, regWidth(r), MemKind.Normal))
    case Stur(r: A64Reg, a) if isPairableAddr(a) => Some(MemoryAccess(r, a, regWidth(r), MemKind.Unscaled))
    case _                                       => None
  }

  // Checks whether two stores can be combined into a single `stp`.
  private def canFormStorePair(
    s1: A64MemInfo,
    s2: A64MemInfo
  ): Option[(A64Reg, A64Reg, A64MemAddress)] = {
    val MemoryAccess(r1, a1, w1, k1) = s1
    val MemoryAccess(r2, a2, w2, k2) = s2

    if (k1 != MemKind.Normal || k2 != MemKind.Normal) return None
    if (w1 != w2) return None

    (immNoIndexParts(a1), immNoIndexParts(a2)) match {
      // Keep the first address as the STP base; the second must be exactly one element later.
      case (Some((base1, off1)), Some((base2, off2)))
        if base1 == base2 && off2 == off1 + w1.bytes =>
        Some((r1, r2, MemAddress.ImmOffsetAddress(base1, off1, NoIndex)))

      case _ =>
        None
    }
  }

  // Local rewrite helpers
  private def sameReg(a: A64Reg, b: A64Reg): Boolean = Reg.samePhysicalReg(a, b)
  private def sameWidth(a: A64Reg, b: A64Reg): Boolean = regWidth(a) == regWidth(b)

  // Canonicalises mixed-width move operands before comparing physical registers.
  private def normaliseMovRegs(dst: A64Reg, src: A64Reg): (A64Reg, A64Reg) = (dst, src) match {
    // Canonicalise mixed X/W views so physical-register comparisons stay sound.
    case (dx: wacc.backend.AArch64.target.X, sw: wacc.backend.AArch64.target.W) =>
      (Reg.toW(dx), sw)
    case (dw: wacc.backend.AArch64.target.W, sx: wacc.backend.AArch64.target.X) =>
      (dw, Reg.toW(sx))
    case _ =>
      (dst, src)
  }

  private def isZeroMove(i: A64Instr): Option[A64Reg] = i match {
    case Mov(r: A64Reg, Immediate(0)) => Some(r)
    case _                            => None
  }

  // Recognises the synthetic push sequence used for one-register save/restore rewrites.
  private def isSinglePushPair(i: A64Instr): Option[A64Reg] = i match {
    case Stp(src, XZR, MemAddress.ImmOffsetAddress(SP, -16, PreIndex)) => Some(src)
    case _                                                              => None
  }

  // Recognises the synthetic pop sequence used for one-register save/restore rewrites.
  private def isSinglePopPair(i: A64Instr): Option[A64Reg] = i match {
    case Ldp(dst, XZR, MemAddress.ImmOffsetAddress(SP, 16, PostIndex)) => Some(dst)
    case _                                                              => None
  }

  // Replaces adjacent store/load pairs with a move when the slot value is unchanged.
  private def tryStoreLoadForward(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] =
    PeepholeOptimiserCommon.rewriteAdjacentStoreLoadToMov(
      cur,
      next,
      storeInfo,
      loadInfo,
      (dst: A64Reg, src: A64Reg) => Mov(dst, src)
    )

  // Rewrites byte store/load pairs while preserving zero-extension semantics.
  private def tryByteStoreLoadForward(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] =
    (byteStoreInfo(cur), byteLoadInfo(next)) match {
      case (Some(s), Some(l)) if s.addr == l.addr =>
        // LDRB zero-extends, so replacing it with MOV would be wrong for dirty upper bits.
        Some(Rewrite(2, List(cur, And(l.reg, s.reg, Immediate(255)))))

      case _ =>
        None
    }

  // Removes duplicated adjacent loads from the same safe address.
  private def tryRepeatedLoad(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] =
    (loadInfo(cur), loadInfo(next), byteLoadInfo(cur), byteLoadInfo(next)) match {
      case (Some(l1), Some(l2), _, _)
        if l1.addr == l2.addr && l1.width == l2.width && l1.kind == l2.kind && l1.reg == l2.reg =>
        // Nothing can change the slot between adjacent identical loads.
        Some(Rewrite(2, List(cur)))

      case (_, _, Some(l1), Some(l2))
        if l1.addr == l2.addr && l1.reg == l2.reg =>
        Some(Rewrite(2, List(cur)))

      case _ =>
        None
    }

  // Propagates register moves through short move chains.
  private def tryMoveChain(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] = {
    (cur, next) match {
      case (Mov(mid, src: A64Reg), Mov(dst, src2: A64Reg)) =>
        val (midN, srcN) = normaliseMovRegs(mid, src)
        val (dstN, src2N) = normaliseMovRegs(dst, src2)

        if (sameReg(src2N, midN) && sameWidth(src2N, midN) && !sameReg(dstN, midN))
          Some(Rewrite(2, List(Mov(midN, srcN), Mov(dstN, srcN))))
        else None

      case _ =>
        None
    }
  }

  // Removes earlier moves that are immediately overwritten before use.
  private def tryRedundantMoveOverwrite(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] = {
    (cur, next) match {
      case (Mov(dst1, src1: A64Reg), Mov(dst2, src2)) =>
        val (dst1N, _) = normaliseMovRegs(dst1, src1)
        src2 match {
          case src2Reg: A64Reg =>
            val (dst2N, src2N) = normaliseMovRegs(dst2, src2Reg)
            if (sameReg(dst1N, dst2N)) Some(Rewrite(2, List(Mov(dst2N, src2N))))
            else None
          case _ =>
            if (sameReg(dst1N, dst2)) Some(Rewrite(2, List(Mov(dst2, src2))))
            else None
        }

      case (Mov(dst1, _), Mov(dst2, src2))
        if sameReg(dst1, dst2) =>
        Some(Rewrite(2, List(Mov(dst2, src2))))

      case _ =>
        None
    }
  }

  // Collapses a push/pop roundtrip into either nothing or a single move.
  private def tryPushPopRoundtrip(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] = {
    (isSinglePushPair(cur), isSinglePopPair(next)) match {
      case (Some(src), Some(dst)) =>
        if (sameReg(src, dst)) Some(Rewrite(2, Nil))
        else Some(Rewrite(2, List(Mov(dst, src))))

      case _ =>
        None
    }
  }

  // Replaces move-via-register of an immediate with a direct immediate move.
  private def tryImmediateMovePropagation(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] = {
    (cur, next) match {
      case (Mov(mid, imm: Immediate), Mov(dst, src: A64Reg))
        if sameReg(src, mid) && sameWidth(src, mid) && !sameReg(dst, mid) =>
        // Replace the second move with the original literal and drop the temporary.
        Some(Rewrite(2, List(Mov(dst, imm))))

      case _ =>
        None
    }
  }

  // Recognises stores that cannot clobber a register-held immediate value.
  private def isNonClobberingStore(i: A64Instr): Boolean = i match {
    case Str(_, MemAddress.ImmOffsetAddress(_, _, NoIndex))  => true
    case Stur(_, MemAddress.ImmOffsetAddress(_, _, NoIndex)) => true
    case Strb(_, MemAddress.ImmOffsetAddress(_, _, NoIndex)) => true
    case Str(_, MemAddress.BaseRegAddress(_))                => true
    case Stur(_, MemAddress.BaseRegAddress(_))               => true
    case Strb(_, MemAddress.BaseRegAddress(_))               => true
    case _                                                   => false
  }

  // Removes repeated immediate materialisations separated only by non-clobbering stores.
  private def tryRepeatedImmediateMoveAcrossStores(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] = {
    w(i) match {
      case first @ Mov(dst1, imm1: Immediate) =>
        val emitted = List.newBuilder[A64Instr]
        emitted += first

        var j = i + 1
        var removedAny = false
        var done = false

        while (j < w.length && !done) {
          w(j) match {
            case store if isNonClobberingStore(store) =>
              // Pure stores do not disturb the immediate already held in dst1.
              emitted += store
              j += 1

            case Mov(dst2, imm2: Immediate)
              if sameReg(dst1, dst2) && sameWidth(dst1, dst2) && imm1 == imm2 =>
              removedAny = true
              j += 1

            case _ =>
              done = true
          }
        }

        if (removedAny) Some(Rewrite(j - i, emitted.result()))
        else None

      case _ =>
        None
    }
  }

  // Applies the core two-instruction move/load/store rewrite set.
  private def trySimpleMoveRules(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] =
    tryRepeatedLoad(cur, next)
      .orElse(tryByteStoreLoadForward(cur, next))
      .orElse(tryStoreLoadForward(cur, next))
      .orElse(tryImmediateMovePropagation(cur, next))
      .orElse(tryPushPopRoundtrip(cur, next))
      .orElse(tryMoveChain(cur, next))
      .orElse(tryRedundantMoveOverwrite(cur, next))

  // Collapses two adjacent stores into one `stp` when the addresses line up.
  private def tryPairStoreRewrite2(cur: A64Instr, next: A64Instr): Option[Rewrite[A64Instr]] =
    (pairableStoreInfo(cur), pairableStoreInfo(next)) match {
      case (Some(s1), Some(s2)) =>
        canFormStorePair(s1, s2) match {
          // Only plain STR pairs are eligible; STUR stays out because STP is scaled.
          case Some((r1, r2, addr)) => Some(Rewrite(2, List(Stp(r1, r2, addr))))
          case None                 => None
        }

      case _ =>
        None
    }

  // Pass 2: Peephole rewrites
  // Algebraic simplifications (single instruction)
  private def tryAlgebraicRewrite1(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] =
    PeepholeOptimiserCommon
      .rewriteSelfMoveToNoop(w, i, {
        case Mov(dst, src: A64Reg) => dst == src
        case _                     => false
      })
      .orElse {
        w(i) match {
          case Add(dst, src, Immediate(0)) =>
            if (sameReg(dst, src)) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          case Sub(dst, src, Immediate(0)) =>
            if (sameReg(dst, src)) Some(Rewrite(1, Nil))
            else Some(Rewrite(1, List(Mov(dst, src))))

          case _ =>
            None
        }
      }

  // Branch selection (two instructions)
  private def tryBranchRewrite2(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] = {
    if (i + 1 >= w.length) return None

    (w(i), w(i + 1)) match {
      case (Cmp(reg, Immediate(0), false), BCond(Cond.EQ, lab)) =>
        Some(Rewrite(2, List(Cbz(reg, lab))))

      case (Cmp(reg, Immediate(0), false), BCond(Cond.NE, lab)) =>
        Some(Rewrite(2, List(Cbnz(reg, lab))))

      case _ =>
        None
    }
  }

  // Local move / forwarding / pair-store rewrites (two instructions)
  private def tryLocalRewrite2(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] = {
    if (i + 1 >= w.length) return None

    val cur = w(i)
    val next = w(i + 1)

    // Branch compression runs before generic move/store rewrites on the same window.
    tryBranchRewrite2(w, i)
      .orElse(tryPairStoreRewrite2(cur, next))
      .orElse(trySimpleMoveRules(cur, next))
  }

  // Branch selection using an explicit zero register materialisation
  private def tryBranchRewrite3(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] = {
    if (i + 2 >= w.length) return None

    val i0 = w(i)
    val i1 = w(i + 1)
    val i2 = w(i + 2)

    (isZeroMove(i0), i1, i2) match {
      case (Some(rZ), Cmp(rX, rCmp: A64Reg, false), BCond(Cond.EQ, lab))
        if sameReg(rCmp, rZ) =>
        // Keep the zero move because the temporary may still be live after this branch.
        Some(Rewrite(3, List(i0, Cbz(rX, lab))))

      case (Some(rZ), Cmp(rX, rCmp: A64Reg, false), BCond(Cond.NE, lab))
        if sameReg(rCmp, rZ) =>
        Some(Rewrite(3, List(i0, Cbnz(rX, lab))))

      case _ =>
        None
    }
  }

  // Folds `adrp; add; mov` into a direct `adrp; add` sequence on the final destination register.
  private def tryAdrpAddMoveRewrite3(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] = {
    if (i + 2 >= w.length) return None

    (w(i), w(i + 1), w(i + 2)) match {
      case (Adrp(base1, label1), Add(addr, base2, low @ MaskedLabel(_, label2)), Mov(dst, src: A64Reg))
        if label1 == label2 && sameReg(base1, base2) && sameReg(src, addr) && !sameReg(dst, addr) =>
        Some(Rewrite(3, List(Adrp(dst, label1), Add(dst, dst, low))))

      case _ =>
        None
    }
  }

  // Memory traffic reduction across four instructions
  private def tryPairStoreRewrite4(w: Vector[A64Instr], i: Int): Option[Rewrite[A64Instr]] = {
    if (i + 3 >= w.length) return None

    val i0 = w(i)
    val i1 = w(i + 1)
    val i2 = w(i + 2)
    val i3 = w(i + 3)

    (loadInfo(i0), pairableStoreInfo(i1), loadInfo(i2), pairableStoreInfo(i3)) match {
      case (
        Some(l1),
        Some(s1 @ MemoryAccess(_, _, _, MemKind.Normal)),
        Some(l2),
        Some(s2 @ MemoryAccess(_, _, _, MemKind.Normal))
        )
        if isSafeLocalSlotAddr(l1.addr) &&
          isSafeLocalSlotAddr(l2.addr) &&
          l1.reg == s1.reg &&
          l2.reg == s2.reg &&
          l1.reg != l2.reg &&
          l1.width == s1.width &&
          l2.width == s2.width =>

        canFormStorePair(s1, s2) match {
          case Some((pr1, pr2, pAddr)) =>
            pairStoreBase(pAddr) match {
              // Skip FP destinations here: stack locals are handled conservatively elsewhere.
              case Some(base) if base != FP =>
                Some(Rewrite(4, List(i0, i2, Stp(pr1, pr2, pAddr))))
              case _ =>
                None
            }

          case None =>
            None
        }

      case _ =>
        None
    }
  }

  private def optimiseBlock(block: List[A64Instr]): List[A64Instr] = {
    val kept = PeepholeOptimiserCommon.eliminateDeadStores(block.toVector, loadAddr, storeKey)

    PeepholeOptimiserCommon.applyRewriteRulesInOrder(
      kept,
      List(tryAlgebraicRewrite1, tryRepeatedImmediateMoveAcrossStores, tryPairStoreRewrite4, tryAdrpAddMoveRewrite3, tryBranchRewrite3, tryLocalRewrite2)
    )
  }
}
