package wacc.backend.BackendCommon

import scala.collection.mutable

object PeepholeOptimiserCommon {

  // Result of applying one peephole rewrite at a given position.
  final case class Rewrite[I](consumed: Int, emitted: List[I])
  type RewriteRule[I] = (Vector[I], Int) => Option[Rewrite[I]]

  // Predicates describing how to segment an instruction stream for safe optimisation.
  final case class SegmentConfig[I](
    isAsmInstr: I => Boolean,
    isUnconditionalBranch: I => Boolean,
    isConditionalBranch: I => Boolean,
    isCall: I => Boolean,
    isReturn: I => Boolean = (_: I) => false
  ) {
    // These instructions split optimisation windows even if control may later rejoin.
    def isBlockTerminatorOrBarrier(i: I): Boolean =
      isUnconditionalBranch(i) ||
        isConditionalBranch(i) ||
        isCall(i) ||
        isReturn(i)
  }

  // Canonical memory-access shape used by shared load/store rewrite passes.
  final case class MemoryAccess[R, Addr, Width, Kind](
    reg: R,
    addr: Addr,
    width: Width,
    kind: Kind
  )

  // Shared "optimise over asm blocks only" template:
  // split stream into segments, optimise asm-only blocks, keep non-asm segments intact.
  def optimiseAsmOnlySegments[I](
    instructions: List[I],
    config: SegmentConfig[I],
    optimiseAsmBlock: List[I] => List[I]
  ): List[I] = {
    val segments = dropUnreachableAsmSegments(splitSegments(instructions, config), config)
    segments.flatMap { seg =>
      if (seg.nonEmpty && seg.forall(config.isAsmInstr)) optimiseAsmBlock(seg)
      else seg
    }
  }

  def splitSegments[I](
    instrs: List[I],
    config: SegmentConfig[I]
  ): List[List[I]] = {
    val out = List.newBuilder[List[I]]
    val cur = List.newBuilder[I]

    def flushCur(): Unit = {
      val b = cur.result()
      cur.clear()
      if (b.nonEmpty) out += b
    }

    instrs.foreach { ins =>
      if (!config.isAsmInstr(ins)) {
        // Labels / directives stay isolated so rewrites never cross them.
        flushCur()
        out += List(ins)
      } else {
        cur += ins
        // Terminators end the current asm block immediately.
        if (config.isBlockTerminatorOrBarrier(ins)) flushCur()
      }
    }

    flushCur()
    out.result()
  }

  // After an unconditional branch or return, any following asm-only segments are
  // unreachable until the next non-asm segment (typically a label / raw / data).
  // This sits above per-block rewrites because splitSegments already separates
  // barriers into distinct asm runs.
  def dropUnreachableAsmSegments[I](
    segments: List[List[I]],
    config: SegmentConfig[I]
  ): List[List[I]] = {
    val out = List.newBuilder[List[I]]
    var droppingAsm = false

    segments.foreach { seg =>
      if (seg.nonEmpty && seg.forall(config.isAsmInstr)) {
        if (!droppingAsm) {
          out += seg
          val last = seg.last
          // Once control definitely leaves, later asm-only segments are unreachable noise.
          droppingAsm = config.isUnconditionalBranch(last) || config.isReturn(last)
        }
      } else {
        out += seg
        droppingAsm = false
      }
    }

    out.result()
  }

  // Dead-store elimination parameterized by address/key extractors.
  def eliminateDeadStores[I, Addr, Width](
    block: Vector[I],
    loadAddr: I => Option[Addr],
    storeKey: I => Option[(Addr, Width)]
  ): Vector[I] = {
    val deleted = mutable.BitSet.empty
    val lastStoreAt = mutable.HashMap.empty[(Addr, Width), Int]

    block.zipWithIndex.foreach { case (ins, idx) =>
      loadAddr(ins) match {
        case Some(a) =>
          // A load from the same address makes prior stores observable.
          lastStoreAt.keys
            .filter { case (addr, _) => addr == a }
            .foreach(lastStoreAt.remove)

        case None =>
          storeKey(ins) match {
            case Some(k) =>
              // A later store to the same slot and width makes the earlier one dead.
              lastStoreAt.get(k).foreach(deleted += _)
              lastStoreAt.update(k, idx)

            case None =>
              ()
          }
      }
    }

    block.zipWithIndex.collect { case (ins, idx) if !deleted.contains(idx) => ins }.toVector
  }

  // Generic one-instruction rewrite for "move to self" patterns.
  def rewriteSelfMoveToNoop[I](
    w: Vector[I],
    i: Int,
    isSelfMove: I => Boolean
  ): Option[Rewrite[I]] =
    if (isSelfMove(w(i))) Some(Rewrite(1, Nil))
    else None

  // Generic two-instruction store/load forwarding:
  //   store rS, [addr]; load rL, [addr] => store; mov rL, rS
  // with width/kind checks, and removes load if rL == rS.
  def rewriteAdjacentStoreLoadToMov[I, R, Addr, Width, Kind](
    cur: I,
    next: I,
    storeAccess: I => Option[MemoryAccess[R, Addr, Width, Kind]],
    loadAccess: I => Option[MemoryAccess[R, Addr, Width, Kind]],
    makeMov: (R, R) => I
  ): Option[Rewrite[I]] = {
    (storeAccess(cur), loadAccess(next)) match {
      case (Some(s), Some(l)) if s.addr == l.addr && s.width == l.width && s.kind == l.kind =>
        if (l.reg == s.reg) Some(Rewrite(2, List(cur)))
        else Some(Rewrite(2, List(cur, makeMov(l.reg, s.reg))))

      case _ =>
        None
    }
  }

  // Apply rewrite rules in priority order during one left-to-right scan.
  def applyRewriteRulesInOrder[I](
    kept: Vector[I],
    rewriteRulesInPriorityOrder: List[RewriteRule[I]]
  ): List[I] = {
    val out = List.newBuilder[I]
    var i = 0

    while (i < kept.length) {
      rewriteRulesInPriorityOrder
        .view
        .flatMap(rule => rule(kept, i))
        // Earlier rules win, so later ones only see still-unmatched windows.
        .headOption
        .fold {
          out += kept(i)
          i += 1
        } { rw =>
          rw.emitted.foreach(out += _)
          i += rw.consumed
        }
    }

    out.result()
  }
}
