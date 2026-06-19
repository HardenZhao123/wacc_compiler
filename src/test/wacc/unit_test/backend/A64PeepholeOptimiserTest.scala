package wacc.unit_test.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.backend.AArch64.codeGen.A64Instr
import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.AArch64.codeGen.A64PeepholeOptimiser
import wacc.backend.AArch64.codeGen.A64MemAddress
import wacc.backend.BackendCommon.MemAddress
import wacc.backend.AArch64.target.Reg.*
import wacc.backend.AArch64.target.{FP, SP, XZR}
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.Immediate
import wacc.backend.BackendCommon.*
import wacc.backend.BackendCommon.AsmEntity.*

final class A64PeepholeOptimiserTest extends AnyFlatSpec with Matchers {

  it should "rewrite add/sub by zero into mov or remove it when the destination is unchanged" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Add(X3, X4, Immediate(0)),
      Sub(X5, X5, Immediate(0)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X3, X4),
      Ret
    )
  }

  it should "shorten move chains" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X1, X2),
      Mov(X3, X1),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X1, X2),
      Mov(X3, X2),
      Ret
    )
  }

  it should "remove overwritten moves to the same destination" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X1, X2),
      Mov(X1, X3),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X1, X3),
      Ret
    )
  }

  it should "handle special registers when removing overwritten moves" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(FP, SP),
      Mov(FP, X0),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(FP, X0),
      Ret
    )
  }

  it should "remove consecutive self-moves inside a block" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X0, X0),
      Mov(X1, Immediate(7)),
      Mov(W2, W2),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X1, Immediate(7)),
      Ret
    )
  }

  it should "NOT peek across Label (self-move after label is removed, but nothing crosses the label)" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X1, Immediate(1)),
      Label("L1"),
      Mov(X0, X0), // should be removed
      Mov(X2, Immediate(2)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X1, Immediate(1)),
      Label("L1"),
      Mov(X2, Immediate(2)),
      Ret
    )
  }

  it should "NOT peek across Branch terminator (blocks split after B)" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X0, X0), // removed
      B(Label("L1")), // terminator => block ends
      Mov(X1, X1), // removed in next block independently
      Label("L1"),
      Mov(X2, Immediate(3)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      B(Label("L1")),
      Label("L1"),
      Mov(X2, Immediate(3)),
      Ret
    )
  }

  it should "remove truly unreachable normal instructions after unconditional B before the next label" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      B(Label("L1")),
      Mov(X0, X1),
      Mov(X2, X3),
      Label("L1"),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      B(Label("L1")),
      Label("L1"),
      Ret
    )
  }

  it should "remove dead stores when the same address is stored multiple times with no intervening load" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Stur(X9, slot),  // dead
      Mov(X10, Immediate(1)),
      Stur(X10, slot), // dead
      Mov(X11, Immediate(2)),
      Stur(X11, slot), // kept
      Ldur(X12, slot), // forwarded to mov
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Mov(X10, Immediate(1)),
      Mov(X11, Immediate(2)),
      Stur(X11, slot),
      Mov(X12, X11),
      Ret
    )
  }

  it should "NOT remove a store if there is a load from that address before the overwrite" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Stur(X9, slot),   // must stay
      Ldur(X12, slot),  // forwarded
      Mov(X10, Immediate(1)),
      Stur(X10, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Stur(X9, slot),
      Mov(X12, X9),
      Mov(X10, Immediate(1)),
      Stur(X10, slot),
      Ret
    )
  }

  it should "NOT peek across call barriers: do not delete a store before Bl even if the same address is stored after" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Stur(X9, slot),
      Bl(Label("_printi")),
      Mov(X10, Immediate(1)),
      Stur(X10, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT remove a W-store when it is overwritten by an X-store to the same address (width mismatch)" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W9, Immediate(123)),
      Stur(W9, slot),
      Mov(X10, Immediate(456)),
      Stur(X10, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT remove stores using PreIndex/PostIndex addressing (base writeback makes it unsafe)" in {
    val pre: A64MemAddress = MemAddress.ImmOffsetAddress(SP, -16, PreIndex)
    val post: A64MemAddress = MemAddress.ImmOffsetAddress(SP, 16, PostIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Stur(X9, pre),
      Mov(X10, Immediate(1)),
      Stur(X10, pre),
      Mov(X11, Immediate(2)),
      Stur(X11, post),
      Mov(X12, Immediate(3)),
      Stur(X12, post),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "rewrite cmp reg, #0 followed by b.eq into cbz" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Cmp(X9, Immediate(0)),
      BCond(Cond.EQ, Label("L1")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Cbz(X9, Label("L1")),
      Ret
    )
  }

  it should "rewrite cmp reg, #0 followed by b.ne into cbnz" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Cmp(W10, Immediate(0)),
      BCond(Cond.NE, Label("L2")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Cbnz(W10, Label("L2")),
      Ret
    )
  }

  it should "NOT rewrite cmp reg, #1 followed by conditional branch" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Cmp(X9, Immediate(1)),
      BCond(Cond.EQ, Label("L1")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT rewrite cmp generated for smull overflow check" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Cmp(X9, W9, true),
      BCond(Cond.NE, Label("_errOverflow")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "rewrite within a block but not across a label boundary" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Cmp(X9, Immediate(0)),
      BCond(Cond.EQ, Label("L1")),
      Label("L1"),
      Cmp(X10, Immediate(0)),
      BCond(Cond.NE, Label("L2")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Cbz(X9, Label("L1")),
      Label("L1"),
      Cbnz(X10, Label("L2")),
      Ret
    )
  }

  // ===========================================================================
  // New tests for current optimiser
  // ===========================================================================

  it should "forward adjacent store-load on FP local slot into mov" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Stur(X9, slot),
      Ldur(X12, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Stur(X9, slot),
      Mov(X12, X9),
      Ret
    )
  }

  it should "drop the load entirely when adjacent store-load uses the same register" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Stur(X9, slot),
      Ldur(X9, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Stur(X9, slot),
      Ret
    )
  }

  it should "remove an adjacent repeated local load into the same register" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldur(X9, slot),
      Ldur(X9, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Ldur(X9, slot),
      Ret
    )
  }

  it should "forward adjacent byte store-load on FP local slot while preserving zero-extension" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -1, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Strb(W9, slot),
      Ldrb(W12, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Strb(W9, slot),
      And(W12, W9, Immediate(255)),
      Ret
    )
  }

  it should "preserve zero-extension for adjacent byte store-load even when using the same register" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -1, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Strb(W9, slot),
      Ldrb(W9, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Strb(W9, slot),
      And(W9, W9, Immediate(255)),
      Ret
    )
  }

  it should "remove an adjacent repeated local byte load into the same register" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -1, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldrb(W9, slot),
      Ldrb(W9, slot),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Ldrb(W9, slot),
      Ret
    )
  }

  it should "NOT forward store-load when load-store kinds differ" in {
    val slot: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs1: List[A64Instr] = List(
      Label("main"),
      Stur(X9, slot),
      Ldr(X12, slot),
      Ret
    )

    val xs2: List[A64Instr] = List(
      Label("main"),
      Str(X9, slot),
      Ldur(X12, slot),
      Ret
    )

    A64PeepholeOptimiser.optimise(xs1) shouldBe xs1
    A64PeepholeOptimiser.optimise(xs2) shouldBe xs2
  }

  it should "form a pair store from adjacent Str instructions to consecutive addresses" in {
    val a0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val a8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Str(X12, a0),
      Str(X13, a8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Stp(X12, X13, a0),
      Ret
    )
  }

  it should "NOT form a pair store when widths differ" in {
    val a0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val a8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Str(W12, a0),
      Str(X13, a8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT form a pair store when stores are not consecutive in memory" in {
    val a0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val a16: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 16, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Str(X12, a0),
      Str(X13, a16),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT form a pair store when base registers differ" in {
    val a0x10: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val a8x11: A64MemAddress = MemAddress.ImmOffsetAddress(X11, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Str(X12, a0x10),
      Str(X13, a8x11),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT form a pair store from adjacent Stur instructions" in {
    val a0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val a8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Stur(X12, a0),
      Stur(X13, a8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "rewrite load-store-load-store into load-load-stp for FP locals to heap object fields" in {
    val s1: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -16, NoIndex)
    val s2: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -24, NoIndex)
    val d0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val d8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      // The optimiser should keep the loads, then merge only the two stores into STP.
      Ldur(X12, s1),
      Str(X12, d0),
      Ldur(X13, s2),
      Str(X13, d8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Ldur(X12, s1),
      Ldur(X13, s2),
      Stp(X12, X13, d0),
      Ret
    )
  }

  it should "NOT apply the 4-instr pair-store rewrite when the destination base is FP" in {
    val s1: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -16, NoIndex)
    val s2: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -24, NoIndex)
    val d0: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -40, NoIndex)
    val d8: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -32, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldur(X12, s1),
      Str(X12, d0),
      Ldur(X13, s2),
      Str(X13, d8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT apply the 4-instr pair-store rewrite when the loaded register does not match the stored register" in {
    val s1: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -16, NoIndex)
    val s2: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -24, NoIndex)
    val d0 :A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val d8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldur(X12, s1),
      Str(X9, d0),   // mismatch
      Ldur(X13, s2),
      Str(X13, d8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT apply the 4-instr pair-store rewrite when the two loads use the same register" in {
    val s1: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -16, NoIndex)
    val s2: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -24, NoIndex)
    val d0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val d8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldur(X12, s1),
      Str(X12, d0),
      Ldur(X12, s2),
      Str(X12, d8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT apply the 4-instr rewrite across a call barrier" in {
    val s1: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -16, NoIndex)
    val s2: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -24, NoIndex)
    val d0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val d8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldur(X12, s1),
      Str(X12, d0),
      Bl(Label("_malloc")),
      Ldur(X13, s2),
      Str(X13, d8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "NOT apply the 4-instr rewrite across a label boundary" in {
    val s1: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -16, NoIndex)
    val s2: A64MemAddress = MemAddress.ImmOffsetAddress(FP, -24, NoIndex)
    val d0: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 0, NoIndex)
    val d8: A64MemAddress = MemAddress.ImmOffsetAddress(X10, 8, NoIndex)

    val xs: List[A64Instr] = List(
      Label("main"),
      Ldur(X12, s1),
      Str(X12, d0),
      Label("Lmid"),
      Ldur(X13, s2),
      Str(X13, d8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe xs
  }

  it should "rewrite mov zero, cmp reg zeroReg, b.eq into mov zero + cbz" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W10, Immediate(0)),
      Cmp(X9, X10),
      BCond(Cond.EQ, Label("L1")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(W10, Immediate(0)),
      Cbz(X9, Label("L1")),
      Ret
    )
  }

  it should "rewrite mov zero, cmp reg zeroReg, b.ne into mov zero + cbnz" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W10, Immediate(0)),
      Cmp(X9, X10),
      BCond(Cond.NE, Label("L1")),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(W10, Immediate(0)),
      Cbnz(X9, Label("L1")),
      Ret
    )
  }

  it should "propagate immediates through a following move" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W8, Immediate(3)),
      Mov(W19, W8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(W19, Immediate(3)),
      Ret
    )
  }

  it should "remove a repeated identical immediate move separated only by a non-clobbering store" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.BaseRegAddress(X10)),
      Mov(W9, Immediate(0)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.BaseRegAddress(X10)),
      Ret
    )
  }

  it should "remove a repeated identical immediate move across multiple non-clobbering stores" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.BaseRegAddress(X10)),
      Str(W9, MemAddress.ImmOffsetAddress(X10, 4, NoIndex)),
      Mov(W9, Immediate(0)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.BaseRegAddress(X10)),
      Str(W9, MemAddress.ImmOffsetAddress(X10, 4, NoIndex)),
      Ret
    )
  }

  it should "remove multiple repeated identical immediate moves across a store run" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.BaseRegAddress(X10)),
      Str(W9, MemAddress.ImmOffsetAddress(X10, 4, NoIndex)),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.ImmOffsetAddress(X10, 8, NoIndex)),
      Mov(W9, Immediate(0)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(W9, Immediate(0)),
      Str(W9, MemAddress.BaseRegAddress(X10)),
      Str(W9, MemAddress.ImmOffsetAddress(X10, 4, NoIndex)),
      Str(W9, MemAddress.ImmOffsetAddress(X10, 8, NoIndex)),
      Ret
    )
  }

  it should "NOT remove a repeated immediate move across a writeback store" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Mov(X9, Immediate(0)),
      Stur(X9, MemAddress.ImmOffsetAddress(SP, -16, PreIndex)),
      Mov(X9, Immediate(0)),
      Ret
    )

    A64PeepholeOptimiser.optimise(xs) shouldBe xs
  }

  it should "rewrite adjacent single-register push-pop into a move" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Stp(X8, XZR, MemAddress.ImmOffsetAddress(SP, -16, PreIndex)),
      Ldp(X0, XZR, MemAddress.ImmOffsetAddress(SP, 16, PostIndex)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Mov(X0, X8),
      Ret
    )
  }

  it should "remove adjacent single-register push-pop when the register is unchanged" in {
    val xs: List[A64Instr] = List(
      Label("main"),
      Stp(X8, XZR, MemAddress.ImmOffsetAddress(SP, -16, PreIndex)),
      Ldp(X8, XZR, MemAddress.ImmOffsetAddress(SP, 16, PostIndex)),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Ret
    )
  }

  it should "rewrite adrp add mov into adrp/add directly on the destination register" in {
    val low = MaskedLabel(12, Label(".L.str0"))
    val xs: List[A64Instr] = List(
      Label("main"),
      Adrp(X8, Label(".L.str0")),
      Add(X8, X8, low),
      Mov(X0, X8),
      Ret
    )

    val got = A64PeepholeOptimiser.optimise(xs)

    got shouldBe List(
      Label("main"),
      Adrp(X0, Label(".L.str0")),
      Add(X0, X0, low),
      Ret
    )
  }

}

