package wacc.unit_test.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.backend.BackendCommon.{Immediate, NoIndex, PreIndex, PostIndex}
import wacc.backend.Arm32.codeGen.A32Instr
import wacc.backend.Arm32.codeGen.A32Instr.*
import wacc.backend.Arm32.codeGen.A32PeepholeOptimiser
import wacc.backend.Arm32.codeGen.A32MemAddress
import wacc.backend.Arm32.target.{FP, SP, R}
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.BackendCommon.*

final class A32PeepholeOptimiserTest extends AnyFlatSpec with Matchers {

  it should "remove consecutive self-moves inside a block" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(0), R(0)),
      Mov(R(1), Immediate(7)),
      Mov(R(2), R(2)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(1), Immediate(7)),
      B(Label("end"))
    ))
  }

  it should "NOT peek across Label" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(1), Immediate(1)),
      Label("L1"),
      Mov(R(0), R(0)),
      Mov(R(2), Immediate(2)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(1), Immediate(1)),
      Label("L1"),
      Mov(R(2), Immediate(2)),
      B(Label("end"))
    ))
  }

  it should "remove unreachable instructions after unconditional B inside a block" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      B(Label("L1")),
      Mov(R(0), R(0)),
      Add(R(1), R(1), Immediate(0)),
      Label("L1"),
      Mov(R(2), Immediate(3))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      B(Label("L1")),
      Label("L1"),
      Mov(R(2), Immediate(3))
    ))
  }

  it should "remove truly unreachable normal instructions after unconditional B before the next label" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      B(Label("L1")),
      Mov(R(0), R(1)),
      Mov(R(2), R(3)),
      Label("L1"),
      Mov(R(4), Immediate(9))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      B(Label("L1")),
      Label("L1"),
      Mov(R(4), Immediate(9))
    ))
  }

  it should "remove dead word stores on the same FP slot" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Str(R(4), slot),
      Str(R(5), slot),
      Ldr(R(6), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Str(R(5), slot),
      Mov(R(6), R(5)),
      B(Label("end"))
    ))
  }

  it should "NOT remove a store if a load observes it before overwrite" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Str(R(4), slot),
      Ldr(R(5), slot),
      Str(R(6), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Str(R(4), slot),
      Mov(R(5), R(4)),
      Str(R(6), slot),
      B(Label("end"))
    ))
  }

  it should "forward adjacent store-load on FP local word slot into mov" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Str(R(4), slot),
      Ldr(R(5), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Str(R(4), slot),
      Mov(R(5), R(4)),
      B(Label("end"))
    ))
  }

  it should "drop the load entirely when adjacent store-load uses the same register" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Str(R(4), slot),
      Ldr(R(4), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Str(R(4), slot),
      B(Label("end"))
    ))
  }

  it should "remove an adjacent repeated word load from the same FP local slot into the same register" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Ldr(R(4), slot),
      Ldr(R(4), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Ldr(R(4), slot),
      B(Label("end"))
    ))
  }

  it should "forward adjacent byte store-load on FP local byte slot while preserving zero-extension" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -1, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Strb(R(4), slot),
      Ldrb(R(5), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Strb(R(4), slot),
      And(R(5), R(4), Immediate(255)),
      B(Label("end"))
    ))
  }

  it should "preserve zero-extension for byte store-load even when using the same register" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -1, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Strb(R(4), slot),
      Ldrb(R(4), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Strb(R(4), slot),
      And(R(4), R(4), Immediate(255)),
      B(Label("end"))
    ))
  }

  it should "remove an adjacent repeated byte load from the same FP local slot into the same register" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -1, NoIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Ldrb(R(4), slot),
      Ldrb(R(4), slot),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Ldrb(R(4), slot),
      B(Label("end"))
    ))
  }

  it should "NOT forward store-load when widths differ" in {
    val slot: A32MemAddress = MemAddress.ImmOffsetAddress(FP, -8, NoIndex)

    val xs1: List[A32Instr] = List(
      Label("main"),
      Str(R(4), slot),
      Ldrb(R(5), slot),
      B(Label("end"))
    )

    val xs2: List[A32Instr] = List(
      Label("main"),
      Strb(R(4), slot),
      Ldr(R(5), slot),
      B(Label("end"))
    )

    A32PeepholeOptimiser.optimise(xs1).shouldBe(xs1)
    A32PeepholeOptimiser.optimise(xs2).shouldBe(xs2)
  }

  it should "NOT optimise PreIndex/PostIndex addressing as safe local slots" in {
    val pre: A32MemAddress = MemAddress.ImmOffsetAddress(SP, -4, PreIndex)
    val post: A32MemAddress = MemAddress.ImmOffsetAddress(SP, 4, PostIndex)

    val xs: List[A32Instr] = List(
      Label("main"),
      Str(R(4), pre),
      Str(R(5), pre),
      Str(R(6), post),
      Str(R(7), post),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(xs)
  }

  it should "rewrite add by zero into mov" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Add(R(4), R(5), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), R(5)),
      B(Label("end"))
    ))
  }

  it should "rewrite sub by zero into mov" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Sub(R(4), R(5), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), R(5)),
      B(Label("end"))
    ))
  }

  it should "remove add or sub by zero when destination equals source" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Add(R(4), R(4), Immediate(0)),
      Sub(R(5), R(5), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      B(Label("end"))
    ))
  }

  it should "rewrite lsl lsr asr by zero into mov" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Lsl(R(4), R(5), Immediate(0)),
      Lsr(R(6), R(7), Immediate(0)),
      Asr(R(8), R(9), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), R(5)),
      Mov(R(6), R(7)),
      Mov(R(8), R(9)),
      B(Label("end"))
    ))
  }

  it should "remove zero-shift when destination equals source" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Lsl(R(4), R(4), Immediate(0)),
      Lsr(R(5), R(5), Immediate(0)),
      Asr(R(6), R(6), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      B(Label("end"))
    ))
  }

  it should "shorten move chains" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(4), R(3)),
      Mov(R(5), R(4)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), R(3)),
      Mov(R(5), R(3)),
      B(Label("end"))
    ))
  }

  it should "remove overwritten moves to the same destination" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(4), R(1)),
      Mov(R(4), R(2)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), R(2)),
      B(Label("end"))
    ))
  }

  it should "NOT propagate across labels or branch barriers because blocks are split" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(4), R(3)),
      Label("L1"),
      Mov(R(5), R(4)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(xs)
  }

  it should "propagate immediates through a following move" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(8), Immediate(3)),
      Mov(R(4), R(8)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), Immediate(3)),
      B(Label("end"))
    ))
  }

  it should "propagate literal loads through a following move" in {
    val addr: A32MemAddress = MemAddress.LabelAddress(Label("=.str_2"))
    val xs: List[A32Instr] = List(
      Label("main"),
      LdrFuncPtr(R(9), addr),
      Mov(R(0), R(9)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      LdrFuncPtr(R(0), addr),
      B(Label("end"))
    ))
  }

  it should "remove identical repeated immediate moves" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      Mov(R(7), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      B(Label("end"))
    ))
  }

  it should "rewrite mov zero plus cmp against that temporary into cmp immediate zero" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(4), Immediate(0)),
      Cmp(R(7), R(4)),
      BCond(Cond.EQ, Label("L1")),
      Label("L1"),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(4), Immediate(0)),
      Cmp(R(7), Immediate(0)),
      BCond(Cond.EQ, Label("L1")),
      Label("L1"),
      B(Label("end"))
    ))
  }

  it should "remove a repeated identical immediate move separated only by a non-clobbering store" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      Str(R(7), MemAddress.BaseRegAddress(R(8))),
      Mov(R(7), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      Str(R(7), MemAddress.BaseRegAddress(R(8))),
      B(Label("end"))
    ))
  }

  it should "remove a repeated identical immediate move across multiple non-clobbering stores" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      // These stores should not block deletion of the repeated immediate move.
      Str(R(7), MemAddress.BaseRegAddress(R(8))),
      Str(R(7), MemAddress.ImmOffsetAddress(R(8), 4, NoIndex)),
      Mov(R(7), Immediate(0)),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      Str(R(7), MemAddress.BaseRegAddress(R(8))),
      Str(R(7), MemAddress.ImmOffsetAddress(R(8), 4, NoIndex)),
      B(Label("end"))
    ))
  }

  it should "NOT remove a repeated immediate move across a writeback store" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Mov(R(7), Immediate(0)),
      Str(R(7), MemAddress.ImmOffsetAddress(SP, -4, PreIndex)),
      Mov(R(7), Immediate(0)),
      B(Label("end"))
    )

    A32PeepholeOptimiser.optimise(xs).shouldBe(xs)
  }

  it should "remove adjacent push-pop roundtrips on the same register" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Push(List(R(8))),
      Pop(List(R(8))),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      B(Label("end"))
    ))
  }

  it should "rewrite adjacent push-pop on different registers into a move" in {
    val xs: List[A32Instr] = List(
      Label("main"),
      Push(List(R(8))),
      Pop(List(R(0))),
      B(Label("end"))
    )

    val got = A32PeepholeOptimiser.optimise(xs)

    got.shouldBe(List(
      Label("main"),
      Mov(R(0), R(8)),
      B(Label("end"))
    ))
  }

}
