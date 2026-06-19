package wacc.unit_test.backend

import org.scalatest.Assertions.*
import wacc.backend.midir.TAC.*
import wacc.backend.AArch64.target.A64RegisterAllocator
import wacc.backend.AArch64.target.A64CallingConvention

/** Test helpers for constructing TAC values and common assertions. */
trait StackFrameTestSupport {

  /** Construct a temp value with an id and bit-length. */
  def t(id: Int, len: BitLength = BitLength._64): Temp =
    Temp(id, len)

  /** Construct an integer immediate RHS. */
  def immI(v: Long): Rhs = ImmValue(v)

  /** Construct a string literal RHS by string-table id. */
  def str(id: Int): Rhs = TACStr(id)

  /** Assert the given size is a multiple of 16. */
  def assert16Aligned(n: Int): Unit =
    assert(n % 16 == 0, s"expected 16-byte alignment, got $n")

  /** Create a register allocator seeded with the scratch register set. */
  def freshRA(): A64RegisterAllocator =
    new A64RegisterAllocator(A64CallingConvention.scratchRegisters)
}