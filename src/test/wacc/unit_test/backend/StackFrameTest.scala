package wacc.unit_test.backend

import org.scalatest.funsuite.AnyFunSuite
import wacc.backend.AArch64.target.Reg
import wacc.backend.AArch64.target.A64CallingConvention
import wacc.backend.AArch64.target.StackFrame
import wacc.backend.AArch64.codeGen.A64Instr
import wacc.backend.AArch64.codeGen.A64Instr.*

final class StackFrameTest extends AnyFunSuite with StackFrameTestSupport {

  test("A64 calling convention: arg registers are x0..x7") {
    // Compare argRegisters list with the expected x0..x7 sequence.
    assert(A64CallingConvention.argRegisters == List(
      Reg.X0, Reg.X1, Reg.X2, Reg.X3, Reg.X4, Reg.X5, Reg.X6, Reg.X7
    ))
  }

  test("A64 calling convention: callee-saved registers are x19..x28") {
    // Compare calleeSavedRegisters list with the expected x19..x28 sequence.
    assert(A64CallingConvention.calleeSavedRegisters == List(
      Reg.X19, Reg.X20, Reg.X21, Reg.X22, Reg.X23,
      Reg.X24, Reg.X25, Reg.X26, Reg.X27, Reg.X28
    ))
  }

  test("A64 calling convention: fp/lr/sp are defined") {
    // Check the string forms of fp/lr/sp symbolic registers.
    assert(A64CallingConvention.framePointer.toString == "fp")
    assert(A64CallingConvention.linkRegister.toString == "lr")
    assert(A64CallingConvention.stackPointer.toString == "sp")
  }

  test("StackFrame: can construct with empty slots") {
    // Construct a StackFrame with no locals.
    val sf = new StackFrame(Seq.empty)

    // Read localsAreaSize and check it is 0 or aligned.
    assert(sf.localsAreaSize == 0 || sf.localsAreaSize % 16 == 0)
  }

  test("StackFrame: addFrame returns prologue saving fp/lr and setting fp=sp") {
    // Construct a frame and generate prologue instructions.
    val sf = new StackFrame(Seq.empty)
    val pro = sf.addFrame()

    // Find an Stp instruction that stores fp and lr.
    assert(pro.exists {
      case A64Instr.Stp(a, b, _) => a.toString == "fp" && b.toString == "lr"
      case _ => false
    })

    // Find a Mov instruction that sets fp from sp.
    assert(pro.exists {
      case Mov(dst, src) => dst.toString == "fp" && src.toString == "sp"
      case _ => false
    })
  }

  test("StackFrame: removeFrame returns epilogue ending with Ret") {
    // Construct a frame and generate epilogue instructions.
    val sf = new StackFrame(Seq.empty)
    val epi = sf.removeFrame()

    // Check the epilogue list is non-empty and ends with Ret.
    assert(epi.nonEmpty)
    assert(epi.last == Ret)
  }

  test("StackFrame: locals slots are negative offsets from FP; localsAreaSize is 16B aligned") {
    // Construct a frame with three local temps.
    val slots = Seq(t(1), t(2), t(3))
    val sf = new StackFrame(slots)

    // Compute fp-relative offsets for each temp.
    val offs = slots.map(sf.offsetOf)

    // Check each offset is negative.
    assert(offs.forall(_ < 0), s"expected negative FP-relative offsets, got $offs")

    // Check offsets are pairwise distinct.
    assert(offs.distinct.size == offs.size, s"expected distinct slots, got $offs")

    // Check the locals area size is 16-byte aligned.
    assert16Aligned(sf.localsAreaSize)
  }
}