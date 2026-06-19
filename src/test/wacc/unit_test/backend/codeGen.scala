package wacc.unit_test.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.backend.AArch64.codeGen.*
import wacc.backend.label.Label as TacLabel
import wacc.backend.midir.TAC.*
import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.BackendCommon.Immediate
import wacc.backend.BackendCommon.Cond
import wacc.backend.AArch64.preDefFunctions.PreDefRunTimeError
import wacc.backend.BackendCommon.AsmEntity.*

class CodeGenTest extends AnyFlatSpec {

  "code generator" should "generate AArch64 instructions for a simple exit statement" in {
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACExit(ImmValue(0))),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Mov(_, Immediate(0)) => true; case _ => false } shouldBe true
    asm.exists { case Bl(Label("exit")) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for a simple binary operation: 1 + 1" in {
    val t0 = Temp(0, BitLength._32)
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(BinOp(t0, ArithOp.Add, ImmValue(1), ImmValue(1))),
      locals = List(t0)
    )
    val asm = A64Generator.genA64Program(p)

    val movOnes = asm.collect {
      case Mov(_, Immediate(1)) => 1
    }
    movOnes.size shouldBe 2
    asm.exists { case Adds(_, _, _) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for a print statement: print 1" in {
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(PrintInt(ImmValue(1))),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Bl(Label("_printi")) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for a read statement: read x" in {
    val x = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACRead(x, ReadType.Int)),
      locals = List(x)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Bl(Label("_readi")) => true; case _ => false } shouldBe true
    asm.exists { case Stur(_, _) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for a free statement: free x" in {
    val x = Temp(0, BitLength._ptr)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACFree(x, isPair = true)),
      locals = List(x)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Bl(Label("_freepair")) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for declaration: int x = 1" in {
    val x = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACAssign(x, ImmValue(1))),
      locals = List(x)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Stur(_, _) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for assignment: declared = true" in {
    val x = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACAssign(x, ImmValue(1, BitLength._8))),
      locals = List(x)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Mov(_, Immediate(1)) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for print bool" in {
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(PrintBool(ImmValue(1, BitLength._8))),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Bl(Label("_printb")) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for exit non-zero" in {
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACExit(ImmValue(7))),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Mov(_, Immediate(7)) => true; case _ => false } shouldBe true
    asm.exists { case Bl(Label("exit")) => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for return immediate" in {
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(TACReturn(ImmValue(3))),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Mov(_, Immediate(3)) => true; case _ => false } shouldBe true
    asm.exists { case Ret => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for return temp" in {
    val t0 = Temp(0, BitLength._32)


    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(5)),
        TACReturn(t0)
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Stur(_, _) => true; case _ => false } shouldBe true
    asm.exists { case Ret => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for print string literal" in {
    val p = TACProgram(
      strs = List("hello", "world"),
      funcs = Nil,
      body = List(TACExit(ImmValue(0))),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists {
      case DataSeg(items) => items.nonEmpty
      case _              => false
    } shouldBe true

    asm.exists {
      case DataSeg(items) =>
        items.exists {
          case StringData(Label(".str_0"), "hello", _) => true
          case _                                       => false
        }
      case _ => false
    } shouldBe true

    asm.exists {
      case DataSeg(items) =>
        items.exists {
          case StringData(Label(".str_1"), "world", _) => true
          case _                                       => false
        }
      case _ => false
    } shouldBe true
  }

  it should "generate AArch64 instructions for a simple unary operation: -9" in {
    val t0 = Temp(0, BitLength._32)
    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        UnOp(t0, UnaryOp.Neg, ImmValue(9)),
        TACReturn(t0)
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Ret => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for if-else statement" in {
    val t0 = Temp(0, BitLength._32)

    val thenL = TacLabel("then_branch")
    val elseL = TacLabel("else_branch")
    val endL  = TacLabel("end_if")

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(0)),
        CmpJmp(CondOp.EQ, t0, ImmValue(0), thenL),
        Jmp(elseL),

        Mark(thenL),
        TACExit(ImmValue(0)),
        Jmp(endL),

        Mark(elseL),
        TACExit(ImmValue(1)),

        Mark(endL)
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case BCond(_, Label("then_branch")) => true; case _ => false } shouldBe true
    asm.exists { case B(Label("else_branch"))        => true; case _ => false } shouldBe true
    asm.exists { case Label("then_branch") => true; case _ => false } shouldBe true
    asm.exists { case Label("else_branch") => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for while loop" in {
    val i = Temp(0, BitLength._32)

    val loopL = TacLabel("loop")
    val endL  = TacLabel("end_loop")

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(i, ImmValue(0)),

        Mark(loopL),
        CmpJmp(CondOp.GEQ, i, ImmValue(3), endL),

        BinOp(i, ArithOp.Add, i, ImmValue(1)),
        Jmp(loopL),

        Mark(endL),
        TACExit(ImmValue(0))
      ),
      locals = List(i)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Label("loop") => true; case _ => false } shouldBe true
    asm.exists { case Label("end_loop") => true; case _ => false } shouldBe true
    asm.exists { case BCond(_, Label("end_loop")) => true; case _ => false } shouldBe true
    asm.exists { case B(Label("loop"))           => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for conditional jump (CmpJmp)" in {
    val t0 = Temp(0, BitLength._32)
    val l1 = TacLabel("L1")

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(5)),
        CmpJmp(CondOp.EQ, t0, ImmValue(5), l1),
        TACExit(ImmValue(1)),
        Mark(l1),
        TACExit(ImmValue(0))
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Cmp(_, _, _) => true; case _ => false } shouldBe true
    asm.exists { case BCond(_, Label("L1")) => true; case _ => false } shouldBe true
    asm.exists { case Label("L1") => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for unconditional jump" in {
    val l1 = TacLabel("target")

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        Jmp(l1),
        TACExit(ImmValue(1)),
        Mark(l1),
        TACExit(ImmValue(0))
      ),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case B(Label("target")) => true; case _ => false } shouldBe true
    asm.exists { case Label("target") => true; case _ => false } shouldBe true
  }

  it should "generate AArch64 instructions for label mark" in {
    val l = TacLabel("mark_here")

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        Mark(l),
        TACExit(ImmValue(0))
      ),
      locals = Nil
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case Label("mark_here") => true; case _ => false } shouldBe true
  }

  it should "special-case multiply by 2 using adds instead of smull" in {
    val t0 = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(7)),
        BinOp(t0, ArithOp.Mul, t0, ImmValue(2))
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.collect { case Adds(_, _, _) => 1 }.size shouldBe 1
    asm.exists { case BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)) => true; case _ => false } shouldBe true
    asm.exists { case SMull(_, _, _) => true; case _ => false } shouldBe false
  }

  it should "special-case multiply by 3 using two adds instead of smull" in {
    val t0 = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(7)),
        BinOp(t0, ArithOp.Mul, t0, ImmValue(3))
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.collect { case Adds(_, _, _) => 1 }.size shouldBe 2
    asm.collect { case BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)) => 1; case _ => 0 }.sum shouldBe 2
    asm.exists { case SMull(_, _, _) => true; case _ => false } shouldBe false
  }

  it should "special-case multiply by 4 using repeated adds instead of smull" in {
    val t0 = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(7)),
        BinOp(t0, ArithOp.Mul, t0, ImmValue(4))
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.collect { case Adds(_, _, _) => 1 }.size shouldBe 2
    asm.collect { case BCond(Cond.VS, Label(PreDefRunTimeError.ErrOverflowLabel)) => 1; case _ => 0 }.sum shouldBe 2
    asm.exists { case SMull(_, _, _) => true; case _ => false } shouldBe false
  }

  it should "special-case multiply by constant when constant is on the left" in {
    val t0 = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(7)),
        BinOp(t0, ArithOp.Mul, ImmValue(3), t0)
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.collect { case Adds(_, _, _) => 1 }.size shouldBe 2
    asm.exists { case SMull(_, _, _) => true; case _ => false } shouldBe false
  }

  it should "fall back to generic smull for unsupported constant multiplication" in {
    val t0 = Temp(0, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(7)),
        BinOp(t0, ArithOp.Mul, t0, ImmValue(6))
      ),
      locals = List(t0)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case SMull(_, _, _) => true; case _ => false } shouldBe true
  }

  it should "fall back to generic smull when neither operand is a constant" in {
    val t0 = Temp(0, BitLength._32)
    val t1 = Temp(1, BitLength._32)
    val t2 = Temp(2, BitLength._32)

    val p = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(
        TACAssign(t0, ImmValue(2)),
        TACAssign(t1, ImmValue(3)),
        BinOp(t2, ArithOp.Mul, t0, t1)
      ),
      locals = List(t0, t1, t2)
    )

    val asm = A64Generator.genA64Program(p)

    asm.exists { case SMull(_, _, _) => true; case _ => false } shouldBe true
  }
}
