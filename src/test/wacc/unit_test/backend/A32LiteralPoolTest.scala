package wacc.unit_test.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.backend.Arm32.codeGen.{A32Generator, A32Instr}
import wacc.backend.BackendCommon.AsmEntity.*
import wacc.backend.midir.TAC.*

class A32LiteralPoolTest extends AnyFlatSpec {

  "ARM32 code generator" should "emit intermediate literal pools for large programs" in {
    val body = List.fill(160)(PrintStr(TACStr(0)))
    val program = TACProgram(
      strs = List("hello"),
      funcs = Nil,
      body = body,
      locals = Nil
    )

    val asm = A32Generator.genA32Program(program)

    asm.count {
      case Raw(".ltorg") => true
      case _              => false
    } should be > 1
  }

  it should "convert an integer to float with the AEABI helper" in {
    val converted = Temp(0, BitLength._32)
    val program = TACProgram(
      strs = Nil,
      funcs = Nil,
      body = List(IntToFloat(converted, ImmValue(2))),
      locals = List(converted)
    )

    val asm = A32Generator.genA32Program(program)
    asm.exists {
      case A32Instr.Bl(Label("__aeabi_i2f")) => true
      case _ => false
    } shouldBe true
  }
}
