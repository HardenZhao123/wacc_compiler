package wacc.unit_test.backend

import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import wacc.backend.BackendCommon.Immediate
import wacc.backend.AArch64.target.*
import wacc.backend.BackendCommon.ShiftType
import wacc.backend.AArch64.codeGen.A64Formatter
import wacc.backend.AArch64.codeGen.A64Instr.*
import wacc.backend.AArch64.target.Reg.*
import wacc.backend.BackendCommon.Cond
import wacc.backend.BackendCommon.*
import wacc.backend.AArch64.codeGen.A64MemAddress
import wacc.backend.BackendCommon.AsmEntity.*

final class FormatterTest extends AnyFunSuite {

  private def normalizeNewlines(s: String): String =
    s.replace("\r\n", "\n").replace("\r", "\n")

  private def assertEq(actual: String, expected: String): Unit =
    assert(
      normalizeNewlines(actual) == normalizeNewlines(expected),
      s"""|
          |Expected:
          |$expected
          |
          |Actual:
          |$actual
          |""".stripMargin
    )

  private def baosToString(baos: ByteArrayOutputStream): String =
    baos.toString(StandardCharsets.UTF_8)

  test("Formatter.formatInstr: Raw emits line verbatim") {
    assertEq(A64Formatter.formatInstr(Raw(".text")), ".text")
    assertEq(A64Formatter.formatInstr(Raw("\t// comment")), "\t// comment")
  }

  test("Formatter.formatInstr: Label emits '<name>:' with no indent") {
    assertEq(A64Formatter.formatInstr(Label("main")), "main:")
    assertEq(A64Formatter.formatInstr(Label("_L0")), "_L0:")
  }

  test("Formatter.formatInstr: Mov formats with indent and comma+space") {
    assertEq(A64Formatter.formatInstr(Mov(X0, X1)), "\tmov x0, x1")
    assertEq(A64Formatter.formatInstr(Mov(W9, W8)), "\tmov w9, w8")
  }

  test("Formatter.formatInstr: Bl formats as '\\tbl <label>'") {
    assertEq(A64Formatter.formatInstr(Bl(Label("exit"))), "\tbl exit")
    assertEq(A64Formatter.formatInstr(Bl(Label("_errDivZero"))), "\tbl _errDivZero")
  }

  test("Formatter.formatInstr: Ret formats as '\\tret'") {
    assertEq(A64Formatter.formatInstr(Ret), "\tret")
  }

  test("Formatter.formatInstr: Adr formats as '\\tadr <dst>, <label>'") {
    assertEq(A64Formatter.formatInstr(Adr(X0, Label("msg"))), "\tadr x0, msg")
  }

  test("Formatter.formatInstr: Adrp formats as '\\tadrp <dst>, <label>'") {
    assertEq(A64Formatter.formatInstr(Adrp(X0, Label("msg"))), "\tadrp x0, msg")
  }

  test("Formatter.formatInstr: Add formats as '\\tadd <dst>, <op1>, <op2>' (immediate gets '#')") {
    assertEq(A64Formatter.formatInstr(Add(X0, X1, Immediate(7))), "\tadd x0, x1, #7")
    assertEq(A64Formatter.formatInstr(Add(W8, W8, Immediate(0))), "\tadd w8, w8, #0")
  }

  test("Formatter.formatInstr: BCond covers all Cond enum values") {
    for (cond <- Cond.values) {
      val expected = s"\tb.${cond.toCondStr} L"
      assertEq(
        A64Formatter.formatInstr(BCond(cond, Label("L"))),
        expected
      )
    }
  }

  test("Formatter.formatInstr: Cmp (isMull=false) formats as 'cmp <lhs>, <rhs>'") {
    assertEq(A64Formatter.formatInstr(Cmp(W9, Immediate(0), isMull = false)), "\tcmp w9, #0")
    assertEq(A64Formatter.formatInstr(Cmp(X0, X1, isMull = false)), "\tcmp x0, x1")
  }

  test("Formatter.formatInstr: Cmp (isMull=true) appends 'sxtw' as third operand") {
    assertEq(A64Formatter.formatInstr(Cmp(X0, W1, isMull = true)), "\tcmp x0, w1, sxtw")
  }

  test("Formatter.formatInstr: StringData emits .word <len>, then '<label>:' line, then .asciz line") {
    val instr = StringData(Label("msg_0"), "hello", printAlign = false)

    val expected =
      """	.word 5
        |msg_0:
        |	.asciz "hello"""".stripMargin

    assertEq(A64Formatter.formatInstr(instr), expected)
  }

  test("Formatter.formatInstr: StringData with printAlign=true appends '\\n.align 4'") {
    val instr = StringData(Label("msg_1"), "abc", printAlign = true)

    val expected = "\t.word 3\nmsg_1:\n\t.asciz \"abc\"\n.align 4"
    assertEq(A64Formatter.formatInstr(instr), expected)
  }

  test("Formatter.formatInstr: StringData length uses value.length (not including terminator)") {
    val s = "abcd"
    val out = A64Formatter.formatInstr(StringData(Label("msg_len"), s, printAlign = false))
    val firstLine = out.linesIterator.next().trim
    assert(firstLine == ".word 4", s"expected first line '.word 4', got:\n$out")
  }

  test("Formatter.formatInstr: DataSeg prints '.data' then each formatted item, newline-separated") {
    val seg = DataSeg(
      List(
        StringData(Label("s0"), "x", printAlign = false),
        StringData(Label("s1"), "yy", printAlign = false)
      )
    )

    val expected =
      ".data\n" +
        "\t.word 1\n" +
        "s0:\n" +
        "\t.asciz \"x\"\n" +
        "\t.word 2\n" +
        "s1:\n" +
        "\t.asciz \"yy\""

    assertEq(A64Formatter.formatInstr(seg), expected)
  }

  test("Formatter.writeAssembly: writes each formatted instruction followed by a single newline") {
    val baos = new ByteArrayOutputStream()
    val instrs = List(
      Raw(".text"),
      Label("main"),
      Mov(X0, Immediate(7)),
      Ret
    )

    A64Formatter.writeAssembly(instrs, baos)

    val expected = ".text\nmain:\n\tmov x0, #7\n\tret\n"
    assertEq(baosToString(baos), expected)
  }

  test("Formatter.formatOperand: Immediate delegates to formatImmediate") {
    assertEq(A64Formatter.formatOperand(Immediate(5)), "#5")
    assertEq(A64Formatter.formatOperand(Immediate(4095)), "#4095")
    assertEq(A64Formatter.formatOperand(Immediate(4096)), "#0x1000")
  }

  test("Formatter.formatOperand: A64Reg uses reg.toString") {
    assertEq(A64Formatter.formatOperand(X0), "x0")
    assertEq(A64Formatter.formatOperand(W9), "w9")
    assertEq(A64Formatter.formatOperand(FP), "fp")
    assertEq(A64Formatter.formatOperand(LR), "lr")
    assertEq(A64Formatter.formatOperand(SP), "sp")
  }

  test("Formatter.formatOperand: Label formats as raw label text (no colon)") {
    assertEq(A64Formatter.formatOperand(Label("L0")), "L0")
  }

  test("Formatter.formatOperand: String operand is emitted verbatim") {
    assertEq(A64Formatter.formatOperand("sxtw"), "sxtw")
    assertEq(A64Formatter.formatOperand("lsl #2"), "lsl #2")
  }

  test("Formatter.formatOperand: MaskedLabel formats as ':lo<count>:<label>'") {
    assertEq(A64Formatter.formatOperand(MaskedLabel(12, Label("msg"))), ":lo12:msg")
  }

  test("Formatter.formatImmediate: <=4095 prints decimal '#<value>'") {
    assertEq(A64Formatter.formatImmediate(0), "#0")
    assertEq(A64Formatter.formatImmediate(1), "#1")
    assertEq(A64Formatter.formatImmediate(4095), "#4095")
  }

  test("Formatter.formatImmediate: >4095 prints hex '#0x<lowercase>'") {
    assertEq(A64Formatter.formatImmediate(4096), "#0x1000")
    assertEq(A64Formatter.formatImmediate(65535), "#0xffff")
  }

  test("Formatter.formatImmediate: negative values currently print '#-N' (document current behaviour)") {
    assertEq(A64Formatter.formatImmediate(-1), "#-1")
    assertEq(A64Formatter.formatImmediate(-8), "#-8")
  }

  test("Formatter.formatInstr: Cset formatting") {
    assertEq(A64Formatter.formatInstr(Cset(W0, Cond.EQ)), "\tcset w0, eq")
    assertEq(A64Formatter.formatInstr(Cset(W9, Cond.NE)), "\tcset w9, ne")
  }

  test("Formatter.formatInstr: B (unconditional branch) formatting") {
    assertEq(A64Formatter.formatInstr(B(Label("L0"))), "\tb L0")
    assertEq(A64Formatter.formatInstr(B(Label("_errDivZero"))), "\tb _errDivZero")
  }

  test("Formatter.formatInstr: MemAddress ShiftedRegister") {
    val st = ShiftType.LSL(Immediate(2))
    val addr: A64MemAddress = MemAddress.ShiftedRegister(X1, X2, st, 2)
    val expected = s"\tldr x0, [x1, x2, ${st.toShiftStr}, 2]"
    assertEq(A64Formatter.formatInstr(Ldr(X0, addr)), expected)
  }

  test("Formatter.formatInstr: Ldr formatting") {
    assertEq(
      A64Formatter.formatInstr(Ldr(X0, MemAddress.BaseRegAddress(X1))),
      "\tldr x0, [x1]"
    )

    assertEq(
      A64Formatter.formatInstr(
        Ldr(
          W9,
          MemAddress.ImmOffsetAddress(
            FP,
            -16,
            PreIndex
          )
        )
      ),
      "\tldr w9, [fp, #-16]!"
    )

    assertEq(
      A64Formatter.formatInstr(
        Ldr(
          W0,
          MemAddress.ImmOffsetAddress(
            SP,
            8,
            PostIndex
          )
        )
      ),
      "\tldr w0, [sp], #8"
    )
  }

  test("Formatter.formatInstr: Str formatting") {
    assertEq(
      A64Formatter.formatInstr(Str(X0, MemAddress.BaseRegAddress(SP))),
      "\tstr x0, [sp]"
    )

    assertEq(
      A64Formatter.formatInstr(
        Str(
          W1,
          MemAddress.ImmOffsetAddress(
            FP,
            0,
            NoIndex
          )
        )
      ),
      "\tstr w1, [fp, #0]"
    )

    assertEq(
      A64Formatter.formatInstr(
        Str(
          X2,
          MemAddress.ImmOffsetAddress(
            SP,
            16,
            PostIndex
          )
        )
      ),
      "\tstr x2, [sp], #16"
    )
  }

  test("Formatter.formatInstr: Stp formatting") {
    assertEq(
      A64Formatter.formatInstr(
        Stp(
          FP,
          LR,
          MemAddress.ImmOffsetAddress(
            SP,
            -16,
            PreIndex
          )
        )
      ),
      "\tstp fp, lr, [sp, #-16]!"
    )

    assertEq(
      A64Formatter.formatInstr(
        Stp(
          X0,
          X1,
          MemAddress.ImmOffsetAddress(
            SP,
            16,
            PostIndex
          )
        )
      ),
      "\tstp x0, x1, [sp], #16"
    )
  }

  test("Formatter.formatInstr: Ldp formatting") {
    assertEq(
      A64Formatter.formatInstr(
        Ldp(
          FP,
          LR,
          MemAddress.ImmOffsetAddress(
            SP,
            16,
            PostIndex
          )
        )
      ),
      "\tldp fp, lr, [sp], #16"
    )

    assertEq(
      A64Formatter.formatInstr(
        Ldp(
          X2,
          X3,
          MemAddress.ImmOffsetAddress(
            SP,
            0,
            NoIndex
          )
        )
      ),
      "\tldp x2, x3, [sp, #0]"
    )
  }

  test("Formatter.formatInstr: Sub formatting") {
    assertEq(A64Formatter.formatInstr(Sub(X0, X1, Immediate(7))), "\tsub x0, x1, #7")
    assertEq(A64Formatter.formatInstr(Sub(W8, W8, W9)), "\tsub w8, w8, w9")
  }

  test("Formatter.formatInstr: Subs formatting") {
    assertEq(A64Formatter.formatInstr(Subs(W9, W10, Immediate(0))), "\tsubs w9, w10, #0")
    assertEq(A64Formatter.formatInstr(Subs(X0, X1, X2)), "\tsubs x0, x1, x2")
  }

  test("Formatter.formatInstr: Neg formatting") {
    assertEq(A64Formatter.formatInstr(Neg(W0, W1)), "\tneg w0, w1")
    assertEq(A64Formatter.formatInstr(Neg(X2, Immediate(0))), "\tneg x2, #0")
  }

  test("Formatter.formatInstr: Negs formatting") {
    assertEq(A64Formatter.formatInstr(Negs(X0, X1)), "\tnegs x0, x1")
    assertEq(A64Formatter.formatInstr(Negs(W2, Immediate(1))), "\tnegs w2, #1")
  }

  test("Formatter.formatInstr: And formatting") {
    assertEq(A64Formatter.formatInstr(And(W0, W1, Immediate(255))), "\tand w0, w1, #255")
    assertEq(A64Formatter.formatInstr(And(X2, X3, X4)), "\tand x2, x3, x4")
  }

  test("Formatter.formatInstr: Orr formatting") {
    assertEq(A64Formatter.formatInstr(Orr(X0, X1, X2)), "\torr x0, x1, x2")
    assertEq(A64Formatter.formatInstr(Orr(W3, W4, Immediate(0))), "\torr w3, w4, #0")
  }

  test("Formatter.formatInstr: Adds formatting") {
    assertEq(A64Formatter.formatInstr(Adds(W0, W1, Immediate(1))), "\tadds w0, w1, #1")
    assertEq(A64Formatter.formatInstr(Adds(X2, X3, X4)), "\tadds x2, x3, x4")
  }

  test("Formatter.formatInstr: SMull formatting") {
    assertEq(A64Formatter.formatInstr(SMull(X0, W1, W2)), "\tsmull x0, w1, w2")
  }

  test("Formatter.formatInstr: MSub formatting") {
    assertEq(A64Formatter.formatInstr(MSub(X0, X1, X2, X3)), "\tmsub x0, x1, x2, x3")
  }

  test("Formatter.formatInstr': SDiv formatting") {
    assertEq(A64Formatter.formatInstr(SDiv(W0, W1, W2)), "\tsdiv w0, w1, w2")
    assertEq(A64Formatter.formatInstr(SDiv(X3, X4, X5)), "\tsdiv x3, x4, x5")
  }
}
