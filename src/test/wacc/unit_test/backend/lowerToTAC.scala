package wacc.unit_test.backend

import org.scalatest.funsuite.AnyFunSuite
import wacc.backend.midir.{LowerToTAC, TAC}
import wacc.frontend.typedAST.*
import wacc.frontend.typedAST.TypedStmt.*
import wacc.frontend.typedAST.TypedExpr.*
import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.label.*
import wacc.backend.midir.TAC.BitLength._ptr
import wacc.backend.midir.TAC.{BitLength, ImmValue}
import wacc.frontend.typedAST.SemanticType.{SemArray, SemInt, SemString}
import wacc.frontend.typedAST.TypedException.*
import wacc.frontend.ast.*
import wacc.common.PositionInfo

import scala.collection.mutable

final class LowerToTACTest extends AnyFunSuite {
  private def lower(stmts: List[TypedStmt]): TAC.TACProgram = {
    val p = TypedProgram(funcs = Nil, stmts = stmts)
    val lowered = LowerToTAC.fromTypedProgram(p, A64_WORD_SIZE)
    lowered.copy(body = lowered.body.dropWhile {
      case TAC.TACClearException() => true
      case _ => false
    })
  }

  private def normalizeProgram(p: TAC.TACProgram): TAC.TACProgram = {
    val body1 = normalizeLabels(p.body.toList)
    val (body2, tempMap) = normalizeTemps(body1)

    val locals2 = p.locals.toList.map {
      case t: TAC.Temp => tempMap.getOrElse(t, t)
    }

    p.copy(body = body2, locals = locals2)
  }

  // Canonicalize labels by first-appearance order: L0, L1, ...
  private def normalizeLabels(body: List[TAC.Instr]): List[TAC.Instr] = {
    val mapping = mutable.LinkedHashMap.empty[Label, Label]
    var next = 0

    def canon(l: Label): Label =
      mapping.getOrElseUpdate(l, {
        val out = Label(s"L$next"); next += 1; out
      })

    body.map {
      case TAC.Mark(l) => TAC.Mark(canon(l))
      case TAC.Jmp(to) => TAC.Jmp(canon(to))
      case TAC.CmpJmp(cond, lhs, rhs, to) => TAC.CmpJmp(cond, lhs, rhs, canon(to))
      case other => other
    }
  }

  // Canonicalize temps by first-appearance order: Temp(0, len), Temp(1, len), ...
  // Returns mapping so we can normalize locals as well.
  private def normalizeTemps(body: List[TAC.Instr]): (List[TAC.Instr], Map[TAC.Temp, TAC.Temp]) = {
    val mapping = mutable.LinkedHashMap.empty[TAC.Temp, TAC.Temp]

    def canonTemp(t: TAC.Temp): TAC.Temp =
      mapping.getOrElseUpdate(t, TAC.Temp(mapping.size, t.len))

    def mapRhs(r: TAC.Rhs): TAC.Rhs = r match {
      case t: TAC.Temp => canonTemp(t)
      case it: TAC.IndirectTemp =>
        val base2 = canonTemp(it.base)
        val off2: (TAC.Temp | TAC.ImmValue) = it.offset match {
          case tt: TAC.Temp   => canonTemp(tt)
          case ii: TAC.ImmValue => ii
        }
        it.copy(base = base2, offset = off2)
      case other => other
    }

    def mapVal(v: TAC.Val): TAC.Val = v match {
      case t: TAC.Temp => canonTemp(t)
      case it: TAC.IndirectTemp => mapRhs(it).asInstanceOf[TAC.Val]
    }

    def mapInstr(i: TAC.Instr): TAC.Instr = i match {
      case TAC.BinOp(dst, op, lhs, rhs) => TAC.BinOp(canonTemp(dst), op, mapRhs(lhs), mapRhs(rhs))
      case TAC.UnOp(dst, op, x)         => TAC.UnOp(canonTemp(dst), op, mapRhs(x))
      case TAC.IntToFloat(dst, value)   => TAC.IntToFloat(canonTemp(dst), mapRhs(value))
      case TAC.TACAssign(lhs, rhs)      => TAC.TACAssign(mapVal(lhs), mapRhs(rhs))
      case TAC.TACExit(code)            => TAC.TACExit(mapRhs(code))
      case p: TAC.Print =>
        // content is Rhs
        p match {
          case TAC.PrintInt(content)     => TAC.PrintInt(mapRhs(content))
          case TAC.PrintChar(content)    => TAC.PrintChar(mapRhs(content))
          case TAC.PrintBool(content)    => TAC.PrintBool(mapRhs(content))
          case TAC.PrintFloat(content)   => TAC.PrintFloat(mapRhs(content))
          case TAC.PrintStr(content)     => TAC.PrintStr(mapRhs(content))
          case TAC.PrintPointer(content) => TAC.PrintPointer(mapRhs(content))
        }
      case TAC.TACReturn(value) =>
        TAC.TACReturn(mapRhs(value))
      case TAC.TACStoreException(value) =>
        TAC.TACStoreException(mapRhs(value))
      case TAC.TACLoadExceptionFlag(dst) =>
        TAC.TACLoadExceptionFlag(canonTemp(dst))
      case TAC.TACLoadExceptionValue(dst) =>
        TAC.TACLoadExceptionValue(canonTemp(dst))
      case TAC.TACClearException() =>
        TAC.TACClearException()
      case TAC.TACCall(dst, func, args) =>
        TAC.TACCall(canonTemp(dst), func, args.map(mapRhs))
      case TAC.TACRead(dst, t) =>
        TAC.TACRead(mapVal(dst), t)
      case TAC.TACFree(x, isPair) =>
        TAC.TACFree(mapRhs(x), isPair)
      case TAC.CmpJmp(cond, lhs, rhs, to) =>
        TAC.CmpJmp(cond, mapRhs(lhs), mapRhs(rhs), to) // label already normalized earlier
      case other => other
    }

    val out = body.map(mapInstr)
    (out, mapping.toMap)
  }

  test("LowerToTAC: begin skip end lowers to empty TAC body") {
    val got = lower(List(BeginEnd(List(Skip()))))
    assert(got == TAC.TACProgram(Seq(), Seq(), List(TAC.TACSkip()), Seq()))
  }

  test("LowerToTAC: exit(IntLit) lowers to TAC.Exit(ImmInt)") {
    val got = lower(List(Exit(IntLit(7))))
    assert(got == TAC.TACProgram(Seq(), Seq(), Seq(TAC.TACExit(TAC.ImmValue(7))), Seq()))
  }

  test("LowerToTAC: float arithmetic uses float-specific TAC operations") {
    val expr = BinaryArithmetic(FloatLit(1.5f), FloatLit(2.25f), ArithmeticOperation.Add)
    val got = lower(List(Print(expr)))
    val temp = TAC.Temp(0, TAC.BitLength._32)
    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.BinOp(temp, TAC.FloatArithOp.Add, TAC.FloatValue(1.5f), TAC.FloatValue(2.25f)),
      TAC.PrintFloat(temp)
    ), Seq(temp))
    assert(got == expect)
  }

  test("LowerToTAC: mixed arithmetic converts int operands before float operations") {
    val expr = BinaryArithmetic(IntLit(2), FloatLit(1.5f), ArithmeticOperation.Add)
    val got = lower(List(Print(expr)))
    val converted = TAC.Temp(0, TAC.BitLength._32)
    val result = TAC.Temp(1, TAC.BitLength._32)
    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.IntToFloat(converted, TAC.ImmValue(2)),
      TAC.BinOp(result, TAC.FloatArithOp.Add, converted, TAC.FloatValue(1.5f)),
      TAC.PrintFloat(result)
    ), Seq(converted, result))
    assert(got == expect)
  }

  test("LowerToTAC: float negation is lowered as zero minus the operand") {
    val got = lower(List(Print(Neg(FloatLit(3.5f)))))
    assert(got.body.head == TAC.BinOp(
      TAC.Temp(0, TAC.BitLength._32),
      TAC.FloatArithOp.Sub,
      TAC.FloatValue(0.0f),
      TAC.FloatValue(3.5f)
    ))
  }

  test("LowerToTAC: float comparisons use float-specific TAC operations") {
    val expr = BinaryCompare(FloatLit(1.0f), FloatLit(2.0f), CompareOperation.Less)
    val got = lower(List(Print(expr)))
    assert(got.body.head == TAC.BinOp(
      TAC.Temp(0, TAC.BitLength._8),
      TAC.FloatCondOp.LT,
      TAC.FloatValue(1.0f),
      TAC.FloatValue(2.0f)
    ))
  }

  test("LowerToTAC: begin-end is flattened") {
    val got = lower(List(BeginEnd(List(BeginEnd(List(Exit(IntLit(1))))))))
    assert(got == TAC.TACProgram(Seq(), Seq(), Seq(TAC.TACExit(TAC.ImmValue(1))), Seq()))
  }

  test("LowerToTAC: exit(Neg(IntLit)) lowers to UnOp then Exit(temp)") {
    val got = lower(List(Exit(Neg(IntLit(7)))))
    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.UnOp(TAC.Temp(0, TAC.BitLength._32), TAC.UnaryOp.Neg, TAC.ImmValue(7)),
      TAC.TACExit(TAC.Temp(0, TAC.BitLength._32))
    ), Seq(TAC.Temp(0, TAC.BitLength._32)))
    assert(got == expect)
  }

  test("LowerToTAC: exit(1 + 2) lowers to BinOp then Exit(temp)") {
    val expr = BinaryArithmetic(IntLit(1), IntLit(2), ArithmeticOperation.Add)
    val got = lower(List(Exit(expr)))
    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.BinOp(TAC.Temp(0, TAC.BitLength._32), TAC.ArithOp.Add, TAC.ImmValue(1), TAC.ImmValue(2)),
      TAC.TACExit(TAC.Temp(0, TAC.BitLength._32))
    ), Seq(TAC.Temp(0, TAC.BitLength._32)))
    assert(got == expect)
  }

  test("LowerToTAC: exit(1 + (2 * 3)) emits Mul before Add, temps increase") {
    val expr =
      BinaryArithmetic(
        IntLit(1),
        BinaryArithmetic(IntLit(2), IntLit(3), ArithmeticOperation.Mul),
        ArithmeticOperation.Add
      )

    val got = lower(List(Exit(expr)))
    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.BinOp(TAC.Temp(0, TAC.BitLength._32), TAC.ArithOp.Mul, TAC.ImmValue(2), TAC.ImmValue(3)),
      TAC.BinOp(TAC.Temp(1, TAC.BitLength._32), TAC.ArithOp.Add, TAC.ImmValue(1), TAC.Temp(0, TAC.BitLength._32)),
      TAC.TACExit(TAC.Temp(1, TAC.BitLength._32))
    ), Seq(TAC.Temp(0, TAC.BitLength._32), TAC.Temp(1, TAC.BitLength._32)))
    assert(got == expect)
  }

  test("LowerToTAC: exit((1 + 2) - (3 + 4)) preserves left-to-right lowering order") {
    val expr =
      BinaryArithmetic(
        BinaryArithmetic(IntLit(1), IntLit(2), ArithmeticOperation.Add),
        BinaryArithmetic(IntLit(3), IntLit(4), ArithmeticOperation.Add),
        ArithmeticOperation.Sub
      )

    val got = lower(List(Exit(expr)))
    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.BinOp(TAC.Temp(0, TAC.BitLength._32), TAC.ArithOp.Add, TAC.ImmValue(1), TAC.ImmValue(2)),
      TAC.BinOp(TAC.Temp(1, TAC.BitLength._32), TAC.ArithOp.Add, TAC.ImmValue(3), TAC.ImmValue(4)),
      TAC.BinOp(TAC.Temp(2, TAC.BitLength._32), TAC.ArithOp.Sub, TAC.Temp(0, TAC.BitLength._32), TAC.Temp(1, TAC.BitLength._32)),
      TAC.TACExit(TAC.Temp(2, TAC.BitLength._32))
    ), Seq(TAC.Temp(0, TAC.BitLength._32), TAC.Temp(1, TAC.BitLength._32), TAC.Temp(2, TAC.BitLength._32)))
    assert(got == expect)
  }

  test("LowerToTAC: multiple statements share one TempGen (temps keep increasing)") {
    val got = lower(List(
      Exit(Neg(IntLit(1))),
      Exit(BinaryArithmetic(IntLit(2), IntLit(3), ArithmeticOperation.Add))
    ))

    val expect = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.UnOp(TAC.Temp(0, TAC.BitLength._32), TAC.UnaryOp.Neg, TAC.ImmValue(1)),
      TAC.TACExit(TAC.Temp(0, TAC.BitLength._32)),
      TAC.BinOp(TAC.Temp(1, TAC.BitLength._32), TAC.ArithOp.Add, TAC.ImmValue(2), TAC.ImmValue(3)),
      TAC.TACExit(TAC.Temp(1, TAC.BitLength._32))
    ), Seq(TAC.Temp(0, TAC.BitLength._32), TAC.Temp(1, TAC.BitLength._32)))
    assert(got == expect)
  }

  test("LowerToTAC: simple if-else statement with skips") {
    val got = lower(List(
      IfElse(BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And),
        List(Skip()), List(Skip()))
    ))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      // && short-circuit expanded:
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)), // rhs
      TAC.Jmp(Label("and_end")),
      TAC.Mark(Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)),
      TAC.Mark(Label("and_end")),

      // if lowering (uses cond temp(0))
      TAC.CmpJmp(TAC.CondOp.EQ, TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8), Label("if_else")),
      TAC.TACSkip(),
      TAC.Jmp(Label("if_end")),
      TAC.Mark(Label("if_else")),
      TAC.TACSkip(),
      TAC.Mark(Label("if_end"))
    ), Seq(TAC.Temp(0, TAC.BitLength._8)))

    assert(normalizeProgram(got) == normalizeProgram(expected))
  }

  test("LowerToTAC: simple while loop statement with skips") {
    val got = lower(List(
      While(BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And), List(Skip()))
    ))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.Mark(Label("while_head")),

      // && short-circuit expanded:
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)),
      TAC.Jmp(Label("and_end")),
      TAC.Mark(Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)),
      TAC.Mark(Label("and_end")),

      // while condition jump (uses temp(0))
      TAC.CmpJmp(TAC.CondOp.EQ, TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8), Label("while_end")),
      TAC.TACSkip(),
      TAC.Jmp(Label("while_head")),
      TAC.Mark(Label("while_end"))
    ), Seq(TAC.Temp(0, TAC.BitLength._8)))

    assert(normalizeProgram(got) == normalizeProgram(expected))
  }

  test("LowerToTAC: switch dispatches grouped labels and falls back to default") {
    val got = lower(List(
      Switch(IntLit(3), List(
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedCaseLabel(IntLit(1))),
          List(Print(IntLit(10)), Break())
        ),
        TypedSwitchCaseBody(
          List(
            TypedSwitchLabel.TypedCaseLabel(IntLit(2)),
            TypedSwitchLabel.TypedCaseLabel(IntLit(3))
          ),
          List(Print(IntLit(20)), Break())
        ),
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedDefaultLabel()),
          List(Print(IntLit(30)))
        )
      ))
    ))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(3), ImmValue(1), Label("case_one")),
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(3), ImmValue(2), Label("case_two")),
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(3), ImmValue(3), Label("case_two")),
      TAC.Jmp(Label("default_case")),
      TAC.Mark(Label("case_one")),
      TAC.PrintInt(ImmValue(10)),
      TAC.Jmp(Label("switch_end")),
      TAC.Mark(Label("case_two")),
      TAC.PrintInt(ImmValue(20)),
      TAC.Jmp(Label("switch_end")),
      TAC.Mark(Label("default_case")),
      TAC.PrintInt(ImmValue(30)),
      TAC.Mark(Label("switch_end"))
    ), Seq())

    assert(normalizeProgram(got) == normalizeProgram(expected))
  }

  test("LowerToTAC: switch falls through until an explicit break") {
    val got = lower(List(
      Switch(IntLit(2), List(
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedCaseLabel(IntLit(1))),
          List(Print(IntLit(10)))
        ),
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedCaseLabel(IntLit(2))),
          List(Print(IntLit(20)))
        ),
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedCaseLabel(IntLit(3))),
          List(Print(IntLit(30)), Break())
        ),
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedDefaultLabel()),
          List(Print(IntLit(40)))
        )
      ))
    ))

    val body = got.body.toList
    val endLabel = body.last match {
      case TAC.Mark(label) => label
      case other => fail(s"expected switch end label, got: $other")
    }
    val print20 = body.indexOf(TAC.PrintInt(ImmValue(20)))
    val print30 = body.indexOf(TAC.PrintInt(ImmValue(30)))

    assert(print20 >= 0 && body(print20 + 1).isInstanceOf[TAC.Mark])
    assert(print30 >= 0 && body(print30 + 1) == TAC.Jmp(endLabel))
  }

  test("LowerToTAC: switch break and enclosing-loop continue use different targets") {
    val got = lower(List(
      While(BoolLit(true), List(
        Switch(IntLit(1), List(
          TypedSwitchCaseBody(
            List(TypedSwitchLabel.TypedCaseLabel(IntLit(1))),
            List(Continue())
          ),
          TypedSwitchCaseBody(
            List(TypedSwitchLabel.TypedDefaultLabel()),
            List(Break())
          )
        )),
        Break()
      ))
    ))

    val body = got.body.toList
    val markedLabels = body.collect { case TAC.Mark(label) => label }
    val whileHead = markedLabels.head
    val switchEnd = markedLabels.init.last
    val whileEnd = markedLabels.last

    assert(body.count(_ == TAC.Jmp(whileHead)) == 2)
    assert(body.contains(TAC.Jmp(switchEnd)))
    assert(body.contains(TAC.Jmp(whileEnd)))
  }

  test("LowerToTAC: switch evaluates its selector once") {
    val selector = BinaryArithmetic(IntLit(1), IntLit(2), ArithmeticOperation.Add)
    val got = lower(List(
      Switch(selector, List(
        TypedSwitchCaseBody(
          List(
            TypedSwitchLabel.TypedCaseLabel(IntLit(1)),
            TypedSwitchLabel.TypedCaseLabel(IntLit(3))
          ),
          List(Skip())
        )
      ))
    ))

    assert(got.body.count {
      case TAC.BinOp(_, TAC.ArithOp.Add, _, _) => true
      case _ => false
    } == 1)

    val comparisons = got.body.collect {
      case TAC.CmpJmp(TAC.CondOp.EQ, lhs, _, _) => lhs
    }
    assert(comparisons.size == 2)
    assert(comparisons.distinct.size == 1)
  }

  test("LowerToTAC: float switch uses floating-point equality") {
    val got = lower(List(
      Switch(FloatLit(1.5f), List(
        TypedSwitchCaseBody(
          List(TypedSwitchLabel.TypedCaseLabel(FloatLit(1.5f))),
          List(Skip())
        )
      ))
    ))

    assert(got.body.exists {
      case TAC.BinOp(_, TAC.FloatCondOp.EQ, TAC.FloatValue(1.5f, _), TAC.FloatValue(1.5f, _)) => true
      case _ => false
    })
  }

  private def idxOfFirst(body: List[TAC.Instr])(p: TAC.Instr => Boolean): Int =
    body.indexWhere(p)

  private def idxOfFirstDiv(body: List[TAC.Instr]): Int =
    idxOfFirst(body) {
      case TAC.BinOp(_, TAC.ArithOp.Div, _, _) => true
      case _ => false
    }

  test("LowerToTAC: OR short-circuits (CFG can jump to true before rhs)") {
    val rhs =
      BinaryCompare(
        BinaryArithmetic(IntLit(1), IntLit(0), ArithmeticOperation.Div),
        IntLit(2),
        CompareOperation.Greater
      )
    val expr = BinaryBool(BoolLit(true), rhs, BoolOperation.Or)

    val got0 = lower(List(Exit(expr)))
    val got = normalizeProgram(got0)
    val body = got.body.toList

    // Expect shape:
    // 0: CmpJmp(NEQ, ImmBool(1), ImmBool(0), Ltrue)
    // ... rhs lowering (Div) ...
    // Mark(Ltrue)
    body.head match {
      case TAC.CmpJmp(TAC.CondOp.NEQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), lTrue) =>
        val iDiv = idxOfFirstDiv(body)
        assert(iDiv >= 0, "rhs Div should appear in TAC body (we are not doing DCE/constant folding)")
        val iTrueMark = idxOfFirst(body) {
          case TAC.Mark(l) => l == lTrue
          case _ => false
        }
        assert(iTrueMark >= 0, "true label mark must exist")
        assert(iDiv < iTrueMark, "rhs code should be before the true-label mark (so jump can skip it)")
      case other =>
        fail(s"Expected first instr to be OR short-circuit CmpJmp(NEQ,...), got: $other")
    }
  }

  test("LowerToTAC: AND short-circuits (CFG can jump to false before rhs)") {
    val rhs = BinaryArithmetic(IntLit(1), IntLit(0), ArithmeticOperation.Div)
    val expr = BinaryBool(BoolLit(false), rhs, BoolOperation.And)

    val got0 = lower(List(Exit(expr)))
    val got  = normalizeProgram(got0)
    val body = got.body.toList

    // Expect first instr: if lv == 0 goto Lfalse
    body.head match {
      case TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(0, BitLength._8), ImmValue(0, BitLength._8), lFalse) =>
        val iDiv = idxOfFirstDiv(body)
        assert(iDiv >= 0, "rhs Div should appear in TAC body (we are not doing DCE/constant folding)")

        val iFalseMark = idxOfFirst(body) {
          case TAC.Mark(l) => l == lFalse
          case _ => false
        }
        assert(iFalseMark >= 0, "false label mark must exist")

        // rhs block is placed before the false-label mark, so the branch can skip it.
        assert(iDiv < iFalseMark, "rhs code should be before the false-label mark so the jump can skip it")
      case other =>
        fail(s"Expected first instr to be AND short-circuit CmpJmp(EQ, ImmBool(0), ImmBool(0), Lfalse), got: $other")
    }
  }

  test("LowerToTAC: nested boolean (a&&b)||c lowers to nested CFG") {
    val expr =
      BinaryBool(
        BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And),
        BoolLit(true),
        BoolOperation.Or
      )

    val got0 = lower(List(Exit(expr)))
    val got = normalizeProgram(got0)
    val body = got.body.toList

    val nCmpJmp = body.count { case TAC.CmpJmp(_, _, _, _) => true; case _ => false }
    val nMark = body.count { case TAC.Mark(_) => true; case _ => false }
    val nJmp = body.count { case TAC.Jmp(_) => true; case _ => false }

    // Should have at least two short-circuit structures -> multiple branches and marks.
    assert(nCmpJmp >= 2, s"expected >=2 CmpJmp, got $nCmpJmp")
    assert(nMark >= 3, s"expected >=3 Mark, got $nMark")
    assert(nJmp >= 2, s"expected >=2 Jmp, got $nJmp")
  }

  test("LowerToTAC: bool expression can be used inside compare: (true && false) == false") {
    val expr =
      BinaryCompare(
        BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And),
        BoolLit(false),
        CompareOperation.Equal
      )

    val got = normalizeProgram(lower(List(Exit(expr))))

    // Expected:
    // - AND short-circuit lowered into temp(0,_8)
    // - then a compare BinOp into temp(1,_8)
    // - then Exit(temp(1,_8))
    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      // AND:
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)), // rhs=false
      TAC.Jmp(Label("and_end")),
      TAC.Mark(Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)),
      TAC.Mark(Label("and_end")),

      // compare: (temp0 == false)
      TAC.BinOp(TAC.Temp(1, TAC.BitLength._8), TAC.CondOp.EQ, TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)),
      TAC.TACExit(TAC.Temp(1, TAC.BitLength._8))
    ), Seq(TAC.Temp(0, TAC.BitLength._8), TAC.Temp(1, TAC.BitLength._8)))

    assert(got == normalizeProgram(expected))
  }

  test("LowerToTAC: while with OR condition lowers correctly") {
    val got = normalizeProgram(lower(List(
      While(BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.Or), List(Skip()))
    )))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.Mark(Label("while_head")),

      // OR short-circuit expanded: if lv != 0 goto true
      TAC.CmpJmp(TAC.CondOp.NEQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("or_true")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)), // rhs=false
      TAC.Jmp(Label("or_end")),
      TAC.Mark(Label("or_true")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(1, BitLength._8)),
      TAC.Mark(Label("or_end")),

      // while condition jump
      TAC.CmpJmp(TAC.CondOp.EQ, TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8), Label("while_end")),
      TAC.TACSkip(),
      TAC.Jmp(Label("while_head")),
      TAC.Mark(Label("while_end"))
    ), Seq(TAC.Temp(0, TAC.BitLength._8)))

    assert(got == normalizeProgram(expected))
  }
  test("LowerToTAC: nested if inside while lowers with correct label structure") {
    // while (true && false) do
    //   if (true || false) then skip else skip fi
    // done
    val got = normalizeProgram(lower(List(
      While(
        BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And),
        List(
          IfElse(
            BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.Or),
            List(Skip()),
            List(Skip())
          )
        )
      )
    )))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      // while head
      TAC.Mark(Label("while_head")),

      // cond: (true && false)
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)), // rhs=false
      TAC.Jmp(Label("and_end")),
      TAC.Mark(Label("and_false")),
      TAC.TACAssign(TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8)),
      TAC.Mark(Label("and_end")),

      // while condition jump
      TAC.CmpJmp(TAC.CondOp.EQ, TAC.Temp(0, TAC.BitLength._8), ImmValue(0, BitLength._8), Label("while_end")),

      // body: if (true || false) then skip else skip fi
      // cond: (true || false)
      TAC.CmpJmp(TAC.CondOp.NEQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("or_true")),
      TAC.TACAssign(TAC.Temp(1, TAC.BitLength._8), ImmValue(0, BitLength._8)), // rhs=false
      TAC.Jmp(Label("or_end")),
      TAC.Mark(Label("or_true")),
      TAC.TACAssign(TAC.Temp(1, TAC.BitLength._8), ImmValue(1, BitLength._8)),
      TAC.Mark(Label("or_end")),

      // if lowering (uses cond temp(1))
      TAC.CmpJmp(TAC.CondOp.EQ, TAC.Temp(1, TAC.BitLength._8), ImmValue(0, BitLength._8), Label("if_else")),
      TAC.TACSkip(),
      TAC.Jmp(Label("if_end")),
      TAC.Mark(Label("if_else")),
      TAC.TACSkip(),
      TAC.Mark(Label("if_end")),

      // loop back
      TAC.Jmp(Label("while_head")),
      TAC.Mark(Label("while_end"))
    ), Seq(
      TAC.Temp(0, TAC.BitLength._8),
      TAC.Temp(1, TAC.BitLength._8)
    ))

    assert(got == normalizeProgram(expected))
  }

  test("LowerToTAC: while inside if lowers with correct head/end labels") {
    // if true then while true do skip done else skip fi
    val got = normalizeProgram(lower(List(
      IfElse(
        BoolLit(true),
        List(While(BoolLit(true), List(Skip()))),
        List(Skip())
      )
    )))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      // if cond jump false -> else
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("if_else")),

      // then branch: while
      TAC.Mark(Label("while_head")),
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("while_end")),
      TAC.TACSkip(),
      TAC.Jmp(Label("while_head")),
      TAC.Mark(Label("while_end")),

      // jump over else
      TAC.Jmp(Label("if_end")),

      // else
      TAC.Mark(Label("if_else")),
      TAC.TACSkip(),
      TAC.Mark(Label("if_end"))
    ), Seq())

    assert(got == normalizeProgram(expected))
  }

  test("LowerToTAC: short-circuit with non-trivial lhs (needs temp) preserves evaluation order") {
    // (1+2==3) && (4/2==2)
    val lhs =
      BinaryCompare(
        BinaryArithmetic(IntLit(1), IntLit(2), ArithmeticOperation.Add),
        IntLit(3),
        CompareOperation.Equal
      )

    val rhs =
      BinaryCompare(
        BinaryArithmetic(IntLit(4), IntLit(2), ArithmeticOperation.Div),
        IntLit(2),
        CompareOperation.Equal
      )

    val expr = BinaryBool(lhs, rhs, BoolOperation.And)
    val got0 = lower(List(Exit(expr)))
    val got  = normalizeProgram(got0)
    val body = got.body.toList

    def idxOfFirstAdd: Int =
      idxOfFirst(body) {
        case TAC.BinOp(_, TAC.ArithOp.Add, _, _) => true
        case _ => false
      }

    def idxOfFirstCmpJmp: Int =
      idxOfFirst(body) {
        case TAC.CmpJmp(_, _, _, _) => true
        case _ => false
      }

    val iAdd   = idxOfFirstAdd
    val iCjmp  = idxOfFirstCmpJmp
    val iDiv   = idxOfFirstDiv(body)

    assert(iAdd >= 0, "lhs Add should appear")
    assert(iCjmp >= 0, "AND short-circuit CmpJmp should appear")
    assert(iDiv >= 0, "rhs Div should appear (no DCE/const-folding)")

    // lowering order guaranteed by lowerShortCircuitBool:
    // - lowerExpr(lhs) emits Add + EQ compare BEFORE emitting the CmpJmp
    // - lowerExpr(rhs) (Div) happens AFTER the CmpJmp
    assert(iAdd < iCjmp, "lhs must be evaluated before AND short-circuit branch")
    assert(iCjmp < iDiv, "rhs must be emitted after the short-circuit branch (so it can be skipped)")
  }

  test("LowerToTAC: multi-level boolean chain a&&b&&c lowers as right-associative") {
    // a && (b && c)  (right-associative form)
    val expr =
      BinaryBool(
        BoolLit(true),
        BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And),
        BoolOperation.And
      )

    val got = normalizeProgram(lower(List(Exit(expr))))
    val body = got.body.toList

    val nCmpJmp = body.count { case TAC.CmpJmp(_, _, _, _) => true; case _ => false }
    val nMark   = body.count { case TAC.Mark(_) => true; case _ => false }
    val nJmp    = body.count { case TAC.Jmp(_) => true; case _ => false }

    // Two nested AND short-circuits => should see >=2 branches and multiple marks/jumps.
    assert(nCmpJmp >= 2, s"expected >=2 CmpJmp for nested &&, got $nCmpJmp")
    assert(nMark >= 3,  s"expected >=3 Mark for nested &&, got $nMark")
    assert(nJmp >= 2,   s"expected >=2 Jmp for nested &&, got $nJmp")

    // Outer AND with literal lhs: first instr should be its CmpJmp
    body.head match {
      case TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), _) => succeed
      case other => fail(s"expected first instr to be outer AND CmpJmp(EQ, true, 0, ...), got: $other")
    }
  }

  test("LowerToTAC: boolean expressions inside array index are lowered correctly") {
    // We build a typedAST directly (unit test), so we can put a Bool expr as an index
    // to stress lowering order: index lowering (short-circuit) must appear before the IndirectTemp load.
    //
    // int[] a = [10, 20];
    // exit a[ true && false ];
    val aId = TypedLValue.Id("a", SemArray(SemInt, 1))

    val prog = List(
      Decl(aId, TypedRValue.ArrayLit(List(IntLit(10), IntLit(20)), SemArray(SemInt, 1))),
      Exit(
        TypedLValue.ArrayElem(
          aId,
          List(BinaryBool(BoolLit(true), BoolLit(false), BoolOperation.And)),
          SemInt
        )
      )
    )

    val got = normalizeProgram(lower(prog))
    val body = got.body.toList

    // Find the first array-element load: TACAssign(dst, IndirectTemp(_, _, isArray=true, ...))
    val iLoad = idxOfFirst(body) {
      case TAC.TACAssign(_, it: TAC.IndirectTemp) if it.isArray => true
      case _ => false
    }
    assert(iLoad >= 0, "should contain an array element load via IndirectTemp(isArray=true)")

    // Find the first short-circuit branch used to compute the index: CmpJmp(EQ, true, 0, Lfalse)
    val iIdxBranch = idxOfFirst(body) {
      case TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), _) => true
      case _ => false
    }
    assert(iIdxBranch >= 0, "should contain short-circuit code for (true && false) index expression")

    // The index expression lowering must appear before the array load that consumes it.
    assert(iIdxBranch < iLoad, "index expression (short-circuit) must be lowered before array element load")
  }

  test("LowerToTAC: break in while jumps to loop end") {
    val got = normalizeProgram(lower(List(
      While(BoolLit(true), List(Break()))
    )))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.Mark(Label("while_head")),
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("while_end")),
      TAC.Jmp(Label("while_end")),
      TAC.Jmp(Label("while_head")),
      TAC.Mark(Label("while_end"))
    ), Seq())

    assert(got == normalizeProgram(expected))
  }

  test("LowerToTAC: continue in while jumps to loop head") {
    val got = normalizeProgram(lower(List(
      While(BoolLit(true), List(Continue()))
    )))

    val expected = TAC.TACProgram(Seq(), Seq(), Seq(
      TAC.Mark(Label("while_head")),
      TAC.CmpJmp(TAC.CondOp.EQ, ImmValue(1, BitLength._8), ImmValue(0, BitLength._8), Label("while_end")),
      TAC.Jmp(Label("while_head")),
      TAC.Jmp(Label("while_head")),
      TAC.Mark(Label("while_end"))
    ), Seq())

    assert(got == normalizeProgram(expected))
  }

  test("LowerToTAC: throw stores exception then exits in main") {
    val got = lower(List(Throw(ArrayOutOfBoundsEx(StrLit("array out of bound")))))
    val expected = TAC.TACProgram(Vector("array out of bound"), Seq(), Seq(
      TAC.TACAssign(TAC.Temp(0, BitLength._ptr), TAC.Pair(ImmValue(3, BitLength._32), TAC.TACStr(0, BitLength._ptr))),
      TAC.TACStoreException(TAC.Temp(0, BitLength._ptr)),
      TAC.TACReportError(TAC.Temp(0, _ptr)),
      TAC.TACExit(TAC.ImmValue(255, BitLength._32)),
    ), Seq(TAC.Temp(0, BitLength._ptr)))

    assert(got == expected)
  }

  test("LowerToTAC: general exception stores the general exception id") {
    val got = lower(List(Throw(GeneralEx(StrLit("general failure")))))
    val expected = TAC.TACProgram(Vector("general failure"), Seq(), Seq(
      TAC.TACAssign(TAC.Temp(0, BitLength._ptr), TAC.Pair(ImmValue(6, BitLength._32), TAC.TACStr(0, BitLength._ptr))),
      TAC.TACStoreException(TAC.Temp(0, BitLength._ptr)),
      TAC.TACReportError(TAC.Temp(0, _ptr)),
      TAC.TACExit(TAC.ImmValue(255, BitLength._32)),
    ), Seq(TAC.Temp(0, BitLength._ptr)))

    assert(got == expected)
  }

  test("LowerToTAC: division by zero inside try lowers to arithmetic exception flow") {
    val prog = TypedProgram(
      funcs = Nil,
      stmts = List(
        TryCatch(
          List(Decl(TypedLValue.Id("x", SemInt), TypedRValue.ExprR(BinaryArithmetic(IntLit(1), IntLit(0), ArithmeticOperation.Div)))),
          List(TypedCatchHandler(Arithmetic()(PositionInfo(1, 1)), TypedLValue.Id("e", SemString), List(Print(TypedLValue.Id("e", SemString)))))
        )
      )
    )
    val lowered = LowerToTAC.fromTypedProgram(prog, A64_WORD_SIZE)
    val got = normalizeProgram(lowered.copy(body = lowered.body.dropWhile {
      case TAC.TACClearException() => true
      case _ => false
    }))
    val body = got.body.toList
    assert(body.exists { case TAC.CmpJmp(TAC.CondOp.NEQ, _, TAC.ImmValue(0, _), _) => true; case _ => false })
    assert(body.exists { case TAC.TACStoreException(_) => true; case _ => false })
    assert(body.exists { case TAC.TACClearException() => true; case _ => false })
    assert(body.exists { case TAC.PrintStr(_) => true; case _ => false })
  }

  test("LowerToTAC: integer overflow inside try lowers to integer-overflow exception flow") {
    val prog = TypedProgram(
      funcs = Nil,
      stmts = List(
        TryCatch(
          List(Decl(TypedLValue.Id("x", SemInt), TypedRValue.ExprR(BinaryArithmetic(IntLit(Int.MaxValue), IntLit(1), ArithmeticOperation.Add)))),
          List(TypedCatchHandler(IntegerOverflow()(PositionInfo(1, 1)), TypedLValue.Id("e", SemString), List(Print(TypedLValue.Id("e", SemString)))))
        )
      )
    )
    val lowered = LowerToTAC.fromTypedProgram(prog, A64_WORD_SIZE)
    val got = normalizeProgram(lowered.copy(body = lowered.body.dropWhile {
      case TAC.TACClearException() => true
      case _ => false
    }))
    val body = got.body.toList
    assert(body.exists { case TAC.CheckedBinOp(_, TAC.ArithOp.Add, _, _, _) => true; case _ => false })
    assert(body.exists { case TAC.TACStoreException(_) => true; case _ => false })
    assert(body.exists { case TAC.TACClearException() => true; case _ => false })
    assert(body.exists { case TAC.PrintStr(_) => true; case _ => false })
  }

  test("LowerToTAC: call checks exception flag and jumps to caller catch") {
    val callee = TypedFunction(
      SemInt,
      "boom",
      "boom__V",
      Nil,
      List(Throw(ArrayOutOfBoundsEx(StrLit("array out of bound"))))
    )
    val prog = TypedProgram(
      funcs = List(callee),
      stmts = List(
        TryCatch(
          List(Decl(TypedLValue.Id("x", SemInt), TypedRValue.Call("boom", "boom__V", Nil, SemInt, Nil))),
          List(TypedCatchHandler(ArrayOutOfBounds()(PositionInfo(1, 1)), TypedLValue.Id("e", SemString), List(Print(TypedLValue.Id("e", SemInt)))))
        )
      )
    )

    val lowered = LowerToTAC.fromTypedProgram(prog, A64_WORD_SIZE)
    val got = normalizeProgram(lowered.copy(body = lowered.body.dropWhile {
      case TAC.TACClearException() => true
      case _ => false
    }))

    val body = got.body.toList
    assert(body.exists { case TAC.TACLoadExceptionFlag(_) => true; case _ => false })
    assert(body.exists { case TAC.TACLoadExceptionValue(_) => true; case _ => false })
    assert(body.exists { case TAC.TACClearException() => true; case _ => false })
    assert(body.exists { case TAC.PrintInt(_) => true; case _ => false })
  }
}
