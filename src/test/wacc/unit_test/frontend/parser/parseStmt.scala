package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Success, Failure}

import wacc.frontend.parser
import wacc.frontend.ast.Expr.*
import wacc.frontend.ast.Stmt.*
import wacc.frontend.ast.LValue.*
import wacc.frontend.ast.RValue.*
import wacc.frontend.ast.*
import wacc.frontend.ast.WACCException.*

class PairElemParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "pairElem parser" should "parse fst applied to identifier lvalue" in {
    p.parsePairElem("fst x") match {
      case Success(PFst(LIdent(Identifier("x")))) => succeed
      case Success(other)                         => fail(s"Unexpected parse result: $other")
      case Failure(err)                           => fail(s"Parsing failed: $err")
    }
  }

  it should "parse snd applied to array element lvalue" in {
    p.parsePairElem("snd arr[1]") match {
      case Success(PSnd(LArray(ArrayElem(Identifier("arr"), Seq(IntLiter(1)))))) => succeed
      case Success(other)                                                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                          => fail(s"Parsing failed: $err")
    }
  }

  it should "parse nested pairElem lvalue: fst (snd x)" in {
    p.parsePairElem("fst snd x") match {
      case Success(PFst(LPairElem(PSnd(LIdent(Identifier("x")))))) => succeed
      case Success(other)                                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                                            => fail(s"Parsing failed: $err")
    }
  }
}

class LValueParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "lvalue parser" should "parse identifier lvalue" in {
    p.parseLValue("x") match {
      case Success(LIdent(Identifier("x"))) => succeed
      case Success(other)                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                     => fail(s"Parsing failed: $err")
    }
  }

  it should "parse array element lvalue (multi index)" in {
    p.parseLValue("arr[i][1+2]") match {
      case Success(
      LArray(
      ArrayElem(
      Identifier("arr"),
      Seq(Identifier("i"), Add(IntLiter(1), IntLiter(2)))
      )
      )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse pairElem as an lvalue (fst)" in {
    p.parseLValue("fst x") match {
      case Success(LPairElem(PFst(LIdent(Identifier("x"))))) => succeed
      case Success(other)                                    => fail(s"Unexpected parse result: $other")
      case Failure(err)                                      => fail(s"Parsing failed: $err")
    }
  }
}

class RValueParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "rvalue parser" should "parse expr rvalues such as 1 + 2 and len a" in {
    p.parseRValue("1 + 2") match {
      case Success(RExpr(Add(IntLiter(1), IntLiter(2)))) => succeed
      case Success(other)                                => fail(s"Unexpected parse result: $other")
      case Failure(err)                                  => fail(s"Parsing failed: $err")
    }

    p.parseRValue("len x") match {
      case Success(RExpr(Len(Identifier("x")))) => succeed
      case Success(other)                       => fail(s"Unexpected parse result: $other")
      case Failure(err)                         => fail(s"Parsing failed: $err")
    }
  }

  it should "parse newpair declaration" in {
    p.parseRValue("newpair(1, 2)") match {
      case Success(RNewPair(IntLiter(1), IntLiter(2))) => succeed
      case Success(other)                              => fail(s"Unexpected parse result: $other")
      case Failure(err)                                => fail(s"Parsing failed: $err")
    }

    p.parseRValue("newpair(1, \"abc\")") match {
      case Success(RNewPair(IntLiter(1), StringLiter("abc"))) => succeed
      case Success(other)                                     => fail(s"Unexpected parse result: $other")
      case Failure(err)                                       => fail(s"Parsing failed: $err")
    }

    p.parseRValue("newpair(1 + 2, len a)") match {
      case Success(RNewPair(Add(IntLiter(1), IntLiter(2)), Len(Identifier("a")))) => succeed
      case Success(other)                                                         => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                           => fail(s"Parsing failed: $err")
    }
  }

  it should "parse array-liter" in {
    p.parseRValue("[1, 2, 3]") match {
      case Success(RArrayLiter(List(IntLiter(1), IntLiter(2), IntLiter(3)))) => succeed
      case Success(other)                                                    => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                      => fail(s"Parsing failed: $err")
    }

    p.parseRValue("[ident, 1 + 2, len true]") match {
      case Success(RArrayLiter(List(Identifier("ident"), Add(IntLiter(1), IntLiter(2)), Len(BooleanLiter(true))))) => succeed
      case Success(other)                                                                                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                                            => fail(s"Parsing failed: $err")
    }

    p.parseRValue("[1 && true, (3 / 2)]") match {
      case Success(RArrayLiter(List(And(IntLiter(1), BooleanLiter(true)), Parens(Div(IntLiter(3), IntLiter(2)))))) => succeed
      case Success(other)                                                                                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                                            => fail(s"Parsing failed: $err")
    }
  }

  it should "parse pair-elem" in {
    p.parseRValue("fst ident") match {
      case Success(RPairElem(PFst(LIdent(Identifier("ident"))))) => succeed
      case Success(other)                                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                                          => fail(s"Parsing failed: $err")
    }

    p.parseRValue("snd arr[1]") match {
      case Success(RPairElem(PSnd(LArray(ArrayElem(Identifier("arr"), Seq(IntLiter(1))))))) => succeed
      case Success(other)                                                                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                     => fail(s"Parsing failed: $err")
    }

    p.parseRValue("fst snd arr[1]") match {
      case Success(RPairElem(PFst(LPairElem(PSnd(LArray(ArrayElem(Identifier("arr"), Seq(IntLiter(1))))))))) => succeed
      case Success(other)                                                                                    => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                                      => fail(s"Parsing failed: $err")
    }
  }

  it should "parse function call" in {
    p.parseRValue("call f()") match {
      case Success(RCall(Identifier("f"), List())) => succeed
      case Success(other)                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                            => fail(s"Parsing failed: $err")
    }

    p.parseRValue("call function(1 + 2, chr \'a\')") match {
      case Success(RCall(Identifier("function"), List(Add(IntLiter(1), IntLiter(2)), Chr(CharLiter('a'))))) => succeed
      case Success(other)                                                                                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                                     => fail(s"Parsing failed: $err")
    }
  }
}

class StmtParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "stmt parser" should "skip statement" in {
    p.parseStmt("skip") match {
      case Success(Skip()) => succeed
      case Success(other)  => fail(s"Unexpected parse result: $other")
      case Failure(err)    => fail(s"Parsing failed: $err")
    }
  }

  it should "parse variable declaration" in {
    p.parseStmt("int x = 1 + 2") match {
      case Success(Decl(IntType(), Identifier("x"), RExpr(Add(IntLiter(1), IntLiter(2))))) => succeed
      case Success(other)                                                                  => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                    => fail(s"Parsing failed: $err")
    }
  }

  it should "parse assignment" in {
    p.parseStmt("x = chr 90") match {
      case Success(Assign(LIdent(Identifier("x")), RExpr(Chr(IntLiter(90))))) => succeed
      case Success(other)                                                     => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                       => fail(s"Parsing failed: $err")
    }

    p.parseStmt("fst x = call f(true)") match {
      case Success(Assign(LPairElem(PFst(LIdent(Identifier("x")))), RCall(Identifier("f"), List(BooleanLiter(true))))) => succeed
      case Success(other)                                                                                                => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                                                  => fail(s"Parsing failed: $err")
    }

    p.parseStmt("arr[1 + 2] = fst p") match {
      case Success(
      Assign(
      LArray(ArrayElem(Identifier("arr"), Seq(Add(IntLiter(1), IntLiter(2))))),
      RPairElem(PFst(LIdent(Identifier("p"))))
      )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse read statement" in {
    p.parseStmt("read x") match {
      case Success(Read(LIdent(Identifier("x")))) => succeed
      case Success(other)                         => fail(s"Unexpected parse result: $other")
      case Failure(err)                           => fail(s"Parsing failed: $err")
    }

    p.parseStmt("read fst x") match {
      case Success(Read(LPairElem(PFst(LIdent(Identifier("x")))))) => succeed
      case Success(other)                                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                                            => fail(s"Parsing failed: $err")
    }

    p.parseStmt("read arr[1]") match {
      case Success(Read(LArray(ArrayElem(Identifier("arr"), Seq(IntLiter(1)))))) => succeed
      case Success(other)                                                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                          => fail(s"Parsing failed: $err")
    }
  }

  it should "parse free statement" in {
    p.parseStmt("free arr") match {
      case Success(Free(Identifier("arr"))) => succeed
      case Success(other)                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                     => fail(s"Parsing failed: $err")
    }

    p.parseStmt("free 1 + true") match {
      case Success(Free(Add(IntLiter(1), BooleanLiter(true)))) => succeed
      case Success(other)                                      => fail(s"Unexpected parse result: $other")
      case Failure(err)                                        => fail(s"Parsing failed: $err")
    }
  }

  it should "parse return statement" in {
    p.parseStmt("return !false") match {
      case Success(Return(Not(BooleanLiter(false)))) => succeed
      case Success(other)                            => fail(s"Unexpected parse result: $other")
      case Failure(err)                              => fail(s"Parsing failed: $err")
    }

    p.parseStmt("return(\'a\')") match {
      case Success(Return(Parens(CharLiter('a')))) => succeed
      case Success(other)                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                            => fail(s"Parsing failed: $err")
    }
  }

  it should "parse exit statement" in {
    p.parseStmt("exit -10") match {
      case Success(Exit(IntLiter(-10))) => succeed
      case Success(other)               => fail(s"Unexpected parse result: $other")
      case Failure(err)                 => fail(s"Parsing failed: $err")
    }
  }

  it should "parse print and println statements" in {
    p.parseStmt("print-1") match {
      case Success(Print(IntLiter(-1))) => succeed
      case Success(other)               => fail(s"Unexpected parse result: $other")
      case Failure(err)                 => fail(s"Parsing failed: $err")
    }

    expectFailure(p.parseStmt("println11"))
  }

  it should "parse if else statement" in {
    p.parseStmt("if x == 1 then return true else return false fi") match {
      case Success(If(Equal(Identifier("x"), IntLiter(1)), Return(BooleanLiter(true)), Return(BooleanLiter(false)))) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err) => fail(s"Parsing failed: $err")
    }

    p.parseStmt("if len arr == 10 then arr = call f(); print 1 else arr = newpair(1, false) fi") match {
      case Success(
      If(
      Equal(Len(Identifier("arr")), IntLiter(10)),
      SeqStmt(
      List(
      Assign(LIdent(Identifier("arr")), RCall(Identifier("f"), List())),
      Print(IntLiter(1))
      )
      ),
      Assign(LIdent(Identifier("arr")), RNewPair(IntLiter(1), BooleanLiter(false)))
      )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }

    expectFailure(p.parseStmt("if true then a = 1 fi"))
  }

  it should "parse while statement" in {
    p.parseStmt("while x >= 10 do x = x - 1 done") match {
      case Success(While(GreaterEqual(Identifier("x"), IntLiter(10)),
      Assign(LIdent(Identifier("x")), RExpr(Sub(Identifier("x"), IntLiter(1)))))) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }

    p.parseStmt("while len x == 0 do x = x - 1; println x done") match {
      case Success(
        While(
          Equal(Len(Identifier("x")), IntLiter(0)),
          SeqStmt(List(
            Assign(
              LIdent(Identifier("x")),
              RExpr(Sub(Identifier("x"), IntLiter(1)))
            ),
            Println(Identifier("x"))
          ))
        )
      ) => succeed

      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }


    expectFailure(p.parseStmt("while x == 0 do x = x - 1; done"))
    expectFailure(p.parseStmt("while ord 1 != true do skip;; x = 10 done"))
  }

  it should "parse with begin .. end block" in {
    p.parseStmt("begin skip end") match {
      case Success(BeginEnd(Skip())) => succeed
      case Success(other)            => fail(s"Unexpected parse result: $other")
      case Failure(err)              => fail(s"Parsing failed: $err")
    }

    p.parseStmt("begin int x = 1; println x; exit 0 end") match {
      case Success(
      BeginEnd(
      SeqStmt(
      List(
      Decl(IntType(), Identifier("x"), RExpr(IntLiter(1))),
      Println(Identifier("x")),
      Exit(IntLiter(0))
      )
      )
      )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse for basic loop statement" in {
    p.parseStmt("for (int x = 1, x <= 10, x = x + 1) int y = x * 2; println y done") match {
      case Success(
        For(
          Decl(IntType(), Identifier("x"), RExpr(IntLiter(1))),
          LessEqual(Identifier("x"), IntLiter(10)),
          Assign(LIdent(Identifier("x")), RExpr(Add(Identifier("x"), IntLiter(1)))),
          SeqStmt(List(
            Decl(IntType(), Identifier("y"), RExpr(Mul(Identifier("x"), IntLiter(2)))),
            Println(Identifier("y"))
          ))
        )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse a basic do-while loop statement" in {
    p.parseStmt("do int x = 1; int y = x * 2; x = x + 1 while x <= 10") match {
      case Success(
        DoWhile(
          SeqStmt(List(
            Decl(IntType(), Identifier("x"), RExpr(IntLiter(1))),
            Decl(IntType(), Identifier("y"), RExpr(Mul(Identifier("x"), IntLiter(2)))),
            Assign(LIdent(Identifier("x")), RExpr(Add(Identifier("x"), IntLiter(1))))
          )),
          LessEqual(Identifier("x"), IntLiter(10))
        )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse throw and try-catch statements" in {
    p.parseStmt("throw ArrayOutOfBoundsException(\"array out of bound\")") match {
      case Success(Throw(ArrayOutOfBoundsException(StringLiter("array out of bound")))) => succeed
      case Success(other)               => fail(s"Unexpected parse result: $other")
      case Failure(err)                 => fail(s"Parsing failed: $err")
    }

    p.parseStmt("try throw ArrayOutOfBoundsException(\"array out of bound\") catch ArrayOutOfBoundsException err do print err done") match {
      case Success(
        TryCatch(
          Throw(ArrayOutOfBoundsException(StringLiter("array out of bound"))),
          List(CatchHandler(
            ArrayOutOfBounds(), 
            Identifier("err"),
            Print(Identifier("err"))
          ))
        )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }

    p.parseStmt("try throw Exception(\"general\") catch Exception err do print err done") match {
      case Success(
        TryCatch(
          Throw(GeneralException(StringLiter("general"))),
          List(CatchHandler(
            General(),
            Identifier("err"),
            Print(Identifier("err"))
          ))
        )
      ) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }


  it should "parse break and continue statements" in {
    p.parseStmt("Break") match {
      case Success(Break()) => succeed
      case Success(other)   => fail(s"Unexpected parse result: $other")
      case Failure(err)     => fail(s"Parsing failed: $err")
    }

    p.parseStmt("Continue") match {
      case Success(Continue()) => succeed
      case Success(other)      => fail(s"Unexpected parse result: $other")
      case Failure(err)        => fail(s"Parsing failed: $err")
    }
  }
}

