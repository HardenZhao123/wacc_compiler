package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}

import wacc.frontend.parser
import wacc.frontend.ast.Expr.*

class ExprParserTest extends AnyFlatSpec with ParserTestHelpers {

  private val p = parser

  "expr parser" should "parse a simple integer" in {
    val r = p.parseExpr("    123  ")
    r match {
      case Success(IntLiter(v)) => v shouldBe 123
      case Success(other)       => fail(s"Unexpected parse result: $other")
      case Failure(err)         => fail(s"Parsing failed: $err")
    }
  }

  it should "parse an addition" in {
    val r = p.parseExpr("  1    +  2")
    r match {
      case Success(Add(IntLiter(1), IntLiter(2))) => succeed
      case Success(other)                         => fail(s"Unexpected parse result: $other")
      case Failure(err)                           => fail(s"Parsing failed: $err")
    }
  }

  it should "parse addition and multiplication with precedence" in {
    val r = p.parseExpr("1 + 2 * 3")
    r match {
      case Success(Add(IntLiter(1), Mul(IntLiter(2), IntLiter(3)))) => succeed
      case Success(other)                                           => fail(s"Unexpected parse result: $other")
      case Failure(err)                                             => fail(s"Parsing failed: $err")
    }
  }

  it should "parse pair-liter" in {
    val r = p.parseExpr("  null")
    r match {
      case Success(PairLiter()) => succeed
      case Success(other)       => fail(s"Unexpected parse result: $other")
      case Failure(err)         => fail(s"Parsing failed: $err")
    }
  }

  it should "parse identifier" in {
    val r = p.parseExpr("identifier")
    r match {
      case Success(Identifier("identifier")) => succeed
      case Success(other)                    => fail(s"Unexpected parse result: $other")
      case Failure(err)                      => fail(s"Parsing failed: $err")
    }
  }

  it should "parse array-elem" in {
    val r = p.parseExpr("arr[5]")
    r match {
      case Success(ArrayElem(Identifier("arr"), List(IntLiter(5)))) => succeed
      case Success(other)                                           => fail(s"Unexpected parse result: $other")
      case Failure(err)                                             => fail(s"Parsing failed: $err")
    }

    val r2 = p.parseExpr("arr[4][5]")
    r2 match {
      case Success(ArrayElem(Identifier("arr"), List(IntLiter(4), IntLiter(5)))) => succeed
      case Success(other)                                                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                          => fail(s"Parsing failed: $err")
    }
  }

  it should "parse parentheses overriding precedence" in {
    val r = p.parseExpr("(1 + 2) * 3")
    r match {
      case Success(Mul(Parens(Add(IntLiter(1), IntLiter(2))), IntLiter(3))) => succeed
      case Success(other)                                                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                     => fail(s"Parsing failed: $err")
    }
  }

  it should "parse chained unary operators" in {
    val r1 = p.parseExpr("!!false")
    r1 match {
      case Success(Not(Not(BooleanLiter(false)))) => succeed
      case Success(other)                         => fail(s"Unexpected parse result: $other")
      case Failure(err)                           => fail(s"Parsing failed: $err")
    }

    val r2 = p.parseExpr("len chr 97")
    r2 match {
      case Success(Len(Chr(IntLiter(97)))) => succeed
      case Success(other)                  => fail(s"Unexpected parse result: $other")
      case Failure(err)                    => fail(s"Parsing failed: $err")
    }
  }

  it should "parse comparisons and boolean operators with expected precedence" in {
    val r1 = p.parseExpr("1 < 2")
    r1 match {
      case Success(Less(IntLiter(1), IntLiter(2))) => succeed
      case Success(other)                          => fail(s"Unexpected parse result: $other")
      case Failure(err)                            => fail(s"Parsing failed: $err")
    }

    // expected: (x == 1) && true
    val r2 = p.parseExpr("x == 1 && true")
    r2 match {
      case Success(And(Equal(Identifier("x"), IntLiter(1)), BooleanLiter(true))) => succeed
      case Success(other)                                                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                          => fail(s"Parsing failed: $err")
    }

    val r3 = p.parseExpr("true || false")
    r3 match {
      case Success(Or(BooleanLiter(true), BooleanLiter(false))) => succeed
      case Success(other)                                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                                          => fail(s"Parsing failed: $err")
    }
  }

  it should "parse bitwise-operations" in {
    val r1 = p.parseExpr("1 & 2")
    r1 match {
      case Success(BitAnd(IntLiter(1), IntLiter(2))) => succeed
      case Success(other)                            => fail(s"Unexpected parse result: $other")
      case Failure(err)                              => fail(s"Parsing failed: $err")
    }

    val r2 = p.parseExpr("1 | 2")
    r2 match {
      case Success(BitOr(IntLiter(1), IntLiter(2))) => succeed
      case Success(other)                           => fail(s"Unexpected parse result: $other")
      case Failure(err)                             => fail(s"Parsing failed: $err")
    }

    val r3 = p.parseExpr("~1")
    r3 match {
      case Success(BitNot(IntLiter(1))) => succeed
      case Success(other)               => fail(s"Unexpected parse result: $other")
      case Failure(err)                 => fail(s"Parsing failed: $err")
    }
  }
}
