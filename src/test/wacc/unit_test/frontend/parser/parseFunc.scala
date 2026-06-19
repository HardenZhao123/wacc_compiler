package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}

import wacc.frontend.parser
import wacc.frontend.ast.Expr.*
import wacc.frontend.ast.Stmt.*
import wacc.frontend.ast.*

class ParamParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "param parser" should "parse a base-type parameter" in {
    p.parseParam("int x") match {
      case Success(Param(t, Identifier(name))) =>
        name shouldBe "x"
        t match {
          case IntType() => succeed
          case other     => fail(s"Expected IntType, got $other")
        }
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse an array-type parameter" in {
    p.parseParam("int[][] xs") match {
      case Success(Param(t, Identifier(name))) =>
        name shouldBe "xs"
        t match {
          case ArrayType(elem, 2) =>
            elem match {
              case IntType() => succeed
              case other     => fail(s"Expected element IntType, got $other")
            }
          case other => fail(s"Expected ArrayType(_,2), got $other")
        }
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse a pair-type parameter" in {
    p.parseParam("pair(int, bool) p") match {
      case Success(Param(t, Identifier(name))) =>
        name shouldBe "p"
        t match {
          case PairType(fst, snd) =>
            fst match {
              case IntType() => succeed
              case other     => fail(s"Expected fst IntType, got $other")
            }
            snd match {
              case BoolType() => succeed
              case other      => fail(s"Expected snd BoolType, got $other")
            }
          case other => fail(s"Expected PairType(_, _), got $other")
        }
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse arrays of pairs as a parameter type" in {
    p.parseParam("pair(int, bool)[] ps") match {
      case Success(Param(t, Identifier(name))) =>
        name shouldBe "ps"
        t match {
          case ArrayType(elem, 1) =>
            elem match {
              case PairType(fst, snd) =>
                fst match {
                  case IntType() => succeed
                  case other     => fail(s"Expected fst IntType, got $other")
                }
                snd match {
                  case BoolType() => succeed
                  case other      => fail(s"Expected snd BoolType, got $other")
                }
              case other => fail(s"Expected element PairType, got $other")
            }
          case other => fail(s"Expected ArrayType(_,1), got $other")
        }
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "reject missing identifier" in {
    expectFailure(p.parseParam("int"))
  }
}

class ParamListParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "paramList parser" should "parse a single parameter" in {
    p.parseParamList("bool flag") match {
      case Success(List(Param(t, Identifier(name)))) =>
        name shouldBe "flag"
        t match {
          case BoolType() => succeed
          case other      => fail(s"Expected BoolType, got $other")
        }
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse multiple comma-separated parameters" in {
    p.parseParamList("int x, bool y, char z") match {
      case Success(List(
      Param(IntType(), Identifier("x")),
      Param(BoolType(), Identifier("y")),
      Param(CharType(), Identifier("z"))
      )) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "allow whitespace around commas" in {
    p.parseParamList("int x ,bool y,   string s") match {
      case Success(List(
      Param(IntType(), Identifier("x")),
      Param(BoolType(), Identifier("y")),
      Param(StringType(), Identifier("s"))
      )) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "reject missing comma between parameters" in {
    expectFailure(p.parseParamList("int x bool y"))
  }

  it should "reject trailing comma" in {
    expectFailure(p.parseParamList("int x, bool y,"))
  }
}

class FuncParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "func parser" should "parse a function with empty params and skip body" in {
    p.parseFunc("int f() is skip end") match {
      case Success(fn) if fn.name.ident == "f" =>
        fn.params shouldBe Nil
        fn.body match {
          case Skip() => succeed
          case other  => fail(s"Expected skip body, got: $other")
        }

      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "parse a function with parameters" in {
    p.parseFunc("bool g(int x, char y) is skip end") match {
      case Success(fn) if fn.name.ident == "g" =>
        fn.params.length shouldBe 2

        fn.params(0).name.ident shouldBe "x"
        fn.params(1).name.ident shouldBe "y"

        fn.body match {
          case Skip() => succeed
          case other  => fail(s"Expected skip body, got: $other")
        }

      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "reject if we do not write a statement inside function definition" in {
    expectFailure(p.parseFunc("bool g(int x) is end"))
  }

  it should "reject if we miss 'is' keyword in function definition" in {
    expectFailure(p.parseFunc("int f() skip end"))
  }
}
