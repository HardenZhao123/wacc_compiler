package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Failure, Success}
import wacc.frontend.parser
import wacc.frontend.ast.Expr.*
import wacc.frontend.ast.Stmt.Skip
import wacc.frontend.ast.{Program}
import wacc.frontend.returnCheck.{NonReturnErr, ReturnCheck}

class ProgramParserTest extends AnyFlatSpec with ParserTestHelpers {
  private val p = parser

  "program parser" should "parse minimal program" in {
    val r = p.parseProgram("begin skip end")
    r match {
      case Success(Program(Nil, Skip())) => succeed
      case Success(other)                => fail(s"Unexpected parse result: $other")
      case Failure(err)                  => fail(s"Parsing failed: $err")
    }
  }

  it should "parse program with functions then a body" in {
    val src =
      """
        begin
          int f() is return 1 end
          bool g(int x, char y) is return true end
          skip
        end
      """
    val r = p.parseProgram(src)
    r match {
      case Success(Program(funcs, Skip())) =>
        funcs.length shouldBe 2

        funcs(0).name match {
          case Identifier("f") => succeed
          case other           => fail(s"Expected function name f, got $other")
        }

        funcs(1).name match {
          case Identifier("g") => succeed
          case other           => fail(s"Expected function name g, got $other")
        }

      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err)   => fail(s"Parsing failed: $err")
    }
  }

  it should "reject if a program has function that is not terminated by a return or exit statement" in {
    val src = 
      """
        begin
          int f() is skip end
          bool g(int x, char y) is return true end
          skip
        end
      """
    val r = p.parseProgram(src)
    r match {
      case Success(ast @ Program(_, Skip())) => {
        val nonReturnErrs = List.newBuilder[NonReturnErr]
        ReturnCheck.checkProgram(ast, nonReturnErrs)
        val sErrors = nonReturnErrs.result()
        if sErrors.nonEmpty then succeed
        else fail("Expected error: function should terminate with exit or return")
      }
      case Success(other)                        => fail(s"Unexpected parse result: $other")
      case Failure(err)                          => fail(s"Parsing failed: $err")
    }
  }

  it should "accept a returning switch case that falls through to a later return" in {
    val src =
      """
        begin
          int f(int x) is
            switch (x)
              case 1: skip
              case 2: return 2
              default: return 0
            end
          end
          skip
        end
      """

    p.parseProgram(src) match {
      case Success(ast) =>
        val nonReturnErrs = List.newBuilder[NonReturnErr]
        ReturnCheck.checkProgram(ast, nonReturnErrs)
        nonReturnErrs.result() shouldBe empty
      case Failure(err) => fail(s"Parsing failed: $err")
    }
  }

  it should "reject a switch case that breaks before the function returns" in {
    val src =
      """
        begin
          int f(int x) is
            switch (x)
              case 1: break
              case 2: return 2
              default: return 0
            end
          end
          skip
        end
      """

    p.parseProgram(src) match {
      case Success(ast) =>
        val nonReturnErrs = List.newBuilder[NonReturnErr]
        ReturnCheck.checkProgram(ast, nonReturnErrs)
        nonReturnErrs.result() should not be empty
      case Failure(err) => fail(s"Parsing failed: $err")
    }
  }
}
