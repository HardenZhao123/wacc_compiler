package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Failure, Success}
import wacc.frontend.errorMessage.SyntaxError
import wacc.frontend.parser

class SyntaxErrorMessageTest extends AnyFlatSpec {

  private def formattedSyntaxError(src: String, path: String): String =
    parser.parseProgram(src) match {
      case Failure(err) => SyntaxError.format(path, err)
      case Success(ast) => fail(s"Expected parsing to fail, but it succeeded with: $ast")
    }

  "syntax error messages" should "explain an empty begin-end body without suggesting a missing wrapper" in {
    val err = formattedSyntaxError("begin end", "emptyBody.wacc")

    err should include("Syntax error in emptyBody.wacc")
    err should include("a `begin ... end` block must contain at least one statement")
    err should not include "all program body and function declarations must be within `begin` and `end`"
  }

  it should "explain a missing top-level begin-end wrapper" in {
    val err = formattedSyntaxError("int x = 1", "missingWrapper.wacc")

    err should include("expected begin")
    err should include("all program body and function declarations must be within `begin` and `end`")
  }

  it should "explain a while condition that is missing do" in {
    val err = formattedSyntaxError(
      """begin
        |  while false
        |    skip
        |  done
        |end""".stripMargin,
      "missingDo.wacc"
    )

    err should include("the condition of a while loop must be closed with `do`")
    err should include("|    ^^^^")
  }

  it should "explain unclosed scopes at end of input" in {
    val err = formattedSyntaxError("begin skip", "unclosed.wacc")

    err should include("unexpected end of input")
    err should include("a scope, function or the main body is unclosed")
  }

  it should "keep syntax token extraction aligned with extended language keywords" in {
    val err = formattedSyntaxError("begin end", "tokens.wacc")

    err should include("float")
    err should include("try")
    err should include("throw")
    err should include("break")
    err should include("continue")
    err should include("switch (")
  }
}
