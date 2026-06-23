package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.Success
import wacc.frontend.MacroPreprocessor
import wacc.frontend.MacroPreprocessor.MacroError
import wacc.frontend.parser

final class MacroPreprocessorTest extends AnyFlatSpec {
  private def expand(input: String): String = MacroPreprocessor.preprocess(input) match {
    case Right(output) => output
    case Left(error) => fail(s"Expected macro expansion to succeed, but got: ${error.message}")
  }

  private def error(input: String): MacroError = MacroPreprocessor.preprocess(input) match {
    case Left(result) => result
    case Right(output) => fail(s"Expected macro expansion to fail, but got: $output")
  }

  private def assertProgramParses(input: String): Unit = parser.parseProgram(input) match {
    case Success(_) => succeed: Unit
    case other => fail(s"Expected expanded program to parse, but got: $other")
  }

  "macro preprocessor" should "expand object macros before parsing a program" in {
    val expanded = expand(
      """@define MAX_SIZE 100
        |@define LIMIT MAX_SIZE + 20
        |@define TYPE int
        |begin
        |  TYPE size = LIMIT;
        |  skip
        |end""".stripMargin
    )

    expanded should not include "@define"
    assertProgramParses(expanded)
  }

  it should "apply definitions sequentially and expand chained replacements" in {
    val expanded = expand(
      """begin
        |  int early = LATER;
        |  @define BASE 4
        |  @define LATER BASE + 1
        |  int late = LATER;
        |  skip
        |end""".stripMargin
    )

    expanded should include("int early = LATER;")
    expanded should include("int late = 4 + 1;")
  }

  it should "replace complete identifiers only" in {
    val expanded = expand(
      """@define SIZE 7
        |begin
        |  int SIZE2 = SIZE;
        |  skip
        |end""".stripMargin
    )

    expanded should include("int SIZE2 = 7;")
  }

  it should "not expand inside literals or comments" in {
    val expanded = expand(
      """@define X 42
        |begin
        |  string text = "X";
        |  char marker = 'X'; # X
        |  int value = X;
        |  skip
        |end""".stripMargin
    )

    expanded should include("\"X\"")
    expanded should include("'X'")
    expanded should include("# X")
    expanded should include("int value = 42;")
  }

  it should "accept indented directives and discard their trailing comments" in {
    val expanded = expand(
      """  @define VALUE 7 # this is not part of the replacement
        |begin
        |  int value = VALUE;
        |  skip
        |end""".stripMargin
    )

    expanded should include("int value = 7;")
    expanded should not include "this is not part of the replacement"
  }

  it should "remove definition lines while preserving their line count" in {
    val source = "@define VALUE 1\nbegin skip end\n"
    val expanded = expand(source)

    expanded.split("\\n", -1).length shouldBe source.split("\\n", -1).length
    expanded should startWith("\nbegin skip end\n")
  }

  it should "allow an empty replacement" in {
    val expanded = expand(
      """@define OPTIONAL
        |begin
        |  OPTIONAL skip
        |end""".stripMargin
    )

    assertProgramParses(expanded)
  }

  it should "reject duplicate definitions" in {
    val result = error("@define VALUE 1\n@define VALUE 2")

    result.line shouldBe 2
    result.message should include("already defined")
  }

  it should "reject malformed and function-like definitions" in {
    error("@define").message should include("expected a macro name")
    error("@define 1VALUE 1").message should include("must be an identifier")
    error("@define MAX(value) value").message should include("function-like")
  }

  it should "reject direct and indirect recursive expansions" in {
    error("@define A A\nbegin int x = A; skip end").message should include("A -> A")
    error("@define A B\n@define B A\nbegin int x = A; skip end").message should include("A -> B -> A")
  }
}
