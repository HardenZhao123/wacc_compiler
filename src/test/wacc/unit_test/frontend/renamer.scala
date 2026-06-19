package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}

import wacc.frontend.parser
import wacc.frontend.renamer.{RenameError, Renamer}
import wacc.common.ExitCode

class RenamerTest extends AnyFlatSpec {

  private val p = parser

  private def shouldExit(src: String, expected: Int): Unit =
    p.parseProgram(src) match {
      case Success(ast) =>
        val renameErrs = List.newBuilder[RenameError]
        Renamer.renameProgram(ast, renameErrs)

        val rErrors = renameErrs.result()

        val exitCode = if (rErrors.nonEmpty) 200 else 0
        withClue("\n" + rErrors.map(_.toString).mkString("\n")) {
          exitCode shouldBe expected
        }
      case Failure(err) =>
        fail(s"parse failed: $err")
    }

  "renamer" should "accept begin skip end" in {
    shouldExit("begin skip end", ExitCode.Success)
  }

  it should "reject variable redeclaration" in {
    val src =
      """
      begin
        int[] x = [];
        int x = 2
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "reject function redeclaration" in {
    val src =
      """
      begin
        pair(int, int) f() is
          pair(int, int) p = newpair(1, 1);
          return p
        end

        bool f() is
          string s = "abc";
          return s
        end

        skip
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept function overloading by parameter types" in {
    val src =
      """
      begin
        int f(int x) is
          return x
        end

        bool f(bool x) is
          return x
        end

        skip
      end
      """
    shouldExit(src, ExitCode.Success)
  }
  it should "reject using undeclared variable" in {
    val src =
      """
      begin
        int x = 1;
        int y = x + z;
        println y
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }

  it should "accept legal shadowing" in {
    val src =
      """
      begin
        int x = 1;
        begin
          int x = 2;
          println x
        end;
        println x
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept legal shadowing inside function" in {
    val src =
      """
      begin
        int f(int x) is
          begin
            int x = 10;
            return x
          end
        end
        skip
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept that variable shadows function" in {
    val src =
      """
      begin
        int f() is
          begin return 1 end
        end
        int f = call f()
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept a basic for loop with redeclaration" in {
    val src =
      """
      begin
        int x = 1;
        for (int x = 2, x < 10, x = x + 1)
          println x
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept a basic for loop without redeclaration" in {
    val src =
      """
      begin
        for (int x = 2, x < 10, x = x + 1)
          println x
        done
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "accept a basic do-while loop" in {
    val src =
      """
      begin
        int x = 1;
        do
          int y = x * 2;
          x = x + 1
        while x <= 10
      end
      """
    shouldExit(src, ExitCode.Success)
  }

  it should "reject a variable used in condition that is declared inside the do-while loop" in {
    val src =
      """
      begin
        do
          int x = 1;
          int y = x * 2;
          x = x + 1
        while x <= 10
      end
      """
    shouldExit(src, ExitCode.SemanticError)
  }
}
