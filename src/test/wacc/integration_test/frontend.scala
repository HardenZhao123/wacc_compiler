package wacc.integration_test.frontend

import wacc.frontend.renamer.{RenameError, Renamer}
import wacc.frontend.parser
import wacc.frontend.typeCheck.TypeChecker
import wacc.common.*
import parsley.{Failure, Success}
import scala.io.Source
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import os.*
import wacc.frontend.returnCheck.{NonReturnErr, ReturnCheck}

object CompileRunner {

  // Runs the full compilation pipeline on a single source file
  // and returns the corresponding exit code.
  def run(file: String): Int = {
    val input =
            try Source.fromFile(file).mkString
            catch {
                case _: Exception =>
                    System.err.println(s"cannot read file: $file")
                    sys.exit(ExitCode.UsageError)
            }     

    // Parse the input program.
    val r = parser.parseProgram(input)
    r match {
      case Failure(_) => ExitCode.SyntaxError  

      case Success(ast) =>
        // Perform return checking and collect non-return errors.
        val nonReturnErrs = List.newBuilder[NonReturnErr]
        ReturnCheck.checkProgram(ast, nonReturnErrs)
        val sErrors = nonReturnErrs.result()
        if (sErrors.nonEmpty) return ExitCode.SyntaxError

        // Perform renaming and collect renaming errors.
        val renameErrs = List.newBuilder[RenameError]
        val renamed = Renamer.renameProgram(ast, renameErrs).program
        val errors = renameErrs.result()
        if (errors.nonEmpty) return ExitCode.SemanticError 

        else {
          // Perform type checking on the renamed program.
          val tc = TypeChecker  
          tc.checkProgram(renamed)
          val tErrors = tc.getErrors()
          if (tErrors.nonEmpty) return ExitCode.SemanticError
          else return ExitCode.Success
        }
    }
  }
}

trait CompileIntegrationSpec extends AnyFlatSpec with Matchers {

  def testDir: os.Path        // Directory containing the test programs.
  def description: String     // Description used by ScalaTest for this test suite.
  def expectedExitCode: Int   // Expected exit code for all programs in this directory.

  // Generates one test per .wacc file in the test directory.
  def generateTests(testName: String): Unit = {
    behavior of description
    val files = os.walk(testDir).filter(f => os.isFile(f) && f.ext == "wacc")

    var num = 0 
    for (file <- files) {
      var first = true
      if (first) {
        testName + "-" + file.last + num should s"return exit code $expectedExitCode for ${file.last}" in {
          val code = CompileRunner.run(file.toString)
          code shouldBe expectedExitCode
        }
        first = false
      } else {
        it should s"return exit code $expectedExitCode for ${file.last}" in {
          val code = CompileRunner.run(file.toString)
          code shouldBe expectedExitCode
        }
      }

      num += 1
    }
  }
}

// Tests all valid WACC programs.
class ValidProgramsIT extends CompileIntegrationSpec {
  override val testDir = os.pwd / "examples" / "valid"
  override val description = "Valid WACC programs"
  override val expectedExitCode = 0

  generateTests("valid")
}

// Tests programs with syntax errors.
class SyntaxErrorProgramsIT extends CompileIntegrationSpec {
  override val testDir = os.pwd / "examples" / "invalid" / "syntaxErr"
  override val description = "WACC programs with syntax errors"
  override val expectedExitCode = 100

  generateTests("invalid-syntax")
}

// Tests programs with semantic errors.
class SemanticErrorProgramsIT extends CompileIntegrationSpec {
  override val testDir = os.pwd / "examples" / "invalid" / "semanticErr"
  override val description = "WACC programs with semantic errors"
  override val expectedExitCode = 200

  generateTests("invalid-semantic")
}
