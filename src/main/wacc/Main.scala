package wacc

import parsley.{Failure, Success}

import java.nio.file.Paths
import java.io.FileOutputStream
import scala.io.Source
import wacc.common.*
import wacc.frontend.{MacroPreprocessor, parser}
import wacc.frontend.renamer.{RenameError, Renamer}
import wacc.frontend.returnCheck.{NonReturnErr, ReturnCheck}
import wacc.frontend.typeCheck.TypeChecker
import wacc.frontend.errorMessage.*
import wacc.backend.midir.*
import wacc.backend.AArch64.codeGen.*
import wacc.backend.BackendCommon.BackendConstant.*
import wacc.backend.Arm32.codeGen.*
import wacc.backend.X86.codeGen.{X86Formatter, X86Generator}

object Main {
  enum TargetArch {
    case AArch64, Arm32, X86
  }

  import TargetArch.*

  def compileFile(path: String, isAArch64: Boolean, peephole: Boolean): Int =
    compileFile(path, if isAArch64 then AArch64 else Arm32, peephole)

  def compileFile(path: String, targetArch: TargetArch, peephole: Boolean): Int = {
    println("========== Frontend ==========")

    var semanticErr = false

    val source =
      try Source.fromFile(path).mkString
      catch {
        case _: Exception =>
          System.err.println(s"cannot read file: $path")
          println(s"Finished with exit code: ${ExitCode.UsageError}")
          return ExitCode.UsageError
      }

    println("Starting macro preprocessing...")
    val input = MacroPreprocessor.preprocess(source) match {
      case Left(error) =>
        System.err.println(
          s"""Errors detected during compilation!
             |${"-" * 30}
             |""".stripMargin
        )
        System.err.println(s"${error.format(path, source)}\n")
        println(
          s"""${"-" * 30}
             |Finished with exit code: ${ExitCode.SyntaxError}""".stripMargin
        )
        return ExitCode.SyntaxError
      case Right(expanded) =>
        println("Macro preprocessing finished!")
        expanded
    }

    // 1. Parse
    println("Starting parsing... finished!")
    parser.parseProgram(input) match {
      case Failure(parseErrs) =>
        System.err.println(
          s"""Errors detected during compilation!
             |${"-" * 30}
             |""".stripMargin
        )
        System.err.println(s"${SyntaxError.format(path, parseErrs)}\n")
        println(
          s"""${"-" * 30}
             |Finished with exit code: ${ExitCode.SyntaxError}""".stripMargin
        )
        return ExitCode.SyntaxError

      case Success(ast) =>
        // 2. Additional syntactic restriction: function body must be returning
        val nonReturnErrs = List.newBuilder[NonReturnErr]
        ReturnCheck.checkProgram(ast, nonReturnErrs)

        val sErrors = nonReturnErrs.result()
        if (sErrors.nonEmpty) {
          System.err.println(
            s"""Errors detected during compilation!
               |${"-" * 30}
               |""".stripMargin
          )
          System.err.println(s"${NonReturnErrorFormat.format(path, input, sErrors)}\n")
          println(
            s"""${"-" * 30}
               |Finished with exit code: ${ExitCode.SyntaxError}""".stripMargin
          )
          return ExitCode.SyntaxError
        }

        // 3. Rename (semantic)
        println("Starting renaming... finished!")
        val renameErrs = List.newBuilder[RenameError]
        val renamed = Renamer.renameProgram(ast, renameErrs).program
        val rErrors = renameErrs.result()
        if (rErrors.nonEmpty) semanticErr = true

        // 4. Typecheck (semantic)
        println("Starting type checking... finished!")
        val tc = TypeChecker
        val typedAst = tc.checkProgram(renamed)
        val tErrors = tc.getErrors()
        if (tErrors.nonEmpty) semanticErr = true

        // 5. Output Error Message and Exit
        if (semanticErr) {
          System.err.println(
            s"""Errors detected during compilation!
               |${"-" * 30}
               |""".stripMargin
          )
          if (rErrors.nonEmpty) {
            System.err.println(s"${RenamerErrorFormat.format(path, input, rErrors)}\n")
          }
          if (tErrors.nonEmpty) {
            System.err.println(s"${SemanticError.format(path, input, tErrors)}\n")
          }
          println(
            s"""${"-" * 30}
               |Finished with exit code: ${ExitCode.SemanticError}""".stripMargin
          )
          return ExitCode.SemanticError
        }

        println("========== Backend ==========")

        println("Starting gir generation... finished!")

        val wordSize = targetArch match {
          case AArch64 => A64_WORD_SIZE
          case Arm32   => A32_WORD_SIZE
          case X86     => wacc.backend.X86.X86Constants.SLOT_SIZE
        }
        val tac = LowerToTAC.fromTypedProgram(typedAst, wordSize)

        println(s"Starting ${archName(targetArch)} translation... finished!")

        val fileName = Paths.get(path).getFileName.toString.stripSuffix(".wacc")
        val filePath = os.pwd / s"${fileName}.s"
        val outputStream = new FileOutputStream(filePath.toIO)
        try {
          targetArch match {
            case AArch64 =>
              val a64Instrs = A64Generator.genA64Program(tac)
              val finalInstrs =
                if (peephole) then
                  println("Start peephole optimisation ... finished!")
                  A64PeepholeOptimiser.optimise(a64Instrs)
                else a64Instrs
              A64Formatter.writeAssembly(finalInstrs, outputStream)

            case Arm32 =>
              val a32Instrs = A32Generator.genA32Program(tac)
              val finalInstrs =
                if (peephole) then
                  println("Start peephole optimisation ... finished!")
                  A32PeepholeOptimiser.optimise(a32Instrs)
                else a32Instrs
              A32Formatter.writeAssembly(finalInstrs, outputStream)

            case X86 =>
              val x86Instrs = X86Generator.genX86Program(tac)
              X86Formatter.writeAssembly(x86Instrs, outputStream)
          }
        } finally {
          outputStream.close()
        }

        println(s"Starting ${archName(targetArch)} printing... finished!")

        println(
          s"""${"-" * 30}
             |Finished with exit code: ${ExitCode.Success}""".stripMargin
        )

        return ExitCode.Success
    }
  }

  // Use: ./compile <filename>.wacc [--architecture { aarch64 | arm32 | x86-64 }] [--peephole-optim] [--no-peephole]
  // [ ... ] means optional
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("========== Frontend ==========")
      System.err.println(
        "usage: compile <file.wacc> [--architecture aarch64|arm32|x86-64] [--peephole-optim]"
      )
      println(s"Finished with exit code: ${ExitCode.UsageError}")
      sys.exit(ExitCode.UsageError)
    }

    val file = args(0)

    // defaults
    var targetArch = Arm32      // default: arm32
    var peephole = true         // default: enabled

    var i = 1
    while (i < args.length) {
      args(i) match {

        case "--architecture" =>
          if (i + 1 >= args.length) {
            System.err.println("Missing architecture value")
            sys.exit(ExitCode.UsageError)
          }

          args(i + 1) match {
            case "aarch64" => targetArch = AArch64
            case "arm32"   => targetArch = Arm32
            case "x86" | "x86-64" | "x86_64" => targetArch = X86
            case _ =>
              System.err.println("Invalid architecture (use aarch64, arm32, or x86-64)")
              sys.exit(ExitCode.UsageError)
          }

          i += 1

        case "--peephole-optim" =>
          peephole = true

        case "--no-peephole" =>
          peephole = false

        case other =>
          System.err.println(s"Unknown option: $other")
          sys.exit(ExitCode.UsageError)
      }

      i += 1
    }

    val code = compileFile(file, targetArch, peephole)
    sys.exit(code)
  }

  private def archName(targetArch: TargetArch): String = targetArch match {
    case TargetArch.AArch64 => "aarch64"
    case TargetArch.Arm32   => "arm32"
    case TargetArch.X86     => "x86-64"
  }
}
