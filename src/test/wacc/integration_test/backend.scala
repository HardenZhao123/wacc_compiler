package wacc.integration_test.backend

import scala.io.Source
import scala.util.Try
import wacc.common.ExitCode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.concurrent.TimeLimits

import java.io.{ByteArrayOutputStream, PrintStream}

object BackendRunner {

  private val InputHdr   = "# Input:"
  private val OutputHdr  = "# Output:"
  private val ExitHdr    = "# Exit:"
  private val ProgramHdr = "# Program:"

  final case class Spec(expectedExit: Option[Int], expectedOutput: Option[String], expectedInput: Option[String])

  private def stripOneCommentPrefix(line: String): String = {
    val t = line
    if (t.trim.startsWith("#")) {
      val afterHash = t.dropWhile(_ != '#').drop(1)
      afterHash.stripPrefix(" ")
    } else t
  }

  private def normNewlines(s: String): String =
    s.replace("\r\n", "\n")

  private def stripTrailingWhitespaceLines(s: String): String =
    normNewlines(s).linesIterator.toList.reverse.dropWhile(_.trim.isEmpty).reverse.mkString("\n")

  def outputMatches(expected: String, actual: String): Boolean = {
    val e = stripTrailingWhitespaceLines(expected)
    val a = stripTrailingWhitespaceLines(actual)

    if (e == a) return true

    val addrRegex    = "0x[0-9a-fA-F]+"
    val runtimeRegex = "(?s)fatal error:.*" // (?s) allows newline in case error prints multiple lines

    // helper
    def quote(s: String) = java.util.regex.Pattern.quote(s)

    // Build regex by replacing placeholders one by one (quote literal text in between)
    val tokens = e.split(quote("#addrs#"), -1).map { chunk =>
      // inside each chunk, also handle runtime_error
      chunk.split(quote("#runtime_error#"), -1).map(quote).mkString(runtimeRegex)
    }.mkString(addrRegex)

    val regexStr = "^" + tokens + "$"
    a.matches(regexStr)
  }

  private def isHeaderLine(t: String): Boolean =
    t.startsWith(InputHdr) || t.startsWith(OutputHdr) || t.startsWith(ExitHdr) || t.startsWith(ProgramHdr)

  private def headerPayload(line: String, hdr: String): Option[String] = {
    val t = line.trim
    if (!t.startsWith(hdr)) None
    else {
      val rest = t.drop(hdr.length).trim // allow inline content after "# Input:"
      if (rest.isEmpty) None else Some(rest)
    }
  }

  def parseSpecFromFile(waccPath: os.Path): Spec = {
    val lines = Source.fromFile(waccPath.toIO).getLines().toList

    val headerBlock = lines.takeWhile { l =>
      val t = l.trim
      t.isEmpty || t.startsWith("#")
    }

    // Find the first line whose trimmed form starts with the header (supports inline: "# Input: 1 2 3")
    def findHeaderIdx(prefix: String): Option[Int] = {
      val i = headerBlock.indexWhere(_.trim.startsWith(prefix))
      if (i < 0) None else Some(i)
    }

    def sliceHeader(prefix: String): Option[String] = {
      val idxOpt = findHeaderIdx(prefix)
      idxOpt.map { idx =>
        val firstLine = headerBlock(idx)

        // inline payload on the same line (e.g., "# Input: 1 Y 2 N")
        val inline0 = headerPayload(firstLine, prefix).toList

        // continuation lines after the header line, until next header
        val cont = headerBlock
          .slice(idx + 1, headerBlock.length)
          .takeWhile(l => l.trim.isEmpty || (l.trim.startsWith("#") && !isHeaderLine(l.trim)))
          .map(stripOneCommentPrefix)

        val all = (inline0 ++ cont)
        val trimmed = all.reverse.dropWhile(_.trim.isEmpty).reverse
        trimmed.mkString("\n")
      }.map(normNewlines).map(_.trim).filter(_.nonEmpty)
    }

    val expectedInput  = sliceHeader(InputHdr)
    val expectedOutput = sliceHeader(OutputHdr)

    val expectedExit: Option[Int] =
      sliceHeader(ExitHdr)
        .flatMap(_.linesIterator.map(_.trim).filter(_.nonEmpty).toList.headOption)
        .flatMap(s => Try(s.toInt).toOption)

    Spec(expectedExit, expectedOutput, expectedInput)
  }

  final case class CmdResult(exitCode: Int, out: String, err: String)

  // Wrap external commands with GNU timeout to avoid hung processes blocking CI runners.
  private def withTimeout(cmd: Seq[String], timeoutSec: Int, killAfterSec: Int = 1): Seq[String] =
    Seq("timeout", "-k", s"${killAfterSec}s", s"${timeoutSec}s") ++ cmd

  private def runCmd(
                      cwd: os.Path,
                      cmd: Seq[String],
                      timeoutSec: Int,
                      stdin: Option[String] = None
                    ): CmdResult = {

    val outBuf = new StringBuilder
    val errBuf = new StringBuilder
    val wrapped = withTimeout(cmd, timeoutSec)

    val res = os.proc(wrapped).call(
      cwd = cwd,
      stdin = stdin match {
        case Some(s) =>
          val text = if (s.endsWith("\n")) s else s + "\n"
          text
        case None =>
          os.Pipe
      },
      stdout = os.ProcessOutput.Readlines(s => outBuf.append(s).append("\n")),
      stderr = os.ProcessOutput.Readlines(s => errBuf.append(s).append("\n")),
      check = false
    )

    CmdResult(res.exitCode, outBuf.result(), errBuf.result())
  }

  def runCompile(file: String, isAArch64: Boolean, peephole: Boolean): CmdResult = {
    val repoRoot = os.pwd

    val prevOut = System.out
    val prevErr = System.err
    val prevUserDir = System.getProperty("user.dir")

    val outBytes = new ByteArrayOutputStream()
    val errBytes = new ByteArrayOutputStream()
    val outPs = new PrintStream(outBytes)
    val errPs = new PrintStream(errBytes)

    try {
      System.setOut(outPs)
      System.setErr(errPs)

      System.setProperty("user.dir", repoRoot.toIO.getAbsolutePath)

      val absFile = os.Path(file, base = repoRoot).toIO.getAbsolutePath
      val code = wacc.Main.compileFile(absFile, isAArch64, peephole)

      CmdResult(code, outBytes.toString("UTF-8"), errBytes.toString("UTF-8"))
    } finally {
      System.setOut(prevOut)
      System.setErr(prevErr)
      if (prevUserDir != null) System.setProperty("user.dir", prevUserDir)
      outPs.close()
      errPs.close()
    }
  }

  def assembleAndRunAArch64(asmFile: os.Path, stdin: Option[String]): CmdResult = {
    val repoRoot = os.pwd
    val exe = repoRoot / s"${asmFile.baseName}.out"
    if (os.exists(exe)) os.remove(exe)

    val gcc = Seq(
      "aarch64-linux-gnu-gcc",
      "-o", exe.toString,
      "-z", "noexecstack",
      "-march=armv8-a",
      asmFile.toString
    )
    val gccRes = runCmd(repoRoot, gcc, timeoutSec = 60)
    if (gccRes.exitCode != 0) return gccRes

    runCmd(
      repoRoot,
      Seq("qemu-aarch64", "-L", "/usr/aarch64-linux-gnu/", exe.toString),
      timeoutSec = 20,
      stdin = stdin
    )
  }

  def assembleAndRunARM32(asmFile: os.Path, stdin: Option[String]): CmdResult = {
    val repoRoot = os.pwd
    val exe = repoRoot / s"${asmFile.baseName}.out"
    if (os.exists(exe)) os.remove(exe)

    val gcc = Seq(
      "arm-linux-gnueabi-gcc",
      "-o", exe.toString,
      "-z", "noexecstack",
      "-march=armv6",
      asmFile.toString
    )
    val gccRes = runCmd(repoRoot, gcc, timeoutSec = 60)
    if (gccRes.exitCode != 0) return gccRes

    runCmd(
      repoRoot,
      Seq("qemu-arm", "-L", "/usr/arm-linux-gnueabi/", exe.toString),
      timeoutSec = 20,
      stdin = stdin
    )
  }

  def runValidBackend(file: os.Path, isAArch64: Boolean, peephole: Boolean): (Int, CmdResult, Spec) = {
    val repoRoot = os.pwd

    val comp = runCompile(file.toString, isAArch64, peephole)
    if (comp.exitCode != ExitCode.Success) return (comp.exitCode, comp, Spec(None, None, None))

    val asmOut = repoRoot / s"${file.baseName}.s"
    if (!os.exists(asmOut)) {
      return (comp.exitCode, CmdResult(1, "", s"expected assembly not found: $asmOut"), Spec(None, None, None))
    }

    val spec = parseSpecFromFile(file)

    val stdin = spec.expectedInput.map { s =>
      val t = normNewlines(s).trim
      if (t.isEmpty) "" else t + "\n"
    }

    val runRes = if isAArch64 then assembleAndRunAArch64(asmOut, stdin)
                 else assembleAndRunARM32(asmOut, stdin)
    (comp.exitCode, runRes, spec)
  }
}

trait BackendIntegrationSpec
  extends AnyFlatSpec with Matchers with TimeLimits {

  import BackendRunner.*

  def isAArch64: Boolean = true
  def peephole: Boolean = true
  def testDir: os.Path
  def description: String
  def expectedExitCode: Int
  def allowedPackages: Option[Set[String]] = None

  def generateTests(testName: String): Unit = {
    behavior of description
    val repoRoot = os.pwd

    val subDirs =
      os.list(testDir).filter(os.isDir).filter { d =>
        allowedPackages match {
          case Some(pkgs) => pkgs.contains(d.last)
          case None       => true
        }
      }.sorted

    for (pkgDir <- subDirs) {
      val packageName = pkgDir.last
      behavior of s"$description - package: $packageName"

      val files = os.walk(pkgDir).filter(f => os.isFile(f) && f.ext == "wacc").sorted

      var num = 0
      for (file <- files) {
        val caseName = s"$testName-${packageName}-${file.last}-$num"

        caseName should s"return exit code $expectedExitCode for ${file.last}" in {
          failAfter(Span(60, Seconds)) {

            val asmOut = repoRoot / s"${file.baseName}.s"

            if (expectedExitCode != 0) {
              // Avoid pollution from valid tests: invalid programs should not produce assembly.
              if (os.exists(asmOut)) os.remove(asmOut)

              val comp = runCompile(file.toString, isAArch64, peephole)

              withClue(
                s"""
                   |=== compile comparison ===
                   |file: ${file.relativeTo(repoRoot)}
                   |expected compile exit: $expectedExitCode
                   |actual compile exit:   ${comp.exitCode}
                   |--- compiler stdout ---
                   |${comp.out}
                   |--- compiler stderr ---
                   |${comp.err}
                   |""".stripMargin
              ) {
                comp.exitCode shouldBe expectedExitCode
              }

              withClue(s"invalid program should not emit assembly, but found: $asmOut\n") {
                os.exists(asmOut) shouldBe false
              }

            } else {
              // valid: compile -> asm -> gcc -> qemu; compare runtime behaviour
              val (compileCode, runRes, spec) = runValidBackend(file, isAArch64, peephole)
              compileCode shouldBe expectedExitCode

              if (runRes.exitCode == 124 || runRes.exitCode == 137) {
                fail(
                  s"""
                     |=== TIMEOUT ===
                     |file: ${file.relativeTo(repoRoot)}
                     |exit code: ${runRes.exitCode}
                     |--- stdout ---
                     |${runRes.out}
                     |--- stderr ---
                     |${runRes.err}
                     |""".stripMargin
                )
              }

              spec.expectedExit.foreach { e =>
                withClue(
                  s"""
                     |=== runtime exit comparison ===
                     |file: ${file.relativeTo(repoRoot)}
                     |expected exit: $e
                     |actual exit:   ${runRes.exitCode}
                     |--- stderr ---
                     |${runRes.err}
                     |""".stripMargin
                ) {
                  runRes.exitCode shouldBe e
                }
              }

              spec.expectedOutput.foreach { eo =>
                withClue(
                  s"""
                     |=== runtime stdout comparison ===
                     |file: ${file.relativeTo(repoRoot)}
                     |--- expected stdout ---
                     |$eo
                     |--- actual stdout ---
                     |${runRes.out}
                     |--- stderr ---
                     |${runRes.err}
                     |""".stripMargin
                ) {
                  outputMatches(eo, runRes.out) shouldBe true
                }
              }
            }
          }
        }

        num += 1
      }
    }
  }
}

final class AArch64BackendValidProgramsIT extends BackendIntegrationSpec {
  override val testDir = os.pwd / "examples" / "valid"
  override val description = "Backend: Valid WACC programs (aarch64)"
  override val expectedExitCode = 0

  override val allowedPackages: Option[Set[String]]
      = Some(Set("basic",
                  "sequence",
                  "IO",
                  "variables",
                  "expressions",
                  "array",
                  "if",
                  "while",
                  "for",
                  "do-while",
                  "scope",
                  "function",
                  "pairs",
                  "runtimeErr",
                  "exception"))
  generateTests("backend-valid")
}

final class ARM32BackendValidProgramsIT extends BackendIntegrationSpec {
  override val isAArch64: Boolean = false
  override val testDir = os.pwd / "examples" / "valid"
  override val description = "Backend: Valid WACC programs (arm32)"
  override val expectedExitCode = 0

  override val allowedPackages: Option[Set[String]] =
   Some(Set("basic",
    "sequence",
    "IO",
    "variables",
    "expressions",
    "array",
    "if",
    "while",
    "for",
    "do-while",
    "scope",
    "function",
    "pairs",
    "runtimeErr",
    "exception"))
  generateTests("backend-valid")
}

final class BackendSyntaxErrorProgramsIT extends BackendIntegrationSpec {
  override val testDir = os.pwd / "examples" / "invalid" / "syntaxErr"
  override val description = "Backend: WACC programs with syntax errors (aarch64)"
  override val expectedExitCode = 100
  generateTests("backend-invalid-syntax")
}

final class BackendSemanticErrorProgramsIT extends BackendIntegrationSpec {
  override val testDir = os.pwd / "examples" / "invalid" / "semanticErr"
  override val description = "Backend: WACC programs with semantic errors (aarch64)"
  override val expectedExitCode = 200
  generateTests("backend-invalid-semantic")
}
