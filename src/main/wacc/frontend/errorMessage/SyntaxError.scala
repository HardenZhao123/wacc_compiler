package wacc.frontend.errorMessage

import java.nio.file.Paths
import parsley.Parsley
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.LexToken
import wacc.frontend.lexer.{boolean, char, identifier, implicits, integer, string}


// Represents a single item involved in a syntax error report.
// These items describe either the token that was encountered,
// the tokens that were expected, or the end of input.
enum SyntaxErrorItem {
  case SyntaxRaw(item: String)
  case SyntaxNamed(item: String)
  case SyntaxEndOfInput
}

// Represents the structured information for a syntax error message
// including the offending line of code and contextual explanation
enum SyntaxErrorLines {
  case VanillaError(
    unexpected: Option[SyntaxErrorItem],
    expected: Set[SyntaxErrorItem],
    reasons: Set[String],
    line: Seq[String]
  )
  case SpecialisedError(
    msgs: Set[String],
    line: Seq[String]
  )
}

object SyntaxErrorLines {
  // Convert a SyntaxErrorItem into a human-readable string
  private def showItem(i: SyntaxErrorItem): String = i match {
    case SyntaxErrorItem.SyntaxRaw(s)     => s
    case SyntaxErrorItem.SyntaxNamed(s)   => s
    case SyntaxErrorItem.SyntaxEndOfInput => "end of input"
  }

  // Render the descriptive part of the syntax error (unexpected/expected/reasons)
  def renderDetails(lines: SyntaxErrorLines): List[String] = lines match {
    case SyntaxErrorLines.VanillaError(unexpected, expected, reasons, _) =>
      val u = unexpected.map(i => s"unexpected ${showItem(i)}").toList
      val e =
        if (expected.nonEmpty)
          List("expected " + expected.toList.map(showItem).sorted.mkString(", "))
        else Nil
      val r = reasons.toList.sorted
      u ++ e ++ r
    case SyntaxErrorLines.SpecialisedError(msgs, _) =>
      msgs.toList.sorted
  }

  // Render the source code snippet showing the error location
  def renderSnippet(lines: SyntaxErrorLines): List[String] = lines match {
    case SyntaxErrorLines.VanillaError(_, _, _, line) => line.toList
    case SyntaxErrorLines.SpecialisedError(_, line)   => line.toList
  }

  // Render the full error message (details + code snippet)
  def render(lines: SyntaxErrorLines): List[String] =
    renderDetails(lines) ++ renderSnippet(lines)
}

// Custom ErrorBuilder used by Parsley to construct syntax error messages
abstract class SyntaxErrorBuilder extends ErrorBuilder[String] {
  type Position = (Int, Int)

  // Convert raw line/column numbers into the Position type
  override def pos(line: Int, col: Int): Position = (line, col)

  type Source = String

  // Provide the source name, defaulting to "unknown" if absent
  override def source(sourceName: Option[String]): Source =
    sourceName.getOrElse("unknown")

  type Item = SyntaxErrorItem
  type Raw = SyntaxErrorItem.SyntaxRaw
  type Named = SyntaxErrorItem.SyntaxNamed
  type EndOfInput = SyntaxErrorItem.SyntaxEndOfInput.type

  // Construct a raw token error item
  override def raw(item: String): Raw = SyntaxErrorItem.SyntaxRaw(item)

  // Construct a named token error item
  override def named(item: String): Named = SyntaxErrorItem.SyntaxNamed(item)

  // Constant representing end-of-input
  override val endOfInput: EndOfInput = SyntaxErrorItem.SyntaxEndOfInput

  type ExpectedItems = Set[Item]

  // Combine multiple expected token alternatives
  override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

  // Error message types
  type Message = String
  type Messages = Set[Message]

  // Combine multiple messages
  override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

  // Construct a reason message
  override def reason(reason: String): Message = reason

  // Construct a general message
  override def message(msg: String): Message = msg

  // Optional unexpected token
  type UnexpectedLine = Option[Item]

  override def unexpected(item: Option[Item]): UnexpectedLine = item

  type ExpectedLine = ExpectedItems
  override def expected(alts: ExpectedItems): ExpectedLine = alts

  // Information about the source line(s) containing the error
  type LineInfo = Seq[String]

  // Build the source code snippet showing the error location
  override def lineInfo(
                         line: String,
                         linesBefore: Seq[String],
                         linesAfter: Seq[String],
                         lineNum: Int,
                         errorPointsAt: Int,
                         errorWidth: Int
                       ): Seq[String] = {
    val pad = " " * math.max(0, errorPointsAt)
    val carets = "^" * math.max(1, errorWidth)
    Seq("|", s"|$line", s"|$pad$carets", "|")
  }

  // Number of context lines shown before the error
  override val numLinesBefore: Int = 0
  // Number of context lines shown after the error
  override val numLinesAfter: Int  = 0


  type ErrorInfoLines = SyntaxErrorLines

  // Build a standard syntax error
  override def vanillaError(
                             unexpected: UnexpectedLine,
                             expected: ExpectedLine,
                             reasons: Messages,
                             line: LineInfo
                           ): ErrorInfoLines =
    SyntaxErrorLines.VanillaError(unexpected, expected, reasons, line)

  // Build a specialised syntax error
  override def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines =
    SyntaxErrorLines.SpecialisedError(msgs, line)

  // Construct the final formatted error string
  override def build(pos: Position, source: Source, lines: ErrorInfoLines): String = {
    val (row, col) = pos
    val header = s"Syntax error in $source ($row, $col):"
    (header :: SyntaxErrorLines.render(lines)).mkString("\n")
  }
}

object SyntaxErrorBuilder {
  val instance: ErrorBuilder[String] =
    new SyntaxErrorBuilder with LexToken {
      override def tokens: Seq[Parsley[String]] = {
        // These lists control what Parsley can report as "expected/unexpected" tokens.
        val keywords = Seq(
          "begin","end","skip",
          "if","then","else","fi",
          "while","do","done",
          "read","free","return","exit",
          "print","println",
          "call","newpair","fst","snd",
          "pair","int","bool","char","string",
          "len","ord","chr",
          "true","false","null",
          "is"
        )

        val symbols = Seq(
          "(",")","[","]",",",";","=",
          "!", "+","-","*","/","%",
          "&&","||",
          "==","!=","<","<=",">",">="
        )

        val core = Seq(
          integer.map(n => s"integer $n"),
          identifier.map(v => s"identifier $v"),
          boolean.map(_.toString),
          char.map(c => s"'$c'"),
          string.map(s => "\"" + s + "\"")
        )

        val kw = keywords.map(k => implicits.implicitSymbol(k).map(_ => k))
        val sym = symbols.map(s => implicits.implicitSymbol(s).map(_ => s))

        core ++ kw ++ sym
      }
    }
}

object SyntaxError {
  given ErrorBuilder[String] = SyntaxErrorBuilder.instance

  private val headerRe = "^Syntax error in (.+?) \\((\\d+), (\\d+)\\):$".r

  def attachSource(err: String, sourceName: String): String = {
    val ls = err.linesIterator.toList
    ls match {
      case Nil => err
      case head :: tail =>
        head match {
          case headerRe(_, row, col) =>
            (s"Syntax error in $sourceName ($row, $col):" :: tail).mkString("\n")
          case _ =>
            s"Syntax error in $sourceName:\n$err"
        }
    }
  }

  private val ProgramBeginEndHint =
    "all program body and function declarations must be within `begin` and `end`"

  private val UnclosedHint =
    "a scope, function or the main body is unclosed"

  private val WhileMissingDoHint =
    "the condition of a while loop must be closed with `do`"

  private val UnclosedWhileHint =
    "unclosed while loop"

  private val lineColPatterns: List[scala.util.matching.Regex] = List(
    raw"(?i)\bline\s+(\d+)\s*,\s*column\s+(\d+)\b".r,
    raw"\((\d+)\s*,\s*(\d+)\)".r,
    raw"\b(\d+)\s*:\s*(\d+)\b".r
  )

  private def extractRowCol(raw: String): Option[(Int, Int)] =
    lineColPatterns.view.flatMap { r =>
      r.findFirstMatchIn(raw).map(m => (m.group(1).toInt, m.group(2).toInt))
    }.headOption

  private def extractDetails(raw: String): List[String] = {
    val ls = raw.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    ls.filterNot(s => s.startsWith("Syntax error in ") || s.startsWith("|"))
      .map { s =>
        val lower = s.toLowerCase
        if (lower.startsWith("reason:")) s.drop(7).trim else s
      }
  }

  private def allLines(input: String): Array[String] =
    input.linesIterator.toArray

  private def lineAt(lines: Array[String], row1: Int): String = {
    if (row1 >= 1 && row1 <= lines.length) lines(row1 - 1) else ""
  }

  private def tokenAt(line: String, col1: Int): String = {
    // Extract the identifier-like token around the caret column; used for heuristic hints (e.g. misspelled keywords).
    def isTokenChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'
    val i0 = math.max(0, col1 - 1)
    if (line.isEmpty || i0 >= line.length) ""
    else {
      var start = i0
      while (start > 0 && isTokenChar(line.charAt(start - 1))) start -= 1
      var end = i0
      while (end < line.length && isTokenChar(line.charAt(end))) end += 1
      line.substring(start, end)
    }
  }

  private def caretLen(line: String, col1: Int): Int = {
    // Choose a caret span that roughly covers the identifier-like token at the error position.
    val i0 = math.max(0, col1 - 1)
    if (line.isEmpty) 1
    else if (i0 >= line.length) 1
    else {
      def isTokenChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'
      val ch0 = line.charAt(i0)
      if (!isTokenChar(ch0)) 1
      else {
        var j = i0
        while (j < line.length && isTokenChar(line.charAt(j))) j += 1
        math.max(1, j - i0)
      }
    }
  }

  private val noisyOps: List[String] =
    List("!=", "%", "&&", "*", "+", "-", "/", "<", "<=", "==", ">", ">=", "||")

  private def collapseNoisyExpected(details: List[String]): List[String] = {
    details.map { s =>
      val lower = s.toLowerCase
      if (lower.startsWith("expected ") && lower.contains("end") && noisyOps.exists(op => s.contains(op))) "expected end"
      else s
    }
  }

  private def injectKeywordHints(details: List[String], lines: Array[String], row: Int, col: Int): List[String] = {
    // `details` is a list of human-readable lines like "unexpected ..." and "expected ..."; hints are injected after the last "expected" line.
    val base = collapseNoisyExpected(details)
    val lower = base.map(_.toLowerCase)

    val expectedIdx = lower.lastIndexWhere(_.startsWith("expected"))
    val expectedLineLower =
      if (expectedIdx >= 0) lower(expectedIdx) else ""

    val hasBeginHint = base.contains(ProgramBeginEndHint)

    def insertAfterExpectedAt(ds: List[String], idx: Int, h: String): List[String] = {
      // Insert the hint immediately after the last "expected ..." line (stable placement across different error shapes).
      if (idx >= 0) ds.take(idx + 1) ++ List(h) ++ ds.drop(idx + 1)
      else ds :+ h
    }

    // If the parser expects `begin` at the top level, add a concrete explanation for the common missing-wrapper mistake.
    val d1 =
      if (!hasBeginHint && expectedLineLower.contains("begin"))
        insertAfterExpectedAt(base, expectedIdx, ProgramBeginEndHint)
      else base

    val lower1 = d1.map(_.toLowerCase)
    val expectedIdx1 = lower1.lastIndexWhere(_.startsWith("expected"))
    val expectedLower1 = if (expectedIdx1 >= 0) lower1(expectedIdx1) else ""

    val hasDoHint1 = d1.contains(WhileMissingDoHint)
    // Heuristic: missing `do` in `while <cond> do` often makes a statement keyword appear as "unexpected" while `do` is still expected.
    val expectsDo1 = expectedLower1.split("[^a-z]+").contains("do")
    val unexpectedStmt1 =
      lower1.exists(s =>
        s.startsWith("unexpected") && {
          val ws = s.split("[^a-z]+")
          ws.contains("skip") ||
            ws.contains("read") ||
            ws.contains("free") ||
            ws.contains("return") ||
            ws.contains("exit") ||
            ws.contains("print") ||
            ws.contains("println") ||
            ws.contains("call") ||
            ws.contains("if") ||
            ws.contains("while") ||
            ws.contains("begin")
        }
      )

    val d2 =
      if (!hasDoHint1 && expectsDo1 && unexpectedStmt1)
        // Insert the hint right after the "expected ... do ..." line.
        insertAfterExpectedAt(d1, expectedIdx1, WhileMissingDoHint)
      else d1

    val lower2 = d2.map(_.toLowerCase)
    val expectedIdx2 = lower2.lastIndexWhere(_.startsWith("expected"))
    val expectedLower2 = if (expectedIdx2 >= 0) lower2(expectedIdx2) else ""

    val hasUnexpectedEnd2 =
      lower2.exists(s => s.startsWith("unexpected") && s.contains(" end"))

    val hasUnexpectedEoi2 =
      lower2.exists(s =>
        s.startsWith("unexpected end of input") || s.startsWith("unexpected end of file")
      )

    val unexpectedIdentifier2 =
      lower2.exists(s => s.startsWith("unexpected identifier"))

    val tokLower = tokenAt(lineAt(lines, row), col).toLowerCase
    // Special-case: if `done` is expected but the unexpected token is an identifier starting with "do" (e.g. "dono"), treat it as a likely misspelling and hint an unclosed while.
    val looksLikeMisspelledDone =
      tokLower.startsWith("do") && tokLower != "do" && tokLower != "done"

    val hasWhileHint2 = d2.contains(UnclosedWhileHint)

    val d3 =
      if (
        !hasWhileHint2 &&
          expectedLower2.contains("done") &&
          (
            hasUnexpectedEnd2 ||
              hasUnexpectedEoi2 ||
              (unexpectedIdentifier2 && looksLikeMisspelledDone)
            )
      )
        insertAfterExpectedAt(d2, expectedIdx2, UnclosedWhileHint)
      else d2

    val lower3 = d3.map(_.toLowerCase)
    val expectedIdx3 = lower3.lastIndexWhere(_.startsWith("expected"))
    val expectedLower3 = if (expectedIdx3 >= 0) lower3(expectedIdx3) else ""

    val hasUnclosedAny3 = d3.contains(UnclosedHint) || d3.contains(UnclosedWhileHint)
    val hasUnexpectedEoi3 =
      lower3.exists(s =>
        s.startsWith("unexpected end of input") || s.startsWith("unexpected end of file")
      )

    val expectsACloser3 =
      expectedLower3.contains("end") || expectedLower3.contains("fi") || expectedLower3.contains("done")

    // Generic fallback: only when EOF is reached and the grammar still expects a closing keyword (end/fi/done).
    if (!hasUnclosedAny3 && hasUnexpectedEoi3 && expectsACloser3)
      insertAfterExpectedAt(d3, expectedIdx3, UnclosedHint)
    else d3
  }

  def format(path: String, input: String, parsleyRaw: String): String = {
    val file = Paths.get(path).getFileName.toString
    val raw = parsleyRaw.trim
    val lines = allLines(input)

    val (row, col) = extractRowCol(raw).getOrElse((1, 1))
    val details0 = extractDetails(raw)
    val detailsLines = injectKeywordHints(details0, lines, row, col)

    val srcLine = lineAt(lines, row)
    val beforeOpt =
      if (row > 1 && row - 2 < lines.length) Some(lines(row - 2)) else None
    val afterOpt =
      if (row >= 1 && row < lines.length) Some(lines(row)) else None

    val pad = " " * math.max(0, col - 1)
    val carets = "^" * caretLen(srcLine, col)

    val snippetLines =
      beforeOpt.toList.map(l => s"|$l") ++
        List(s"|$srcLine", s"|$pad$carets") ++
        afterOpt.toList.map(l => s"|$l")

    val header = s"Syntax error in $file ($row, $col):"
    val details = detailsLines.mkString("\n")
    val snippet = snippetLines.mkString("\n")

    if (details.nonEmpty) s"$header\n$details\n$snippet"
    else s"$header\n$raw\n$snippet"
  }
}
