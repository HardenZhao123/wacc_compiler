package wacc.frontend.errorMessage

import java.nio.file.Paths
import parsley.Parsley
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.LexToken
import wacc.frontend.lexer
import wacc.frontend.lexer.{boolean, char, float, identifier, implicits, integer, string}

enum SyntaxErrorItem {
  case SyntaxRaw(item: String)
  case SyntaxNamed(item: String)
  case SyntaxEndOfInput
}

final case class SyntaxLineInfo(
                                 line: String,
                                 linesBefore: Seq[String],
                                 linesAfter: Seq[String],
                                 lineNum: Int,
                                 errorPointsAt: Int,
                                 errorWidth: Int
                               ) {
  def snippetLines: List[String] =
    Snippet
      .renderContextSnippet(line, linesBefore, linesAfter, errorPointsAt, renderedErrorWidth)
      .toList

  private def renderedErrorWidth: Int = {
    val tokenWidth = tokenAt(errorPointsAt + 1).length
    if (tokenWidth > 0) tokenWidth else errorWidth
  }

  def tokenAt(col1: Int): String = {
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
}

enum SyntaxErrorLines {
  case VanillaError(
    unexpected: Option[SyntaxErrorItem],
    expected: Set[SyntaxErrorItem],
    reasons: Set[String],
    line: SyntaxLineInfo
  )
  case SpecialisedError(
    msgs: Set[String],
    line: SyntaxLineInfo
  )
}

object SyntaxErrorLines {
  private def showItem(i: SyntaxErrorItem): String = i match {
    case SyntaxErrorItem.SyntaxRaw(s)     => s
    case SyntaxErrorItem.SyntaxNamed(s)   => s
    case SyntaxErrorItem.SyntaxEndOfInput => "end of input"
  }

  private def rawDetails(lines: SyntaxErrorLines): List[String] = lines match {
    case SyntaxErrorLines.VanillaError(unexpected, expected, reasons, _) =>
      val unexpectedLine = unexpected.map(i => s"unexpected ${showItem(i)}").toList
      val expectedLine =
        if (expected.nonEmpty)
          List("expected " + expected.toList.map(showItem).sorted.mkString(", "))
        else Nil
      val reasonLines = reasons.toList.sorted

      unexpectedLine ++ expectedLine ++ reasonLines

    case SyntaxErrorLines.SpecialisedError(msgs, _) =>
      msgs.toList.sorted
  }

  private def lineInfo(lines: SyntaxErrorLines): SyntaxLineInfo = lines match {
    case SyntaxErrorLines.VanillaError(_, _, _, line) => line
    case SyntaxErrorLines.SpecialisedError(_, line)   => line
  }

  def render(lines: SyntaxErrorLines, col: Int): List[String] = {
    val info = lineInfo(lines)
    val details = SyntaxError.enhanceDetails(rawDetails(lines), info, col)
    details ++ info.snippetLines
  }
}

abstract class SyntaxErrorBuilder extends ErrorBuilder[String] {
  type Position = (Int, Int)

  override def pos(line: Int, col: Int): Position = (line, col)

  type Source = String

  override def source(sourceName: Option[String]): Source =
    sourceName.getOrElse("unknown")

  type Item = SyntaxErrorItem
  type Raw = SyntaxErrorItem.SyntaxRaw
  type Named = SyntaxErrorItem.SyntaxNamed
  type EndOfInput = SyntaxErrorItem.SyntaxEndOfInput.type

  override def raw(item: String): Raw = SyntaxErrorItem.SyntaxRaw(item)

  override def named(item: String): Named = SyntaxErrorItem.SyntaxNamed(item)

  override val endOfInput: EndOfInput = SyntaxErrorItem.SyntaxEndOfInput

  type ExpectedItems = Set[Item]

  override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

  type Message = String
  type Messages = Set[Message]

  override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

  override def reason(reason: String): Message = reason

  override def message(msg: String): Message = msg

  type UnexpectedLine = Option[Item]

  override def unexpected(item: Option[Item]): UnexpectedLine = item

  type ExpectedLine = ExpectedItems
  override def expected(alts: ExpectedItems): ExpectedLine = alts

  type LineInfo = SyntaxLineInfo

  override def lineInfo(
                         line: String,
                         linesBefore: Seq[String],
                         linesAfter: Seq[String],
                         lineNum: Int,
                         errorPointsAt: Int,
                         errorWidth: Int
                       ): SyntaxLineInfo =
    SyntaxLineInfo(line, linesBefore, linesAfter, lineNum, errorPointsAt, math.max(1, errorWidth))

  override val numLinesBefore: Int = 1
  override val numLinesAfter: Int  = 1

  type ErrorInfoLines = SyntaxErrorLines

  override def vanillaError(
                             unexpected: UnexpectedLine,
                             expected: ExpectedLine,
                             reasons: Messages,
                             line: LineInfo
                           ): ErrorInfoLines =
    SyntaxErrorLines.VanillaError(unexpected, expected, reasons, line)

  override def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines =
    SyntaxErrorLines.SpecialisedError(msgs, line)

  override def build(pos: Position, source: Source, lines: ErrorInfoLines): String =
    SyntaxError.render(source, pos, lines)
}

object SyntaxErrorBuilder {
  val instance: ErrorBuilder[String] =
    new SyntaxErrorBuilder with LexToken {
      override def tokens: Seq[Parsley[String]] = {
        val keywords =
          lexer.hardKeywords.toSeq.sorted

        val symbols =
          lexer.hardOperators.toSeq.sortBy(s => (-s.length, s))

        val core = Seq(
          integer.map(n => s"integer $n"),
          float.map(n => s"float $n"),
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

  private val ProgramBeginEndHint =
    "all program body and function declarations must be within `begin` and `end`"

  private val MissingBodyHint =
    "a `begin ... end` block must contain at least one statement"

  private val UnclosedHint =
    "a scope, function or the main body is unclosed"

  private val WhileMissingDoHint =
    "the condition of a while loop must be closed with `do`"

  private val UnclosedWhileHint =
    "unclosed while loop"

  private val StatementStartTokens: Set[String] =
    Set(
      "skip", "if", "while", "for", "do", "begin", "try", "break",
      "continue", "read", "free", "return", "throw", "exit", "print",
      "println", "switch", "int", "bool", "char", "float", "string",
      "pair", "fst", "snd", "identifier", "call", "+", "-", "++", "--"
    )

  private val noisyOps: Set[String] =
    Set("!=", "%", "&", "&&", "*", "+", "-", "/", "<", "<=", "==", ">", ">=", "|", "||")

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

  private[errorMessage] def render(
                                    source: String,
                                    pos: (Int, Int),
                                    lines: SyntaxErrorLines
                                  ): String = {
    val (row, col) = pos
    val header = s"Syntax error in $source ($row, $col):"
    (header :: SyntaxErrorLines.render(lines, col)).mkString("\n")
  }

  private[errorMessage] def enhanceDetails(
                                            details: List[String],
                                            line: SyntaxLineInfo,
                                            col: Int
                                          ): List[String] = {
    val base = collapseNoisyExpected(details)
    val lower = base.map(_.toLowerCase)

    def insertAfterExpected(ds: List[String], h: String): List[String] = {
      val idx = ds.indexWhere(_.toLowerCase.startsWith("expected"))
      if (idx >= 0) ds.take(idx + 1) ++ List(h) ++ ds.drop(idx + 1)
      else ds :+ h
    }

    val expected = expectedTokens(base)
    val unexpected = unexpectedWords(lower)
    val hasUnexpectedKeywordEnd =
      unexpected.contains("end") && !lower.exists(_.startsWith("unexpected end of input"))

    val d1 =
      if (hasUnexpectedKeywordEnd && expected.exists(StatementStartTokens.contains) && !base.contains(MissingBodyHint))
        insertAfterExpected(base, MissingBodyHint)
      else if (expected == Set("begin") && !base.contains(ProgramBeginEndHint))
        insertAfterExpected(base, ProgramBeginEndHint)
      else base

    val lower1 = d1.map(_.toLowerCase)
    val expected1 = expectedTokens(d1)
    val unexpected1 = unexpectedWords(lower1)

    val d2 =
      if (
        expected1.contains("do") &&
          unexpected1.exists(StatementStartTokens.contains) &&
          !d1.contains(WhileMissingDoHint)
      )
        insertAfterExpected(d1, WhileMissingDoHint)
      else d1

    val lower2 = d2.map(_.toLowerCase)
    val expected2 = expectedTokens(d2)
    val unexpected2 = unexpectedWords(lower2)
    val tokLower = line.tokenAt(col).toLowerCase
    val looksLikeMisspelledDone =
      tokLower.startsWith("do") && tokLower != "do" && tokLower != "done"

    val d3 =
      if (
        expected2.contains("done") &&
          !d2.contains(UnclosedWhileHint) &&
          (
            unexpected2.contains("end") ||
              lower2.exists(_.startsWith("unexpected end of input")) ||
              (unexpected2.contains("identifier") && looksLikeMisspelledDone)
            )
      )
        insertAfterExpected(d2, UnclosedWhileHint)
      else d2

    val lower3 = d3.map(_.toLowerCase)
    val expected3 = expectedTokens(d3)
    val hasUnexpectedEoi =
      lower3.exists(s => s.startsWith("unexpected end of input") || s.startsWith("unexpected end of file"))
    val expectsCloser = expected3.exists(Set("end", "fi", "done").contains)

    if (!d3.contains(UnclosedHint) && !d3.contains(UnclosedWhileHint) && hasUnexpectedEoi && expectsCloser)
      insertAfterExpected(d3, UnclosedHint)
    else d3
  }

  private def expectedTokens(details: List[String]): Set[String] =
    details
      .filter(_.toLowerCase.startsWith("expected "))
      .flatMap { s =>
        s.stripPrefix("expected ")
          .split(",")
          .map(_.trim)
          .filter(_.nonEmpty)
      }
      .toSet

  private def unexpectedWords(lowerDetails: List[String]): Set[String] =
    lowerDetails
      .filter(_.startsWith("unexpected"))
      .flatMap(_.split("[^a-z0-9_]+").filter(_.nonEmpty))
      .toSet - "unexpected" - "keyword"

  private def collapseNoisyExpected(details: List[String]): List[String] =
    details.map { s =>
      val tokens = expectedTokens(List(s))
      if (s.toLowerCase.startsWith("expected ") && tokens.contains("end") && tokens.exists(noisyOps.contains))
        "expected end"
      else s
    }

  def format(path: String, parsleyRaw: String): String = {
    val file = Paths.get(path).getFileName.toString
    attachSource(parsleyRaw.trim, file)
  }
}
