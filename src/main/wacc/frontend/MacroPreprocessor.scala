package wacc.frontend

import java.nio.file.Paths
import scala.collection.mutable

object MacroPreprocessor {
  final case class MacroError(line: Int, column: Int, message: String) {
    def format(path: String, input: String): String = {
      val file = Paths.get(path).getFileName.toString
      val sourceLine = input.linesIterator.drop(line - 1).nextOption().getOrElse("")
      val pointer = " " * math.max(0, column - 1) + "^"
      s"Syntax error in $file ($line, $column):\n$message\n|\n|$sourceLine\n|$pointer\n|"
    }
  }

  private final case class MacroDefinition(replacement: String, line: Int)

  private val Define = "@define"

  def preprocess(input: String): Either[MacroError, String] = {
    val macros = mutable.Map.empty[String, MacroDefinition]
    val output = new StringBuilder
    val lines = input.split("\\n", -1)

    var index = 0
    while (index < lines.length) {
      val rawLine = lines(index)
      val (line, lineEndingPrefix) =
        if (rawLine.endsWith("\r")) (rawLine.dropRight(1), "\r") else (rawLine, "")
      val lineNumber = index + 1

      parseDirective(line, lineNumber) match {
        case Left(error) => return Left(error)
        case Right(Some((name, replacement, column))) =>
          macros.get(name) match {
            case Some(previous) =>
              return Left(MacroError(lineNumber, column, s"macro '$name' is already defined on line ${previous.line}"))
            case None => macros(name) = MacroDefinition(replacement, lineNumber)
          }
        case Right(None) =>
          expandText(line, lineNumber, macros.toMap) match {
            case Left(error) => return Left(error)
            case Right(expanded) => output.append(expanded)
          }
      }

      if (index + 1 < lines.length) output.append(lineEndingPrefix).append('\n')
      index += 1
    }

    Right(output.result())
  }

  private def parseDirective(line: String, lineNumber: Int): Either[MacroError, Option[(String, String, Int)]] = {
    val start = skipWhitespace(line, 0)
    val afterDefine = start + Define.length
    val isDefine =
      line.startsWith(Define, start) &&
        (afterDefine == line.length || line.charAt(afterDefine).isWhitespace)

    if (!isDefine) Right(None)
    else {
      var index = skipWhitespace(line, afterDefine)
      if (index >= line.length || line.charAt(index) == '#') {
        Left(MacroError(lineNumber, afterDefine + 1, "expected a macro name after @define"))
      } else if (!isIdentifierStart(line.charAt(index))) {
        Left(MacroError(lineNumber, index + 1, "macro name must be an identifier"))
      } else {
        val nameStart = index
        index += 1
        while (index < line.length && isIdentifierPart(line.charAt(index))) index += 1
        val name = line.substring(nameStart, index)

        if (index < line.length && line.charAt(index) == '(') {
          Left(MacroError(lineNumber, index + 1, "function-like macros are not supported"))
        } else if (index < line.length && !line.charAt(index).isWhitespace && line.charAt(index) != '#') {
          Left(MacroError(lineNumber, index + 1, "macro name must be followed by whitespace or a comment"))
        } else {
          index = skipWhitespace(line, index)
          val replacement = line.substring(index, commentStart(line, index)).trim
          Right(Some((name, replacement, nameStart + 1)))
        }
      }
    }
  }

  private def expandText(
      text: String,
      line: Int,
      macros: Map[String, MacroDefinition],
      stack: List[String] = Nil,
      originColumn: Int = 1
  ): Either[MacroError, String] = {
    val output = new StringBuilder
    var index = 0

    while (index < text.length) {
      val current = text.charAt(index)
      if (current == '#') {
        output.append(text.substring(index))
        index = text.length
      } else if (current == '"' || current == '\'') {
        index = appendQuoted(text, index, output)
      } else if (isIdentifierStart(current)) {
        val start = index
        index += 1
        while (index < text.length && isIdentifierPart(text.charAt(index))) index += 1
        val name = text.substring(start, index)

        macros.get(name) match {
          case None => output.append(name)
          case Some(definition) =>
            val column = if (stack.isEmpty) start + 1 else originColumn
            if (stack.contains(name)) {
              val cycle = (stack.dropWhile(_ != name) :+ name).mkString(" -> ")
              return Left(MacroError(line, column, s"recursive macro expansion: $cycle"))
            }
            expandText(definition.replacement, line, macros, stack :+ name, column) match {
              case Left(error) => return Left(error)
              case Right(expanded) => output.append(expanded)
            }
        }
      } else {
        output.append(current)
        index += 1
      }
    }

    Right(output.result())
  }

  private def appendQuoted(text: String, start: Int, output: StringBuilder): Int = {
    val quote = text.charAt(start)
    output.append(quote)
    var index = start + 1
    var escaped = false

    while (index < text.length && (escaped || text.charAt(index) != quote)) {
      val current = text.charAt(index)
      output.append(current)
      if (escaped) escaped = false
      else if (current == '\\') escaped = true
      index += 1
    }

    if (index < text.length) {
      output.append(quote)
      index + 1
    } else index
  }

  private def commentStart(line: String, start: Int): Int = {
    var index = start
    var quote: Option[Char] = None
    var escaped = false

    while (index < line.length) {
      val current = line.charAt(index)
      quote match {
        case Some(delimiter) =>
          if (escaped) escaped = false
          else if (current == '\\') escaped = true
          else if (current == delimiter) quote = None
        case None =>
          if (current == '"' || current == '\'') quote = Some(current)
          else if (current == '#') return index
      }
      index += 1
    }

    line.length
  }

  private def skipWhitespace(text: String, start: Int): Int = {
    var index = start
    while (index < text.length && text.charAt(index).isWhitespace) index += 1
    index
  }

  private def isIdentifierStart(char: Char): Boolean = char.isLetter || char == '_'
  private def isIdentifierPart(char: Char): Boolean = char.isLetterOrDigit || char == '_'
}
