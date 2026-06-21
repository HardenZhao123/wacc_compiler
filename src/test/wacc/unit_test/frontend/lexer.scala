package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.frontend.lexer
import wacc.frontend.lexer.implicits.given

import org.scalatest.Assertion

class LexerIntegerTest extends AnyFlatSpec {

  private val lex = lexer

  private def assertInteger(input: String, expected: Int): Unit = {
    val r = lex.fully(lex.integer).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful integer parse")
    }
  }

  "integer lexer" should "parse a valid integer" in { assertInteger("45", 45) }
  it should "allow a leading minus sign" in { assertInteger("-45", -45) }
  it should "allow a leading plus sign" in { assertInteger("+45", 45) }
  it should "skip leading whitespaces" in { assertInteger("    45", 45) }
  it should "delete following whitespaces" in { assertInteger("45      ", 45) }
  it should "allow leading zeros" in { assertInteger("000000045", 45) }

  it should "reject non-numeric input" in {
    val r = lex.fully(lex.integer).parse("abc")
    r.shouldBe(a[parsley.Failure[?]])
  }

  it should "reject whitespace between sign and digits" in {
    val r = lex.fully(lex.integer).parse("- 45")
    r.shouldBe(a[parsley.Failure[?]])
  }

  it should "parse Int.MaxValue" in { assertInteger("2147483647", 2147483647) }
  it should "parse Int.MinValue" in { assertInteger("-2147483648", -2147483648) }

  it should "reject overflow above Int.MaxValue" in {
    val r = lex.fully(lex.integer).parse("2147483648")
    r.shouldBe(a[parsley.Failure[?]])
  }

  it should "reject underflow below Int.MinValue" in {
    val r = lex.fully(lex.integer).parse("-2147483649")
    r.shouldBe(a[parsley.Failure[?]])
  }
}

class LexerBooleanTest extends AnyFlatSpec {

  private val lex = lexer

  private def assertBoolean(input: String, expected: Boolean): Unit = {
    val r = lex.fully(lex.boolean).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful boolean parse")
    }
  }

  "boolean lexer" should "parse true" in { assertBoolean("true", true) }
  it should "parse false" in { assertBoolean("false", false) }
  it should "skip leading whitespaces" in { assertBoolean("   true", true) }
  it should "delete following whitespaces" in { assertBoolean("false   ", false) }

  it should "reject non-boolean input" in {
    val r = lex.fully(lex.boolean).parse("truth")
    r.shouldBe(a[parsley.Failure[?]])
  }

  it should "reject boolean with trailing characters (no partial parse)" in {
    val r = lex.fully(lex.boolean).parse("truefalse")
    r.shouldBe(a[parsley.Failure[?]])
  }
}

class LexerCharTest extends AnyFlatSpec {

  private val lex = lexer

  private def assertChar(input: String, expected: Char): Unit = {
    val r = lex.fully(lex.char).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful char parse")
    }
  }

  "char lexer" should "parse a normal graphic ASCII character" in { assertChar("'a'", 'a') }
  it should "parse a space character (space is graphic ASCII)" in { assertChar("' '", ' ') }
  it should "skip leading whitespaces" in { assertChar("   'Z'", 'Z') }
  it should "delete following whitespaces" in { assertChar("'0'   ", '0') }
  it should "ignore a trailing comment after the token" in { assertChar("'x' # comment", 'x') }
}

class LexerFloatTest extends AnyFlatSpec {
  private val lex = lexer

  private def assertFloat(input: String, expected: Float): Unit = {
    val r = lex.fully(lex.float).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful float parse")
    }
  }

  "float lexer" should "parse a normal floating-point number" in { assertFloat("1.5444", 1.5444) }
  it should "parse a negative floating-point number" in { assertFloat("-1.544", -1.544) }
  it should "parse Float.MaxValue" in { assertFloat("3.4028235E38", 3.4028235E38) }
  it should "parse Float.MinValue" in { assertFloat("-3.4028235E38", -3.4028235E38) }
}

class LexerIdentifierTest extends AnyFlatSpec {

  private val lex = lexer

  private def assertIdentifier(input: String, expected: String): Unit = {
    val r = lex.fully(lex.identifier).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful id parse")
    }
  }

  private def assertFail(input: String): Unit = {
    val r = lex.fully(lex.identifier).parse(input)
    r.shouldBe(a[parsley.Failure[?]])
  }

  "identifier lexer" should "parse identifier starting with char" in { assertIdentifier("x", "x") }
  it should "parse identifier starting with underscore" in { assertIdentifier("_x", "_x") }
  it should "parse identifier with digits after start" in { assertIdentifier("a1", "a1") }
  it should "parse identifier with underscores inside" in { assertIdentifier("a_b_c123", "a_b_c123") }
  it should "skip leading and trailing whitespace" in { assertIdentifier("   x123   ", "x123") }

  it should "reject identifier starting with digit" in { assertFail("1abc") }
  it should "reject identifier containing '-'" in { assertFail("a-b") }

  it should "reject keyword if" in { assertFail("if") }
  it should "reject keyword begin" in { assertFail("begin") }
  it should "reject keyword skip" in { assertFail("skip") }
  it should "reject keyword true" in { assertFail("true") }

  it should "ignore a trailing comment after the identifier" in { assertIdentifier("x # comment", "x") }

  it should "allow identifiers that start with a keyword prefix" in {
    assertIdentifier("ifx", "ifx")
    assertIdentifier("begin1", "begin1")
    assertIdentifier("true_1", "true_1")
    assertIdentifier("skip__", "skip__")
  }
}

class LexerStringTest extends AnyFlatSpec {
  private val lex = lexer

  private def assertParses(input: String, expected: String): Unit = {
    val r = lex.fully(lex.string).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful string parse")
    }
  }

  private def assertFails(input: String): Unit = {
    val r = lex.fully(lex.string).parse(input)
    r.shouldBe(a[parsley.Failure[?]])
  }

  "string lexer" should "parse empty string" in { assertParses("\"\"", "") }
  it should "parse a normal ASCII string" in { assertParses("\"abc\"", "abc") }
  it should "parse a string containing spaces" in { assertParses("\"a b c\"", "a b c") }
  it should "parse escape sequences like \\n" in { assertParses("\"hi\\n\"", "hi\n") }
  it should "parse backslash and quote escapes" in { assertParses("\"\\\\\\\"\"", "\\\"") }
  it should "ignore trailing whitespace and comment after a string token" in { assertParses("\"x\"   # comment", "x") }

  it should "reject unterminated strings" in { assertFails("\"unterminated") }
  it should "reject strings containing raw newlines" in { assertFails("\"bad\nline\"") }
  it should "reject unknown escape sequences" in { assertFails("\"\\x\"") }

  it should "parse common escape sequences like \\b \\f \\r" in {
    assertParses("\"a\\bb\"", "a\bb")
    assertParses("\"a\\fb\"", "a\fb")
    assertParses("\"a\\rb\"", "a\rb")
  }
}

class LexerCharEscapeAndFailureTest extends AnyFlatSpec {
  private val lex = lexer

  private def assertParses(input: String, expected: Char): Unit = {
    val r = lex.fully(lex.char).parse(input)
    r match {
      case parsley.Success(value) => value.shouldBe(expected)
      case parsley.Failure(_)     => fail("Expected successful Char parse")
    }
  }

  private def assertFails(input: String): Unit = {
    val r = lex.fully(lex.char).parse(input)
    r.shouldBe(a[parsley.Failure[?]])
  }

  "char lexer" should "parse \\n" in { assertParses("'\\n'", '\n') }
  it should "parse \\t" in { assertParses("'\\t'", '\t') }
  it should "parse \\0" in { assertParses("'\\0'", '\u0000') }
  it should "parse escaped backslash" in { assertParses("'\\\\'", '\\') }
  it should "parse escaped single quote" in { assertParses("'\\''", '\'') }

  it should "reject empty char literal" in { assertFails("''") }
  it should "reject multi-character char literal" in { assertFails("'ab'") }
  it should "reject unknown escape sequences" in { assertFails("'\\x'") }
  it should "reject raw newline in char literal" in { assertFails("'\n'") }

  it should "parse \\b \\f \\r" in {
    assertParses("'\\b'", '\b')
    assertParses("'\\f'", '\f')
    assertParses("'\\r'", '\r')
  }
}

class LexerOperatorTest extends AnyFlatSpec {

  private val lex = lexer

  given parsley.token.symbol.ImplicitSymbol = lex.implicits

  private def assertSucceeds(p: parsley.Parsley[?], input: String): Assertion = {
    val r = lex.fully(p).parse(input)
    r match {
      case parsley.Success(_) => succeed
      case parsley.Failure(_) => fail(s"Expected successful operator parse for input: $input")
    }
  }

  private def assertFails(p: parsley.Parsley[?], input: String): Assertion = {
    val r = lex.fully(p).parse(input)
    r shouldBe a[parsley.Failure[?]]
  }

  "operator lexer" should "lex multi-character operator == as a single token" in {
    assertSucceeds("==", "==")
  }

  it should "lex multi-character operator >= as a single token" in {
    assertSucceeds(">=", ">=")
  }

  it should "lex other multi-character operators as single tokens" in {
    assertSucceeds("!=", "!=")
    assertSucceeds("<=", "<=")
    assertSucceeds("&&", "&&")
    assertSucceeds("||", "||")
  }

  it should "lex single-character operators" in {
    assertSucceeds("=", "=")
    assertSucceeds(";", ";")
    assertSucceeds(",", ",")
    assertSucceeds("(", "(")
    assertSucceeds(")", ")")
    assertSucceeds("[", "[")
    assertSucceeds("]", "]")
    assertSucceeds("+", "+")
    assertSucceeds("-", "-")
    assertSucceeds("*", "*")
    assertSucceeds("/", "/")
    assertSucceeds("%", "%")
    assertSucceeds("!", "!")
    assertSucceeds(">", ">")
    assertSucceeds("<", "<")
  }

  it should "allow whitespace and comments around operators" in {
    assertSucceeds("==", "   ==   # comment")
    assertSucceeds(">=", "\n\t>=\n")
  }

  it should "not parse partially when fully is used" in {
    assertFails("=", "==")
  }
}
