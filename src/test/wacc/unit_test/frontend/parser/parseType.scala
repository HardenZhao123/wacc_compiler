package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Success, Failure}

import wacc.frontend.parser
import wacc.frontend.ast.*

class BaseTypeParserTest extends AnyFlatSpec with ParserTestHelpers {

  private val p = parser

  "baseType parser" should "parse int" in {
    val r = p.parseBaseType("   int   ")
    r match {
      case Success(IntType()) => succeed
      case Success(other)     => fail(s"Unexpected parse result: $other")
      case Failure(err)       => fail(s"Parsing failed: $err")
    }
  }

  it should "parse bool" in {
    val r = p.parseBaseType("bool")
    r match {
      case Success(BoolType()) => succeed
      case Success(other)      => fail(s"Unexpected parse result: $other")
      case Failure(err)        => fail(s"Parsing failed: $err")
    }
  }

  it should "parse float" in {
    val r = p.parseBaseType("float")
    r match {
      case Success(FloatType()) => succeed
      case Success(other) => fail(s"Unexpected parse result: $other")
      case Failure(err) => fail(s"Parsing failed: $err")
    }
  }

  it should "parse char" in {
    val r = p.parseBaseType("  char ")
    r match {
      case Success(CharType()) => succeed
      case Success(other)      => fail(s"Unexpected parse result: $other")
      case Failure(err)        => fail(s"Parsing failed: $err")
    }
  }

  it should "parse string" in {
    val r = p.parseBaseType("string")
    r match {
      case Success(StringType()) => succeed
      case Success(other)        => fail(s"Unexpected parse result: $other")
      case Failure(err)          => fail(s"Parsing failed: $err")
    }
  }

  it should "reject non-base types like pair" in {
    expectFailure(p.parseBaseType("pair"))
  }
}

class PairElemTypeParserTest extends AnyFlatSpec with ParserTestHelpers {

  private val p = parser

  "pairElemType parser" should "parse base types" in {
    p.parsePairElemType("int") match {
      case Success(IntType()) => succeed
      case Success(other)     => fail(s"Unexpected parse result: $other")
      case Failure(err)       => fail(s"Parsing failed: $err")
    }

    p.parsePairElemType("bool") match {
      case Success(BoolType()) => succeed
      case Success(other)      => fail(s"Unexpected parse result: $other")
      case Failure(err)        => fail(s"Parsing failed: $err")
    }
  }

  it should "parse erased pair element type" in {
    val r = p.parsePairElemType("pair")
    r match {
      case Success(PairErasedType()) => succeed
      case Success(other)            => fail(s"Unexpected parse result: $other")
      case Failure(err)              => fail(s"Parsing failed: $err")
    }
  }

  it should "parse array element types with one or more dimensions" in {
    val r1 = p.parsePairElemType("int[]")
    r1 match {
      case Success(ArrayType(IntType(), 1)) => succeed
      case Success(other)                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                     => fail(s"Parsing failed: $err")
    }

    val r2 = p.parsePairElemType("bool[][]")
    r2 match {
      case Success(ArrayType(BoolType(), 2)) => succeed
      case Success(other)                    => fail(s"Unexpected parse result: $other")
      case Failure(err)                      => fail(s"Parsing failed: $err")
    }
  }
}

class PairTypeParserTest extends AnyFlatSpec with ParserTestHelpers {

  private val p = parser

  "pairType parser" should "parse simple pair of base types" in {
    val r = p.parsePairType("pair(int, bool)")
    r match {
      case Success(PairType(IntType(), BoolType())) => succeed
      case Success(other)                           => fail(s"Unexpected parse result: $other")
      case Failure(err)                             => fail(s"Parsing failed: $err")
    }
  }

  it should "allow erased pair as element type" in {
    val r = p.parsePairType("pair(int, pair)")
    r match {
      case Success(PairType(IntType(), PairErasedType())) => succeed
      case Success(other)                                 => fail(s"Unexpected parse result: $other")
      case Failure(err)                                   => fail(s"Parsing failed: $err")
    }
  }

  it should "reject non-erased pair directly inside another non-erased pair" in {
    expectFailure(p.parsePairType("pair(int, pair(int, bool))"))
  }

  it should "allow interleaving via arrays: pair(int, pair(int,bool)[])" in {
    val r = p.parsePairType("pair(int, pair(int, bool)[])")
    r match {
      case Success(PairType(IntType(), ArrayType(PairType(IntType(), BoolType()), 1))) => succeed
      case Success(other)                                                              => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                => fail(s"Parsing failed: $err")
    }
  }

  it should "reject arrays of erased pair" in {
    expectFailure(p.parsePairType("pair(pair[], int)"))
  }
}

class TypeParserTest extends AnyFlatSpec with ParserTestHelpers {

  private val p = parser

  "type parser" should "parse base type without arrays" in {
    p.parseType("int") match {
      case Success(IntType()) => succeed
      case Success(other)     => fail(s"Unexpected parse result: $other")
      case Failure(err)       => fail(s"Parsing failed: $err")
    }
  }

  it should "parse base type arrays" in {
    p.parseType("int[][]") match {
      case Success(ArrayType(IntType(), 2)) => succeed
      case Success(other)                   => fail(s"Unexpected parse result: $other")
      case Failure(err)                     => fail(s"Parsing failed: $err")
    }
  }

  it should "parse pair type as a top-level type" in {
    p.parseType("pair(int, bool)") match {
      case Success(PairType(IntType(), BoolType())) => succeed
      case Success(other)                           => fail(s"Unexpected parse result: $other")
      case Failure(err)                             => fail(s"Parsing failed: $err")
    }
  }

  it should "parse arrays of pair types" in {
    p.parseType("pair(int, bool)[]") match {
      case Success(ArrayType(PairType(IntType(), BoolType()), 1)) => succeed
      case Success(other)                                         => fail(s"Unexpected parse result: $other")
      case Failure(err)                                           => fail(s"Parsing failed: $err")
    }
  }

  it should "parse nested pair via array interleaving" in {
    p.parseType("pair(int, pair(int, bool)[])") match {
      case Success(PairType(IntType(), ArrayType(PairType(IntType(), BoolType()), 1))) => succeed
      case Success(other)                                                              => fail(s"Unexpected parse result: $other")
      case Failure(err)                                                                => fail(s"Parsing failed: $err")
    }
  }
}
