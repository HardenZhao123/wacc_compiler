package wacc.frontend

import parsley.Parsley
import parsley.token.{Basic, Lexer}
import parsley.token.descriptions.*
import parsley.token.symbol.ImplicitSymbol

object lexer {

    // Set of  keywords that cannot be used as identifiers
    // and are recognised directly by the lexer.
    private final val hardKeyWords: Set[String] = Set(
        "int", "char", "float", "string", "bool", "pair", "null",
        "true", "false", "begin", "end", "is", "skip", "switch", "case",
        "read", "free", "return", "throw", "exit", "print", "println",
        "if", "then", "else", "fi", "while", "do", "done",
        "newpair", "fst", "snd", "call", "len", "ord", "chr",
        "try", "catch", "for", "break", "continue", "default",
        "ArrayOutOfBoundsException", "BadCharException",
        "ArithmeticException", "IntegerOverflowException",
        "NullDereferenceException", "Exception"
    )

    // Set of operators and symbols that are tokenised atomically
    // and not parsed as identifiers.
    private final val hardOperators: Set[String] = Set(
        "!", "-", "*", "/", "%", "+",
        ">", ">=", "<", "<=",
        "==", "!=", "&&", "||",
        "=", ";", ",", "(", ")", "[", "]",
        "&", "|", "~"
    )

    // Mapping of escape characters used in char and string literals
    // to their corresponding ASCII integer values.
    private final val escapeMapping: Map[String, Int] = Map(
        "0" -> 0x0000,
        "b" -> 0x0008,
        "t" -> 0x0009,
        "n" -> 0x000a,
        "f" -> 0x000c,
        "r" -> 0x000d
    )
    
    // Complete lexical description defining identifiers, whitespace,
    // numbers, symbols, and text literal behaviour.
    private val desc = LexicalDesc.plain.copy(

        // Identifier rules
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c.isLetter || c == '_'),
            identifierLetter = Basic(c => c.isLetterOrDigit || c == '_')
        ),

        // Whitespace and comment handling.
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
            space = Basic(c => c.isWhitespace)
        ),

        // Numeric literal configuration.
        numericDesc = NumericDesc.plain.copy(
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeBinary = false,
            integerNumbersCanBeOctal = false
        ),

        // Keyword and operator configuration.
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = hardKeyWords,
            hardOperators = hardOperators
        ),

        // Character and string literal configuration
        textDesc = TextDesc.plain.copy(
            escapeSequences = EscapeDesc.plain.copy(
                literals = Set('\\', '\"', '\''),
                mapping = escapeMapping
            ),
            graphicCharacter = Basic(
                c => c >= ' ' && c <= '~' &&  !Set('\\', '\"', '\'').contains(c)
            )
        )
    )

    private val lexer = Lexer(desc)

    val identifier: Parsley[String] = lexer.lexeme.names.identifier
    val integer: Parsley[Int] = lexer.lexeme.integer.decimal32[Int]
    val float: Parsley[Float] = lexer.lexeme.floating.float
    val char: Parsley[Char] = lexer.lexeme.character.ascii
    val string: Parsley[String] = lexer.lexeme.string.ascii
    val boolean: Parsley[Boolean] = lexer.lexeme.symbol("true").as(true) |
                                    lexer.lexeme.symbol("false").as(false)
    val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
