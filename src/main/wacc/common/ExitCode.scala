package wacc.common

/* Exit codes returned by the compiler. */
object ExitCode {
    final val Success: Int = 0
    final val SyntaxError: Int = 100
    final val SemanticError: Int = 200
    final val UsageError: Int = 1
    final val RuntimeError: Int = 255
}
