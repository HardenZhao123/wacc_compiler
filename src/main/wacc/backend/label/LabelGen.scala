package wacc.backend.label

// Generates unique labels.
final class LabelGen {
  private var counter: Long = 0L

  // Generate a fresh label using the given prefix, e.g. fresh("if_end") -> if_end_1
  def fresh(prefix: String): Label = {
    counter += 1
    val p = sanitizePrefix(prefix)
    Label(s"${p}_$counter")
  }

  /** Optional: reset counter for deterministic tests if you want. */
  def reset(): Unit = counter = 0L

  private def sanitizePrefix(prefix: String): String = {
    // keep it assembler-friendly and readable
    val trimmed = prefix.trim
    val base = if (trimmed.isEmpty) "L" else trimmed
    base.map {
      case c if c.isLetterOrDigit => c
      case '_' => '_'
      case _   => '_'
    }
  }
}
