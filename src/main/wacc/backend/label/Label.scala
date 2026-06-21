package wacc.backend.label

// A symbolic label used by backend codegen/asm/data sections.
final case class Label(name: String) extends AnyVal {
  override def toString: String = name
}
