package wacc.backend.AArch64.codeGen

object A64FormatterConstant {
  val ASM_INDENT = "\t"
  val ASM_NEWLINE = "\n"
  val ASM_OPERAND_SEPARATOR = ", "
  val ASM_SPACE = " "
  val ASM_DIRECTIVE_DATA = ".data"
  val ASM_DIRECTIVE_WORD = ".word"
  val ASM_DIRECTIVE_ASCIZ = ".asciz"
  val ASM_DIRECTIVE_ALIGN_4 = ".align 4"
  val RELOC_LO_PREFIX = ":lo"
  val IMM_PREFIX_DEC = "#"
  val IMM_PREFIX_HEX = "#0x"
  val A64_UIMM12_MAX = 4095
}
