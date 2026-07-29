package wacc.backend.X86

object X86Constants {

  import wacc.backend.X86.target.X86Registers

  val _EXIT = "_exit"

  val _PRINTI = "_printi"

  val _PRINTS = "_prints"

  val _PRINTB = "_printb"

  val _PRINTFL = "_printfl"

  val _PRINTLN = "_println"

  val _PRINTC = "_printc"

  val _PRINTP = "_printp"

  val _READI = "_readi"

  val _READC = "_readc"

  val _READFL = "_readfl"

  val _FREE = "_free"

  val _FREEPAIR = "_freepair"

  val _MALLOC = "_malloc"

  val _ERR_UNHANDLED = "_errUnhandled"

  val STACK_ALLIGNMENT = 16

  val SLOT_SIZE = 8

  val ARG_REG_COUNT = 6

  val FRAME_HEADER_BYTES = 16

  val SPILL_TMP_AREA_SLOTS = 6

  val PAIR_SIZE = 16

  val FST_OFF = 0

  val SND_OFF = 8

  val ARR_HDR = 4

  val NULL_PTR = 0L

  val SIZE_CHAR_BOOL_BYTES = 1

  val SIZE_INT_BYTES = 4

  val SIZE_PTR_BYTES = 8

  val RETURN_REG = X86Registers.RAX

  val FIRST_ARG_REG = X86Registers.RDI
}
