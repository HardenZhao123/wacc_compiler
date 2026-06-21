package wacc.backend.AArch64

import wacc.backend.AArch64.target.{Reg, XZR}
import wacc.backend.AArch64.target.A64CallingConvention.*

object AArch64Constants {
  val PREDEF_FUNC_REG_64 = Reg.X1

  val RETURN_REG_64 = Reg.X0

  val RETURN_REG_32 = Reg.W0

  val SCRATCH_REG_START = Reg.X9

  val PRINT_HELP_REG_64 = Reg.X2

  val STACK_POINTER = stackPointer

  val LINK_REGISTER = linkRegister

  val FRAME_POINTER = framePointer

  val ZERO_REGISTER = XZR

  val MOVE_ZERO = 0

  val COMPARE_ZERO = 0

  val FAIL = -1

  val DATA_SEG_ALIGN = 4

  val STACK_ALIGN = 16

  val PAIR_SIZE = 16

  val LO_12 = 12

  val FST_OFF = 0

  val SND_OFF = 8

  //HEADER_SIZE
  val ARR_HDR = 4
  val STR_HDR = 4

  val SHIFT_16 = 16

  val MASK_16 = 0xFFFF

  val PRINTF = "printf"

  val FFLUSH = "fflush"

  val SCANF = "scanf"

  val EXIT = "exit"

  val PUTS = "puts"

  val FREE = "free"

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

  val _FREEPAIR = "_freepair"

  val _FREEARR = "_freearr"

  val _MALLOC = "_malloc"

  val MALLOC = "malloc"
  
  val _ERR_UNHANDLED = "_errUnhandled"
}
