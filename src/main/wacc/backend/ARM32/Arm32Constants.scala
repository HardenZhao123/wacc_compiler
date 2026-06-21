package wacc.backend.Arm32

import wacc.backend.Arm32.target.{A32Registers}

object Arm32Constants {

  val RETURN_REG = A32Registers.R0

  val R1_REG = A32Registers.R1

  val R2_REG = A32Registers.R2

  val R3_REG = A32Registers.R3

  val SCRATCH_REG_START = A32Registers.R4

  val ARG_REG_COUNT = 4

  val STR_HDR = 4

  val ARR_HDR = 4

  val SPILL_TMP_AREA_SLOTS = 4

  val STACK_ALLIGNMENT = 8

  val SLOT_SIZE = 4

  val FRAME_HEADER_BYTES = 8

  val PAIR_SIZE = 4

  val FST_OFF = 0

  val SND_OFF = 4

  val NULL_PTR = 0L

  val MAX_INT_ARM32 = 256

  val BoolCharSize = 1

  val OverflowCheck = 31

  val POOP_INTERVAL = 120

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
  
  val AEABI_IDIVMOD = "__aeabi_idivmod"

  val AEABI_FADD = "__aeabi_fadd"

  val AEABI_FSUB = "__aeabi_fsub"

  val AEABI_FMUL = "__aeabi_fmul"

  val AEABI_FDIV = "__aeabi_fdiv"

  val AEABI_FCMPEQ = "__aeabi_fcmpeq"

  val AEABI_FCMPLT = "__aeabi_fcmplt"

  val AEABI_FCMPLE = "__aeabi_fcmple"

  val AEABI_FCMPGT = "__aeabi_fcmpgt"

  val AEABI_FCMPGE = "__aeabi_fcmpge"
  
  val A32_POOP_SKIP = "a32_pool_skip"
  
  val LTORG = ".ltorg"
  
  val WACC_EXCEPTION_FLAG = "wacc_exception_flag"
  
  val WACC_EXCEPTION_VALUE = "wacc_exception_value"
}
