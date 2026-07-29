package wacc.backend.X86.preDefFunctions

object preDefHelpersConstant {
  val LBL_PRINTI_STR0 = ".L._printi_str0"
  val LBL_PRINTC_STR0 = ".L._printc_str0"
  val LBL_PRINTP_STR0 = ".L._printp_str0"
  val LBL_PRINTFL_STR0 = ".L._printfl_str0"
  val LBL_PRINTS_STR0 = ".L._prints_str0"
  val LBL_READI_STR0 = ".L._readi_str0"
  val LBL_READC_STR0 = ".L._readc_str0"
  val LBL_READFL_STR0 = ".L._readfl_str0"
  val LBL_PRINTB_STR_FALSE = ".L._printb_str0"
  val LBL_PRINTB_STR_TRUE = ".L._printb_str1"
  val LBL_PRINTB_FMT0 = ".L._printb_str2"
  val LBL_PRINTB_TRUE = ".L_printb0"
  val LBL_PRINTB_GO = ".L_printb1"
  val LBL_ERR_UNHANDLED_PREFIX = "LBL_ERR_UNHANDLED_PREFIX"
  val LBL_ERRNULL_STR0 = ".L._errNull_str0"
  val LBL_ERROVERFLOW_STR0 = ".L._errOverflow_str0"
  val LBL_ERROUTOFMEMORY_STR0 = ".L._errOutOfMemory_str0"
  val LBL_ERRDIVZERO_STR0 = ".L._errDivZero_str0"
  val LBL_ERROUTOFBOUNDS_STR0 = ".L._errOutOfBounds_str0"
  val LBL_ERRBADCHAR_STR0 = ".L._errBadChar_str0"
  val ERR_NULL = "_errNull"
  val ERR_OUT_OF_MEMORY = "_errOutOfMemory"

  val LIBC_PRINTF = "printf@plt"
  val LIBC_FFLUSH = "fflush@plt"
  val LIBC_PUTCHAR = "putchar@plt"
  val LIBC_SCANF = "scanf@plt"
  val LIBC_MALLOC = "malloc@plt"
  val LIBC_FREE = "free@plt"
  val LIBC_EXIT = "exit@plt"

  val MOVE_ZERO = 0
  val FLOAT_VARIADIC_ARG_COUNT = 1
  val COMPARE_ZERO = 0
  val STACK_ALIGN_BYTES = 16
  val STR_HDR = 4
  val NEWLINE_CHAR = 10

  val FMT_CHAR_C = "%c"
  val FMT_INT_D = "%d"
  val FMT_PTR_P = "%p"
  val FMT_FLOAT_G = "%g"
  val FMT_LEN_STR = "%.*s"
  val FMT_SCAN_CHAR_SPACE_C = " %c"
  val FMT_FLOAT_F = "%f"

  val FATAL_PREFIX = "fatal error: "
  val FATAL_SUFFIX_NEWLINE_ESC = "\\n"
  val MSG_NULL_DEREF_OR_FREED = "null pair dereferenced or freed"
  val MSG_INT_OVERFLOW_OR_UNDERFLOW = "integer overflow or underflow occurred"
  val MSG_OUT_OF_MEMORY = "out of memory"
  val MSG_DIV_OR_MOD_BY_ZERO = "division or modulo by zero"
  val MSG_ARRAY_INDEX_OOB_FMT = "array index %d out of bounds"
  val MSG_INT_NOT_ASCII_0_127_FMT = "int %d is not ascii character 0-127 "
  val STR_FALSE = "false"
  val STR_TRUE = "true"
}
