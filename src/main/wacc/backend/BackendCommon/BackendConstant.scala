package wacc.backend.BackendCommon

import wacc.common.ExitCode.RuntimeError

object BackendConstant {

  val A64_WORD_SIZE = 8

  val A32_WORD_SIZE = 4

  val ASCII_MIN = 0

  val ASCII_MAX = 127
  
  val ARITHMETIC_EXCEPTION = 1
  
  val BADCHAR_EXCEPTION = 2
  
  val ARRAY_OUT_OF_BOUNDS_EXCEPTION = 3
  
  val INTEGER_OVERFLOW_EXCEPTION = 4
  
  val NULL_DEREFERENCE_EXCEPTION = 5

  val GENERAL_EXCEPTION = 6

  val RUNTIME_ERROR_EXIT_CODE = RuntimeError

  val ASM_INDENT = "\t"

  val ASM_NEWLINE = "\n"

  val ASM_OPERAND_SEPARATOR = ", "

  val ASM_DIRECTIVE_WORD = ".word"

  val ASM_DIRECTIVE_ASCIZ = ".asciz"

  val ASM_DIRECTIVE_ALIGN_4 = ".align 4"

  val ASM_DIRECTIVE_DATA = ".data"

  val ASM_DIRECTIVE_TEXT = ".text"

  val ASM_DIRECTIVE_GLOBAL_MAIN = ".global main"

  val LABEL_MAIN = "main"

  val LIBC_EXIT_SYMBOL = "exit"

  val STRING_LABEL_PREFIX = ".str_"

  val ERR_UNREACHABLE = "unreachable"
}
