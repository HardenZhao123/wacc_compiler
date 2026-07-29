package wacc.backend.X86.codeGen

import wacc.backend.X86.target.*
import wacc.backend.X86.target.X86Operand
import wacc.backend.BackendCommon.AsmEntity.*

trait X86Instr
object X86Instr {

  // Different sizes defined in x86-64
  enum X86Size(val ptrName: String, val bytes: Int) {
    case Byte extends X86Size("byte ptr", 1)
    case DWord extends X86Size("dword ptr", 4)
    case QWord extends X86Size("qword ptr", 8)
  }

  // Enum representing condition codes for x86-64
  enum X86Cond(val suffix: String) {
    case E extends X86Cond("e")
    case NE extends X86Cond("ne")
    case L extends X86Cond("l")
    case LE extends X86Cond("le")
    case G extends X86Cond("g")
    case GE extends X86Cond("ge")
    case O extends X86Cond("o") // overflow
    case NO extends X86Cond("no")
    case A extends X86Cond("a")   // unsigned above, used after ucomiss
    case AE extends X86Cond("ae") // unsigned above-or-equal, used after ucomiss
    case B extends X86Cond("b")   // unsigned below, used after ucomiss
    case BE extends X86Cond("be") // unsigned below-or-equal, used after ucomiss
  }

  sealed trait X86Address
  object X86Address {
    case class Base(base: X86Reg, disp: Int = 0) extends X86Address
    case class Index(base: Option[X86Reg], index: X86Reg, scale: Int, disp: Int = 0)
      extends X86Address {
      require(Set(1, 2, 4, 8).contains(scale))
    }
    case class RipRelative(label: Label, disp: Int = 0) extends X86Address
  }

  case class Mem(address: X86Address, size: Option[X86Size] = None) extends X86Operand

  // Base trait for real x86-64 assembly instructions
  sealed trait AsmInstr extends X86Instr {
    def opcode: String
    def operands: Seq[Any] = Seq()
  }

  // Trait for binary instructions
  sealed trait BinaryInstr extends AsmInstr {
    val dst: X86Operand
    val src: X86Operand
    override def operands: Seq[Any] = Seq(dst, src)
  }

  case class Add(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "add"
  }
  case class Sub(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "sub"
  }
  case class And(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "and"
  }
  case class Or(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "or"
  }
  case class Xor(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "xor"
  }
  case class Imul(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "imul"
  }

  case class Mov(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "mov"
  }
  case class Movzx(dst: X86Reg, src: X86Operand) extends AsmInstr {
    val opcode = "movzx"
    override def operands = Seq(dst, src)
  }

  case class Movd(dst: X86Operand, src: X86Operand) extends BinaryInstr {
    val opcode = "movd"
  }

  case class Cvtsi2ss(dst: XMM, src: X86Operand) extends AsmInstr {
    val opcode = "cvtsi2ss"
    override def operands = Seq(dst, src)
  }

  case class Cvtss2sd(dst: XMM, src: XMM) extends AsmInstr {
    val opcode = "cvtss2sd"
    override def operands = Seq(dst, src)
  }

  sealed trait FloatBinaryInstr extends AsmInstr {
    val dst: XMM
    val src: XMM
    override def operands: Seq[Any] = Seq(dst, src)
  }

  case class Addss(dst: XMM, src: XMM) extends FloatBinaryInstr {
    val opcode = "addss"
  }
  case class Subss(dst: XMM, src: XMM) extends FloatBinaryInstr {
    val opcode = "subss"
  }
  case class Mulss(dst: XMM, src: XMM) extends FloatBinaryInstr {
    val opcode = "mulss"
  }
  case class Divss(dst: XMM, src: XMM) extends FloatBinaryInstr {
    val opcode = "divss"
  }

  case class Ucomiss(lhs: XMM, rhs: XMM) extends AsmInstr {
    val opcode = "ucomiss"
    override def operands = Seq(lhs, rhs)
  }

  case class Lea(dst: X86Operand, src: Mem) extends AsmInstr {
    val opcode = "lea"
    override def operands = Seq(dst, src)
  }

  case class Idiv(src: X86Operand) extends AsmInstr {
    val opcode = "idiv"
    override def operands = Seq(src)
  }

  case object Cdq extends AsmInstr {
    val opcode = "cdq"
  } // eax -> edx:eax

  case object Cqo extends AsmInstr {
    val opcode = "cqo"
  } // rax -> rdx:rax

  case class Neg(dst: X86Operand) extends AsmInstr {
    val opcode = "neg"
    override def operands = Seq(dst)
  }

  case class Not(dst: X86Operand) extends AsmInstr {
    val opcode = "not"
    override def operands = Seq(dst)
  }

  case class Cmp(lhs: X86Operand, rhs: X86Operand) extends BinaryInstr {
    val opcode = "cmp"
    val dst = lhs
    val src = rhs
  }

  case class Test(lhs: X86Operand, rhs: X86Operand) extends BinaryInstr {
    val opcode = "test"
    val dst = lhs
    val src = rhs
  }
  case class Setcc(cond: X86Cond, dst: X86Reg) extends AsmInstr {
    val opcode = s"set${cond.suffix}"
    override def operands = Seq(dst)
  }
  case class Jmp(label: Label) extends AsmInstr {
    val opcode = "jmp"
    override def operands = Seq(label)
  }
  case class Jcc(cond: X86Cond, label: Label) extends AsmInstr {
    val opcode = s"j${cond.suffix}"
    override def operands = Seq(label)
  }

  case class Call(label: Label) extends AsmInstr {
    val opcode = "call"
    override def operands = Seq(label)
  }
  case object Ret extends AsmInstr {
    val opcode = "ret"
  }
  case class Push(src: X86Operand) extends AsmInstr {
    val opcode = "push"
    override def operands = Seq(src)
  }
  case class Pop(dst: X86Operand) extends AsmInstr {
    val opcode = "pop"
    override def operands = Seq(dst)
  }
  case object Leave extends AsmInstr {
    val opcode = "leave"
  }
  case object Nop extends AsmInstr {
    val opcode = "nop"
  }
}
