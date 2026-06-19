package wacc.backend.BackendCommon

import wacc.backend.midir.TAC

// Enum representing condition codes.
enum Cond {
  case EQ, NE, LT, LE, GT, GE, LS, HI, VC, VS, PL, MI, CC, CS, AL

  // Converts the enum to its string mnemonic used in assembly.
  def toCondStr: String = this match {
    case EQ => "eq"
    case NE => "ne"
    case LT => "lt"
    case LE => "le"
    case GT => "gt"
    case GE => "ge"
    case LS => "ls"
    case HI => "hi"
    case VC => "vc"
    case VS => "vs"
    case PL => "pl"
    case MI => "mi"
    case CC => "cc"
    case CS => "cs"
    case AL => ""
  }
}

object fromTACCondOpToCond {
  // Converts TAC comparison operators to backend condition codes.
  def toCond(c: TAC.CondOp): Cond = c match {
    case TAC.CondOp.EQ => Cond.EQ
    case TAC.CondOp.NEQ => Cond.NE
    case TAC.CondOp.LT => Cond.LT
    case TAC.CondOp.LEQ => Cond.LE
    case TAC.CondOp.GT => Cond.GT
    case TAC.CondOp.GEQ => Cond.GE
  }
}
