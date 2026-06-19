package wacc.backend.AArch64.codeGen

//constant recognised by the AArch64 multiplication lowerer.
object A64CodeGenConstant {

  enum MulConst(val value: Int) {
    case Zero extends MulConst(0)
    case One extends MulConst(1)
    case NegOne extends MulConst(-1)
    case Two extends MulConst(2)
    case Three extends MulConst(3)
    case Four extends MulConst(4)
    case Five extends MulConst(5)
    case Eight extends MulConst(8)
  }

  object MulConst {
    def fromInt(value: Int): Option[MulConst] = value match {
      case 0  => Some(MulConst.Zero)
      case 1  => Some(MulConst.One)
      case -1 => Some(MulConst.NegOne)
      case 2  => Some(MulConst.Two)
      case 3  => Some(MulConst.Three)
      case 4  => Some(MulConst.Four)
      case 5  => Some(MulConst.Five)
      case 8  => Some(MulConst.Eight)
      case _  => None
    }
  }
}