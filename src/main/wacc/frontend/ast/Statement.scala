package wacc.frontend.ast

import wacc.common.PositionInfo
import wacc.frontend.ast.Expr.*
import wacc.frontend.ParserBridge.*
import wacc.frontend.ast.WACCException
import wacc.frontend.ast.WACCExceptionType

// Program, func, Param
case class Param(t: Type, name: Identifier)(val positionInfo: PositionInfo)
object Param extends ParserBridgePos2[Type, Identifier, Param]

case class Func(ret: Type, name: Identifier, params: List[Param], body: Stmt)(
  val positionInfo: PositionInfo
)
object Func extends ParserBridgePos4[Type, Identifier, List[Param], Stmt, Func]

case class Program(funcs: List[Func], body: Stmt)(val positionInfo: PositionInfo)
object Program extends ParserBridgePos2[List[Func], Stmt, Program]

case class CatchHandler(exType: WACCExceptionType, exName: Identifier, body: Stmt)
                       (val positionInfo: PositionInfo)

// stmt
sealed trait Stmt { val positionInfo: PositionInfo }
object Stmt{
  case class Skip()(override val positionInfo: PositionInfo) extends Stmt
  case class Decl(t: Type, name: Identifier, rhs: RValue)(override val positionInfo: PositionInfo) extends Stmt
  case class Assign(lhs: LValue, rhs: RValue)(override val positionInfo: PositionInfo) extends Stmt
  case class Read(lhs: LValue)(override val positionInfo: PositionInfo) extends Stmt
  case class Free(e: Expr)(override val positionInfo: PositionInfo) extends Stmt
  case class Return(e: Expr)(override val positionInfo: PositionInfo) extends Stmt
  case class Throw(e: WACCException)(override val positionInfo: PositionInfo) extends Stmt
  case class Exit(e: Expr)(override val positionInfo: PositionInfo) extends Stmt
  case class Print(e: Expr)(override val positionInfo: PositionInfo) extends Stmt
  case class Println(e: Expr)(override val positionInfo: PositionInfo) extends Stmt
  case class IfElse(cond: Expr, thn: Stmt, els: Stmt)(override val positionInfo: PositionInfo) extends Stmt
  case class If(cond: Expr, thn: Stmt)(override val positionInfo: PositionInfo) extends Stmt
  case class While(cond: Expr, body: Stmt)(override val positionInfo: PositionInfo) extends Stmt
  case class TryCatch(tryBody: Stmt, handlers: List[CatchHandler])(override val positionInfo: PositionInfo) extends Stmt
  case class For(init: Stmt, cond: Expr, update: Stmt, body: Stmt)(override val positionInfo: PositionInfo) extends Stmt
  case class DoWhile(body: Stmt, cond: Expr)(override val positionInfo: PositionInfo) extends Stmt
  case class Break()(override val positionInfo: PositionInfo) extends Stmt
  case class Continue()(override val positionInfo: PositionInfo) extends Stmt
  case class BeginEnd(body: Stmt)(override val positionInfo: PositionInfo) extends Stmt
  case class SeqStmt(stmts: List[Stmt])(override val positionInfo: PositionInfo) extends Stmt

  // Parser bridges for statements
  object Throw extends ParserBridgePos1[WACCException, Throw]
  object TryCatch extends ParserBridgePos2[Stmt, List[CatchHandler], TryCatch]
  object Decl extends ParserBridgePos3[Type, Identifier, RValue, Decl]
  object Assign extends ParserBridgePos2[LValue, RValue, Assign]
  object Read extends ParserBridgePos1[LValue, Read]
  object Free extends ParserBridgePos1[Expr, Free]
  object Return extends ParserBridgePos1[Expr, Return]
  object Exit extends ParserBridgePos1[Expr, Exit]
  object Print extends ParserBridgePos1[Expr, Print]
  object Println extends ParserBridgePos1[Expr, Println]
  object IfElse extends ParserBridgePos3[Expr, Stmt, Stmt, IfElse]
  object If extends ParserBridgePos2[Expr, Stmt, If]
  object While extends ParserBridgePos2[Expr, Stmt, While]
  object For extends ParserBridgePos4[Stmt, Expr, Stmt, Stmt, For]
  object DoWhile extends ParserBridgePos2[Stmt, Expr, DoWhile]
  object Break extends ParserSingletonBridgePos[Break] {
    override def con(pos: PositionInfo): Break = Break()(pos)
  }
  object Continue extends ParserSingletonBridgePos[Continue] {
    override def con(pos: PositionInfo): Continue = Continue()(pos)
  }
  object BeginEnd extends ParserBridgePos1[Stmt, BeginEnd]
  object SeqStmt extends ParserBridgePos1[List[Stmt], SeqStmt]
}

// LValue
sealed trait LValue { val positionInfo: PositionInfo }
object LValue {
  case class LIdent(id: Identifier)(override val positionInfo: PositionInfo) extends LValue
  case class LArray(elem: ArrayElem)(override val positionInfo: PositionInfo) extends LValue
  case class LPairElem(pe: PairElem)(override val positionInfo: PositionInfo) extends LValue

  // Parser bridges for LValues
  object LIdent extends ParserBridgePos1[Identifier, LIdent]
  object LArray extends ParserBridgePos1[ArrayElem, LArray]
  object LPairElem extends ParserBridgePos1[PairElem, LPairElem]
}
// RValue
sealed trait RValue { val positionInfo: PositionInfo }
object RValue {
  case class RExpr(e: Expr)(override val positionInfo: PositionInfo) extends RValue
  case class RArrayLiter(elems: List[Expr])(override val positionInfo: PositionInfo) extends RValue
  case class RNewPair(fst: Expr, snd: Expr)(override val positionInfo: PositionInfo) extends RValue
  case class RPairElem(pe: PairElem)(override val positionInfo: PositionInfo) extends RValue
  case class RCall(fn: Identifier, args: List[Expr])(override val positionInfo: PositionInfo) extends RValue

  // Parser bridges for RValues
  object RExpr extends ParserBridgePos1[Expr, RExpr]
  object RArrayLiter extends ParserBridgePos1[List[Expr], RArrayLiter]
  object RNewPair extends ParserBridgePos2[Expr, Expr, RNewPair]
  object RPairElem extends ParserBridgePos1[PairElem, RPairElem]
  object RCall extends ParserBridgePos2[Identifier, List[Expr], RCall]
}

// PairElem
sealed trait PairElem { val positionInfo: PositionInfo }
case class PFst(lv: LValue)(override val positionInfo: PositionInfo) extends PairElem
case class PSnd(lv: LValue)(override val positionInfo: PositionInfo) extends PairElem

object PFst extends ParserBridgePos1[LValue, PFst]
object PSnd extends ParserBridgePos1[LValue, PSnd]
