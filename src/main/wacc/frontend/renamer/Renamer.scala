package wacc.frontend.renamer

import wacc.common.PositionInfo
import scala.collection.mutable
import wacc.frontend.ast.*
import wacc.frontend.ast.Expr.*
import wacc.frontend.ast.Stmt.*
import wacc.frontend.ast.LValue.*
import wacc.frontend.ast.RValue.*
import wacc.frontend.ast.WACCException
import wacc.frontend.ast.WACCException.*

// wrapper for renaming result(include symbol table and function table)
final case class RenameResult(
                               program: Program,
                               funcsTable: Map[String, PositionInfo],
                               declInfo: Map[Renamed, DeclInfo]
                             )

object Renamer {
  // collect all function names,
  // allowing overloads but rejecting duplicate signatures (same name + same parameter types)
  private def buildFuncTable(funcs: List[Func])(using st: RenamerState): Map[String, PositionInfo] = {
    val byName = mutable.Map.empty[String, PositionInfo]
    val seenSignatures = mutable.Map.empty[(String, List[Type]), PositionInfo]

    funcs.foreach { f =>
      val n = f.name.ident
      val sig = f.params.map(_.t)
      val sigKey = (n, sig)

      seenSignatures.get(sigKey) match {
        case Some(prevPos) =>
          st.emit(RenameError.FuncRedeclaration(n, f.positionInfo, prevPos))
        case None =>
          seenSignatures(sigKey) = f.positionInfo
      }

      if (!byName.contains(n)) {
        byName(n) = f.positionInfo
      }
    }

    byName.toMap
  }

  // top level entry point for renamer:
  // builds the global function table, renames all functions and the main body
  def renameProgram(p: Program, errs: mutable.Builder[RenameError, List[RenameError]]): RenameResult = {
    given st: RenamerState = new RenamerState(mutable.Map.empty, mutable.Map.empty, errs)

    val funcsTable = buildFuncTable(p.funcs)

    val funcs2 = p.funcs.map(renameFunc(_, funcsTable))
    val body2 = renameStmt(p.body, funcsTable)(using new RenamerScope(mutable.Map.empty, Map.empty), st)

    RenameResult(Program(funcs2, body2)(p.positionInfo), funcsTable, st.declInfo.toMap)
  }

  // Rename a function with parameters in an outer scope and the body in a nested scope (allows shadowing)
  private def renameFunc(f: Func, funcs: Map[String, PositionInfo])(using st: RenamerState): Func = {
    given paramScope: RenamerScope = new RenamerScope(mutable.Map.empty, Map.empty)

    val params2 = f.params.map { p =>
      val id2 = RenamerScope.declare(p.name, p.t)
      Param(p.t, id2)(p.positionInfo)
    }

    // Body gets a fresh inner scope whose parent includes the parameters
    val body2 = renameStmt(f.body, funcs)(using paramScope.newScope, st)

    Func(f.ret, f.name, params2, body2)(f.positionInfo)
  }

  private def renameStmt(s: Stmt, funcs: Map[String, PositionInfo])(using ctx: RenamerScope, st: RenamerState): Stmt = s match {

    case Skip() => s

    // variable declaration: rename RHS first,
    // then declare the new binding in the current scope
    case Decl(t, name, rhs) =>
      val rhs2 = renameRValue(rhs, funcs)
      val name2 = RenamerScope.declare(name, t)
      Decl(t, name2, rhs2)(s.positionInfo)

    // assignment: rename both side
    case Assign(lhs, rhs) =>
      Assign(renameLValue(lhs, funcs), renameRValue(rhs, funcs))(s.positionInfo)

    case Read(lhs) =>
      Read(renameLValue(lhs, funcs))(s.positionInfo)

    case Free(e) =>
      Free(renameExpr(e, funcs))(s.positionInfo)

    case Return(e) =>
      Return(renameExpr(e, funcs))(s.positionInfo)

    case Throw(e) =>
      Throw(renameWACCException(e, funcs))(s.positionInfo)

    case Exit(e) =>
      Exit(renameExpr(e, funcs))(s.positionInfo)

    case Print(e) =>
      Print(renameExpr(e, funcs))(s.positionInfo)

    case Println(e) =>
      Println(renameExpr(e, funcs))(s.positionInfo)

    case ExprStmt(e) =>
      ExprStmt(renameExpr(e, funcs))(s.positionInfo)

    case BeginEnd(body) =>
      BeginEnd(renameStmt(body, funcs)(using ctx.newScope, st))(s.positionInfo)
      
    // if statement
    case If(cond, thn) =>
      val cond2 = renameExpr(cond, funcs)
      val thn2 = renameStmt(thn, funcs)(using ctx.newScope, st)
      If(cond2, thn2)(s.positionInfo)

    // if-else statement: condition in current scope, each branch renamed in its own nested scope
    case IfElse(cond, thn, els) =>
      val cond2 = renameExpr(cond, funcs)
      val thn2 = renameStmt(thn, funcs)(using ctx.newScope, st)
      val els2 = renameStmt(els, funcs)(using ctx.newScope, st)
      IfElse(cond2, thn2, els2)(s.positionInfo)
      
    // Switch statement  
    case Switch(selector, cases) => 
      val selector2 = renameExpr(selector, funcs)
      val cases2 = cases.map(c => renameSwitchCaseBody(c, funcs))
      Switch(selector2, cases2)(s.positionInfo)

    // while loop: condition in current scope, body renamed in a nested scope
    case While(cond, body) =>
      val cond2 = renameExpr(cond, funcs)
      val body2 = renameStmt(body, funcs)(using ctx.newScope, st)
      While(cond2, body2)(s.positionInfo)

    // Handle try-catch statements
    case TryCatch(tryBody, handlers) =>
      val try2 = renameStmt(tryBody, funcs)(using ctx.newScope, st)
      val handlers2 = handlers.map(renameCatchHandler(_, funcs))
      TryCatch(try2, handlers2)(s.positionInfo)

    // Handle for-loop statements
    case For(init, cond, update, body) =>
      val loopScope = ctx.newScope
      val init2 = renameStmt(init, funcs)(using loopScope, st)
      val cond2 = renameExpr(cond, funcs)(using loopScope, st)
      val update2 = renameStmt(update, funcs)(using loopScope, st)
      val body2 = renameStmt(body, funcs)(using loopScope, st)
      For(init2, cond2, update2, body2)(s.positionInfo)

    // Handle do-while loop statements
    case DoWhile(body, cond) =>
      val body2 = renameStmt(body, funcs)(using ctx.newScope, st)
      val cond2 = renameExpr(cond, funcs)
      DoWhile(body2, cond2)(s.positionInfo)

    case Break() => s

    case Continue() => s

    // sequence stmt: rename statements left to right in the same scope so earlier declarations affect later ones
    case SeqStmt(stmts) =>
      val ss2 = stmts.map(renameStmt(_, funcs))
      SeqStmt(ss2)(s.positionInfo)
  }

  // Rename an LValue (left-hand side of an assignment)
  private def renameLValue(lv: LValue, funcs: Map[String, PositionInfo])(using ctx: RenamerScope, st: RenamerState): LValue = lv match {
    case LIdent(id) =>
      LIdent(RenamerScope.fromScope(id))(lv.positionInfo)

    case LArray(ae) =>
      LArray(renameArrayElem(ae, funcs))(lv.positionInfo)

    case LPairElem(pe) =>
      LPairElem(renamePairElem(pe, funcs))(lv.positionInfo)
  }

  // Rename a pair element (fst or snd access)
  private def renamePairElem(pe: PairElem, funcs: Map[String, PositionInfo])(using ctx: RenamerScope, st: RenamerState): PairElem = pe match {
    case PFst(lv) => PFst(renameLValue(lv, funcs))(pe.positionInfo)
    case PSnd(lv) => PSnd(renameLValue(lv, funcs))(pe.positionInfo)
  }

  // Rename an RValue (right-hand side of an assignment)
  private def renameRValue(rv: RValue, funcs: Map[String, PositionInfo])(using ctx: RenamerScope, st: RenamerState): RValue = rv match {

    case RExpr(e) =>
      RExpr(renameExpr(e, funcs))(rv.positionInfo)

    case RArrayLiter(elems) =>
      RArrayLiter(elems.map(renameExpr(_, funcs)))(rv.positionInfo)

    case RNewPair(fst, snd) =>
      RNewPair(renameExpr(fst, funcs), renameExpr(snd, funcs))(rv.positionInfo)

    case RPairElem(pe) =>
      RPairElem(renamePairElem(pe, funcs))(rv.positionInfo)

    case RCall(fn, args) =>
      val fName = fn.ident
      if (!funcs.contains(fName)) {
        st.emit(RenameError.FuncOutOfScope(fName, fn.positionInfo))
      }
      RCall(fn, args.map(renameExpr(_, funcs)))(rv.positionInfo)
  }

  // Rename an array element access
  private def renameArrayElem(ae: ArrayElem, funcs: Map[String, PositionInfo])(using ctx: RenamerScope, st: RenamerState): ArrayElem = {
    val id2 = RenamerScope.fromScope(ae.ident)
    val idx2 = ae.indices.map(renameExpr(_, funcs))
    ArrayElem(id2, idx2)(ae.positionInfo)
  }

  private def renameExpr(e: Expr, funcs: Map[String, PositionInfo])(using ctx: RenamerScope, st: RenamerState): Expr = e match {

    // atoms
    case IntLiter(_) | BooleanLiter(_) | CharLiter(_) | FloatLiter(_) | StringLiter(_) | PairLiter() => e
    case id: Identifier => RenamerScope.fromScope(id)
    case ae: ArrayElem => renameArrayElem(ae, funcs)
    case Parens(inner) => Parens(renameExpr(inner, funcs))(e.positionInfo)

    // unary operation
    case Not(op) => Not(renameExpr(op, funcs))(e.positionInfo)
    case Neg(op) => Neg(renameExpr(op, funcs))(e.positionInfo)
    case Len(op) => Len(renameExpr(op, funcs))(e.positionInfo)
    case Ord(op) => Ord(renameExpr(op, funcs))(e.positionInfo)
    case Chr(op) => Chr(renameExpr(op, funcs))(e.positionInfo)
    case BitNot(op) => BitNot(renameExpr(op, funcs))(e.positionInfo)

    // binary operation
    case Mul(l, r) => Mul(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Div(l, r) => Div(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Mod(l, r) => Mod(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Add(l, r) => Add(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Sub(l, r) => Sub(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Greater(l, r) => Greater(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case GreaterEqual(l, r) => GreaterEqual(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Less(l, r) => Less(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case LessEqual(l, r) => LessEqual(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Equal(l, r) => Equal(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case NotEqual(l, r) => NotEqual(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case And(l, r) => And(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case Or(l, r) => Or(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case BitAnd(l, r) => BitAnd(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case BitOr(l, r) => BitOr(renameExpr(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    
    // unary side-effecting expr
    case Increment(op) => Increment(renameLValue(op, funcs))(e.positionInfo)
    case Decrement(op) => Decrement(renameLValue(op, funcs))(e.positionInfo)
    
    // binary side-effecting expr 
    case AddEqual(l, r) => AddEqual(renameLValue(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case SubEqual(l, r) => SubEqual(renameLValue(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case MulEqual(l, r) => MulEqual(renameLValue(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case DivEqual(l, r) => DivEqual(renameLValue(l, funcs), renameExpr(r, funcs))(e.positionInfo)
    case ModEqual(l, r) => ModEqual(renameLValue(l, funcs), renameExpr(r, funcs))(e.positionInfo)
  }
  
  // Helper function to rename WACCExceptions
  private def renameWACCException(e: WACCException, funcs: Map[String, PositionInfo])
                                 (using ctx: RenamerScope, st: RenamerState): WACCException = e match {
    case ArrayOutOfBoundsException(msg) => ArrayOutOfBoundsException(renameExpr(msg, funcs))(e.positionInfo)
    case BadCharException(msg) => BadCharException(renameExpr(msg, funcs))(e.positionInfo)
    case ArithmeticException(msg) => ArithmeticException(renameExpr(msg, funcs))(e.positionInfo)
    case IntegerOverflowException(msg) => IntegerOverflowException(renameExpr(msg, funcs))(e.positionInfo)
    case NullDereferenceException(msg) => NullDereferenceException(renameExpr(msg, funcs))(e.positionInfo)
    case GeneralException(msg) => GeneralException(renameExpr(msg, funcs))(e.positionInfo)
  }

  private def renameCatchHandler(h: CatchHandler, funcs: Map[String, PositionInfo])
                                (using ctx: RenamerScope, st: RenamerState): CatchHandler = {
    // Each catch block gets its own fresh scope
    val catchScope = ctx.newScope

    // Declare the exception variable in this specific catch scope
    val exName2 = RenamerScope.declare(h.exName, h.exType)(using catchScope, st)
    val body2 = renameStmt(h.body, funcs)(using catchScope, st)

    CatchHandler(h.exType, exName2, body2)(h.positionInfo)
  }
  
  private def renameSwitchCaseBody(switchCaseBody: SwitchCaseBody, funcs: Map[String, PositionInfo])
                                  (using ctx: RenamerScope, st: RenamerState): SwitchCaseBody = {
    val labels2 = switchCaseBody.labels.map(label => renameSwitchLabel(label, funcs))
    val body2 = switchCaseBody.body.map(b => renameStmt(b, funcs))
    SwitchCaseBody(labels2, body2)(switchCaseBody.positionInfo)
  }
  
  private def renameSwitchLabel(label: SwitchLabel, funcs: Map[String, PositionInfo])
                               (using ctx: RenamerScope, st: RenamerState): SwitchLabel = {
    label match {
      case SwitchLabel.CaseLabel(value) => 
        val value2 = renameExpr(value, funcs)
        SwitchLabel.CaseLabel(value2)(label.positionInfo)
      case SwitchLabel.DefaultLabel() => SwitchLabel.DefaultLabel()(label.positionInfo)
    }
  }
}
