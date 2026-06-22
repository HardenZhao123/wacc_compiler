package wacc.frontend.returnCheck

import wacc.frontend.ast.Stmt.*
import wacc.frontend.ast.{Stmt, Program}
import wacc.frontend.ast.SwitchLabel.DefaultLabel

import scala.collection.mutable

object ReturnCheck {
  // Return true iff this statement is a returning block
  def isReturning(s: Stmt): Boolean = s match {
    case Return(_) => true
    case Throw(_) => true
    case Exit(_) => true

    case IfElse(_, thn, els) =>
      // Both branches must terminate to guarantee the if-statement as a whole returns.
      isReturning(thn) && isReturning(els)

    case Switch(_, cases) =>
      // Every possible entry point must eventually return. A non-returning case
      // can still satisfy this by falling through to a returning later case.
      val hasDefault = cases.exists { c =>
        c.labels.exists {
          case _: DefaultLabel => true
          case _ => false
        }
      }

      var followingCasesReturn = false
      val allEntriesReturn = cases.reverse.forall { c =>
        val returnsInThisBody = c.body.lastOption.exists(isReturning)
        val canUseFollowingCase = !c.body.exists(mayBreakCurrentSwitch)
        val entryReturns = returnsInThisBody || (canUseFollowingCase && followingCasesReturn)
        followingCasesReturn = entryReturns
        entryReturns
      }

      hasDefault && allEntriesReturn

    case TryCatch(tryBody, handlers) => 
      // Every catch path must also terminate; otherwise control could still fall through.
      isReturning(tryBody) && handlers.forall(h => isReturning(h.body))

    case BeginEnd(body) =>
      isReturning(body)

    case SeqStmt(stmts) =>
      // Earlier statements may have side effects, but only the final reachable statement decides fallthrough.
      stmts.lastOption.exists(isReturning)

    // Everything else are not returning
    case _ =>
      false
  }

  private def mayBreakCurrentSwitch(s: Stmt): Boolean = s match {
    case Break() => true
    case If(_, thn) => mayBreakCurrentSwitch(thn)
    case IfElse(_, thn, els) => mayBreakCurrentSwitch(thn) || mayBreakCurrentSwitch(els)
    case TryCatch(tryBody, handlers) =>
      mayBreakCurrentSwitch(tryBody) || handlers.exists(h => mayBreakCurrentSwitch(h.body))
    case BeginEnd(body) => mayBreakCurrentSwitch(body)
    case SeqStmt(stmts) => stmts.exists(mayBreakCurrentSwitch)
    // A nested loop or switch consumes its own Break statements.
    case While(_, _) | For(_, _, _, _) | DoWhile(_, _) | Switch(_, _) => false
    case _ => false
  }

  // Check all functions; emit syntax errors if a function body is not returning
  def checkProgram(p: Program, errs: mutable.Builder[NonReturnErr, List[NonReturnErr]]): Unit = {
    p.funcs.foreach { f =>
      if (!isReturning(f.body)) {
        errs += NonReturnErr.NonReturningFunction(f.name.ident, f.positionInfo)
      }
    }
  }
}
