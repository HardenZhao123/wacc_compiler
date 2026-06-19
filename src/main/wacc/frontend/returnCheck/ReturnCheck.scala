package wacc.frontend.returnCheck

import wacc.frontend.ast.Stmt.*
import wacc.frontend.ast.{Stmt, Program}

import scala.collection.mutable

object ReturnCheck {
  // Return true iff this statement is a returning block
  def isReturning(s: Stmt): Boolean = s match {
    case Return(_) => true
    case Throw(_) => true
    case Exit(_) => true

    case If(_, thn, els) =>
      // Both branches must terminate to guarantee the if-statement as a whole returns.
      isReturning(thn) && isReturning(els)

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

  // Check all functions; emit syntax errors if a function body is not returning
  def checkProgram(p: Program, errs: mutable.Builder[NonReturnErr, List[NonReturnErr]]): Unit = {
    p.funcs.foreach { f =>
      if (!isReturning(f.body)) {
        errs += NonReturnErr.NonReturningFunction(f.name.ident, f.positionInfo)
      }
    }
  }
}
