package wacc.frontend.typeCheck

import wacc.frontend.ast.WACCException
import wacc.frontend.ast.WACCException.*
import wacc.frontend.typedAST.TypedException
import wacc.frontend.typedAST.TypedException.*
import wacc.frontend.ast.Expr
import wacc.frontend.typeCheck.TypeCheckerExpr.checkExpr
import wacc.frontend.typedAST.SemanticType
import wacc.frontend.typedAST.SemanticType.{SemString, SemException}
import wacc.frontend.typedAST.TypedExpr

object TypeCheckerWACCException {

  // Type-check a WACC exception node and convert it into a typed exception.
  // Returns:
  //   1. The semantic type of the expression (always SemException)
  //   2. The typed AST representation of the exception.
  def checkWACCException(e: WACCException)
                        (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedException) = e match {
    case ArrayOutOfBoundsException(msg) => checkException(msg)(s => ArrayOutOfBoundsEx(s))
    case BadCharException(msg) => checkException(msg)(s => BadCharEx(s))
    case ArithmeticException(msg) => checkException(msg)(s => ArithmeticEx(s))
    case IntegerOverflowException(msg) => checkException(msg)(s => IntegerOverflowEx(s))
    case NullDereferenceException(msg) => checkException(msg)(s => NullDereferenceEx(s))
    case GeneralException(msg) => checkException(msg)(s => GeneralEx(s))
  }

  // Helper function used to type-check the message expression for an exception.
  // Ensures that the message has type string and constructs the typed exception node.
  private def checkException(msg: Expr)(makeTyped: TypedExpr => TypedException)
                            (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedException) =
    val (_, typedMsg) = checkExpr(msg, Constraint.Is(SemString))
    (Some(SemException), makeTyped(typedMsg))
}


