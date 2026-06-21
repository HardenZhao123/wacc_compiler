package wacc.frontend.typeCheck

import wacc.frontend.ast.*
import wacc.frontend.typeCheck.TypeCheckError.*
import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.typedAST.*
import TypeCheckerHelpers.*
import wacc.frontend.ast.Stmt.*
import wacc.frontend.typeCheck.TypeCheckerWACCException

object TypeCheckerStmt {

  def checkFunc(func: Func)(using ctx: TypeChecker.TypeCheckerCtx): TypedFunction = {
    val Func(_, name, params, body) = func
    val funcName = name.ident
    val declaredParamTypes = params.map(p => SemanticType.fromAstType(p.t))
    val resolved = ctx.resolveDeclaredFunc(funcName, declaredParamTypes)
      .getOrElse(TypeChecker.FuncEntry(funcName, TypeChecker.mangleFuncName(funcName, declaredParamTypes), SemUnknown, declaredParamTypes, func.positionInfo))
    val retTy = resolved.returnType
    val paramTypes = resolved.paramTypes
    val resolvedName = resolved.resolvedName

    // Reuse the resolved overload signature so the typed function carries the mangled backend-facing name.
    // Create typed parameters from declared parameter types
    val typedParams = (params zip paramTypes).map { (param, paramType) =>
      TypedLValue.Id(param.name.ident, paramType)
    }

    // Build a fresh variable environment for the function body
    val paramEnv: Map[String, SemanticType] = typedParams.map(p => p.name -> p.ty).toMap

    // Save the outer context because function bodies type-check in an isolated variable scope.
    val oldEnv = ctx.env
    val oldIsFunction = ctx.isFunction
    val oldCurrentReturnType = ctx.currentReturnType

    // Enter function scope
    ctx.isFunction = true
    ctx.currentReturnType = Some(retTy)
    ctx.env = TypeEnv(varEnv = paramEnv, funcEnv = oldEnv.funcEnv)

    // Type check function body
    val typedStmts = checkStmtHelper(body)

    // Restore previous context
    ctx.env = oldEnv
    ctx.isFunction = oldIsFunction
    ctx.currentReturnType = oldCurrentReturnType

    TypedFunction(retTy, funcName, resolvedName, typedParams, typedStmts)
  }

  def checkStmt(stmt: Stmt)(using ctx: TypeChecker.TypeCheckerCtx): Option[TypedStmt] = stmt match {

    case Skip() => Some(TypedStmt.Skip())

    // Variable declaration
    case Decl(t, name, rhs) =>

      // Check RHS
      val declared = SemanticType.fromAstType(t)
      val (rhsTyOpt, rhsTypedRValue) = TypeCheckerLR.checkRValue(rhs, Constraint.Is(declared))

      // Extend the variable env
      ctx.env = ctx.env.copy(
        varEnv = ctx.env.varEnv + (name.ident -> declared)
      )
      val rhsTy = rhsTyOpt.getOrElse(SemUnknown)
      (declared, rhsTy) match {
        // Array compatibility check
        case (a1 @ SemArray(t1, n1), a2 @ SemArray(t2, n2)) if n1 != n2 || !compatible(t1, t2) =>
          ctx.addError(TypeMismatch(SemanticType.show(a1), SemanticType.show(a2), rhs.positionInfo))
        case _ => ()
      }

      Some(TypedStmt.Decl(TypedLValue.Id(name.ident, declared), rhsTypedRValue))

    // Assignment
    case Assign(lhs, rhs) =>
      // First, type-check LHS to obtain the expected type.
      val (lhsTyOpt, lhsTypedLValue) = TypeCheckerLR.checkLValue(lhs, Constraint.UnConstraint)
      val expectedTy = lhsTyOpt.getOrElse(SemUnknown)

      // Then type-check RHS under the expected type (the original, correct behaviour).
      val (rhsTyOpt, rhsTypedRValue) = TypeCheckerLR.checkRValue(rhs, Constraint.Is(expectedTy))
      val rhsTy = rhsTyOpt.getOrElse(SemUnknown)

      // Pair-exchange special case:
      // If both sides remain truly unknown AND at least one side is a pair element access (fst/snd),
      // report the dedicated error instead of a generic "cannot infer" / silent pass.
      val bothTrulyUnknown = (expectedTy == SemUnknown) && (rhsTy == SemUnknown)
      if (bothTrulyUnknown && (isPairLValue(lhs) || isPairRValue(rhs))) {
        ctx.addError(PairExchangeUnknownTypes(stmt.positionInfo))
        Some(TypedStmt.Assign(lhsTypedLValue, rhsTypedRValue))
      } else {
        // Normal assignment: enforce compatibility and emit TypeMismatch when needed.
        unifyTypes(lhsTypedLValue, rhsTypedRValue)(using ctx, stmt.positionInfo)
        Some(TypedStmt.Assign(lhsTypedLValue, rhsTypedRValue))
      }

    // Read statement
    case Read(lhs) =>

      // l-value must have a scalar type supported by the runtime input helpers
      val (rawTyOpt, lhsTypedLValue) = TypeCheckerLR.checkLValue(lhs, Constraint.UnConstraint)
      rawTyOpt match {
        case Some(SemInt) | Some(SemChar) | Some(SemFloat) => ()
        case Some(SemUnknown) => ctx.addError(CannotInferType(stmt.positionInfo))
        case Some(otherTy) =>
          ctx.addError(TypeMismatch("int, char or float", SemanticType.show(otherTy), stmt.positionInfo))
        case None =>
          ctx.addError(CannotInferType(stmt.positionInfo))
      }
      Some(TypedStmt.Read(lhsTypedLValue))

    // Free statement
    case Free(expr) =>
      val (_, typedExpr) =
        TypeCheckerExpr.checkExpr(expr, Constraint.Either(SemArray(SemUnknown, anyDimension), SemPair(SemUnknown, SemUnknown)))
      Some(TypedStmt.Free(typedExpr))

    // Return statement
    case Return(expr) =>
      // Check if return not inside function body
      if (!ctx.isFunction) {
        ctx.addError(ReturnInMain(stmt.positionInfo))
        None
      } else {
        val expected = ctx.currentReturnType.get
        val (_, typedExpr) = TypeCheckerExpr.checkExpr(expr, Constraint.Is(expected))
        Some(TypedStmt.Return(typedExpr))
      }

    case Throw(e) =>
      val (_, typedException) = TypeCheckerWACCException.checkWACCException(e)
      Some(TypedStmt.Throw(typedException))

    // Exit statement
    case Exit(expr) =>
      val (_, typedExpr) = TypeCheckerExpr.checkExpr(expr, Constraint.Is(SemInt))
      Some(TypedStmt.Exit(typedExpr))

    // Print statement
    case Print(expr) =>
      val (_, typedExpr) = TypeCheckerExpr.checkExpr(expr, Constraint.UnConstraint)
      Some(TypedStmt.Print(typedExpr))

    // Println Statement
    case Println(expr) =>
      val (_, typedExpr) = TypeCheckerExpr.checkExpr(expr, Constraint.UnConstraint)
      Some(TypedStmt.Println(typedExpr))

    // If statement  
    case If(cond, thn) => 
      val (_, condTypedExpr) = TypeCheckerExpr.checkExpr(cond, Constraint.Is(SemBool))
      val thenTypedStmts = checkStmtHelper(thn)
      
      Some(TypedStmt.If(condTypedExpr, thenTypedStmts))

    // If-Else statement
    case IfElse(cond, thn, els) =>
      val (_, condTypedExpr) = TypeCheckerExpr.checkExpr(cond, Constraint.Is(SemBool))

      // Branches are checked independently so declarations in one arm do not leak into the other.
      // Check `then` body
      val thenTypedStmts = checkStmtHelper(thn)

      // Check `else` body
      val elseTypedStmts = checkStmtHelper(els)

      Some(TypedStmt.IfElse(condTypedExpr, thenTypedStmts, elseTypedStmts))

    // While loop
    case While(cond, body) =>
      val (_, condTypedExpr) = TypeCheckerExpr.checkExpr(cond, Constraint.Is(SemBool))
      val bodyTypedStmt = withLoopDepth { checkStmtHelper(body) }
      Some(TypedStmt.While(condTypedExpr, bodyTypedStmt))
      
    case TryCatch(tryBody, handlers) =>
      // Snapshot the incoming environment because neither the try body nor any catch binding should escape.
      // Check the try body in its own scope
      val oldEnv = ctx.env
      val tryTypedStmts = checkStmtHelper(tryBody)
      // Restore immediately after the try block
      ctx.env = oldEnv

      // Check each handler
      val typedHandlers = handlers.map { h =>
        // Each catch block gets a fresh scope
        val catchEnv = oldEnv.copy(
          varEnv = oldEnv.varEnv + (h.exName.ident -> SemString)
        )
        ctx.env = catchEnv

        val handlerBodyTyped = checkStmtHelper(h.body)

        // Restore environment for the next handler
        ctx.env = oldEnv
        
        TypedCatchHandler(h.exType, TypedLValue.Id(h.exName.ident, SemString), handlerBodyTyped)
      }

      Some(TypedStmt.TryCatch(tryTypedStmts, typedHandlers))

    case For(init, cond, update, body) =>
      // The initializer introduces bindings that are visible to the condition,
      // update and body, but the entire loop scope must not escape afterwards.
      val oldEnv = ctx.env
      val (initTypedStmts, condTypedExpr, updateTypedStmts, bodyTypedStmts) =
        try {
          withLoopDepth {
            val initTyped = checkStmtHelper(init)
            val (_, condTyped) = TypeCheckerExpr.checkExpr(cond, Constraint.Is(SemBool))
            val updateTyped = checkStmtHelper(update)
            val bodyTyped = checkStmtHelper(body)
            (initTyped, condTyped, updateTyped, bodyTyped)
          }
        } finally {
          ctx.env = oldEnv
        }

      Some(TypedStmt.For(initTypedStmts, condTypedExpr, updateTypedStmts, bodyTypedStmts))

    case DoWhile(body, cond) =>
      val (_, condTypedExpr) = TypeCheckerExpr.checkExpr(cond, Constraint.Is(SemBool))
      val bodyTypedStmt = withLoopDepth { checkStmtHelper(body) }
      Some(TypedStmt.DoWhile(bodyTypedStmt, condTypedExpr))

    case Break() =>
      if (ctx.loopDepth <= 0) {
        ctx.addError(BreakOutsideLoop(stmt.positionInfo))
      }
      Some(TypedStmt.Break())

    case Continue() =>
      if (ctx.loopDepth <= 0) {
        ctx.addError(ContinueOutsideLoop(stmt.positionInfo))
      }
      Some(TypedStmt.Continue())

    // Begin-end block
    case BeginEnd(body) =>
      val bodyTypedStmt = body match {
        case SeqStmt(stmts) => stmts.flatMap(checkStmt)
        case singleStmt => List(singleStmt).flatMap(checkStmt)
      }
      Some(TypedStmt.BeginEnd(bodyTypedStmt))

    case SeqStmt(stmts) =>
      Some(TypedStmt.BeginEnd(stmts.flatMap(checkStmt)))
  }

  // True if the lhs is a pair-elem
  private def isPairLValue(lv: wacc.frontend.ast.LValue): Boolean = lv match {
    case wacc.frontend.ast.LValue.LPairElem(_) => true
    case _ => false
  }

  // True if the rhs is a pair-elem
  private def isPairRValue(rv: wacc.frontend.ast.RValue): Boolean = rv match {
    case wacc.frontend.ast.RValue.RPairElem(_) => true
    case _ => false
  }

  private def checkStmtHelper(stmt: Stmt)(using ctx: TypeChecker.TypeCheckerCtx): List[TypedStmt] = stmt match {
    case SeqStmt(stmts) => stmts.flatMap(checkStmt)
    case singleStmt => List(singleStmt).flatMap(checkStmt)
  }

  private def withLoopDepth[A](f: => A)(using ctx: TypeChecker.TypeCheckerCtx): A = {
    // Track nesting centrally so break/continue validation stays consistent across while/for/do-while.
    ctx.loopDepth += 1
    try f
    finally ctx.loopDepth -= 1
  }
}
