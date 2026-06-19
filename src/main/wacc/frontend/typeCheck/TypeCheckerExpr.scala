package wacc.frontend.typeCheck

import wacc.common.PositionInfo
import wacc.frontend.ast.*
import wacc.frontend.ast.Expr.*
import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.typedAST.*
import wacc.frontend.typeCheck.TypeCheckError.*
import TypeCheckerHelpers.*
import wacc.frontend.ast.LValue.*

/* TypeCheckerExpr performs type checking for expressions. */
object TypeCheckerExpr {

  def checkExpr(expr: Expr, c: Constraint)
               (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {
    val pos = expr.positionInfo

    expr match {
      // Operators delegate to small helpers so inference/error behaviour stays uniform across the AST.
      // Unary: !, -, len, ord, chr and ~
      case Not(operand) => checkUnaryOp(operand, SemBool, c, pos)(e => TypedExpr.Not(e))
      case Neg(operand) => checkUnaryOp(operand, SemInt, c, pos)(e => TypedExpr.Neg(e))
      case Len(operand) => checkUnaryOp(operand, SemArray(SemUnknown, anyDimension), c, pos)(e => TypedExpr.Len(e))
      case Ord(operand) => checkUnaryOp(operand, SemChar, c, pos)(e => TypedExpr.Ord(e))
      case Chr(operand) => checkUnaryOp(operand, SemInt, c, pos)(e => TypedExpr.Chr(e))
      case BitNot(operand) => checkUnaryOp(operand, SemInt, c, pos)(e => TypedExpr.BitNot(e))

      // Arithmetic (fixed operand type = int)
      case Add(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Add))
      case Sub(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Sub))
      case Mul(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Mul))
      case Div(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Div))
      case Mod(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Mod))

      // Boolean (fixed operand type = bool)
      case And(left, right) => checkBinaryOpFixed(left, right, SemBool, c, pos)((l, r) =>
        TypedExpr.BinaryBool(l, r, TypedExpr.BoolOperation.And))
      case Or(left, right) => checkBinaryOpFixed(left, right, SemBool, c, pos)((l, r) =>
        TypedExpr.BinaryBool(l, r, TypedExpr.BoolOperation.Or))

      // Equality
      case Equal(left, right) => checkEquality(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.Equal))
      case NotEqual(left, right) => checkEquality(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.NotEqual))

      // Comparisons (int/char, needs coordination)
      case Greater(left, right) => checkIntChar(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.Greater))
      case GreaterEqual(left, right) => checkIntChar(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.GreaterEqual))
      case Less(left, right) => checkIntChar(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.Less))
      case LessEqual(left, right) => checkIntChar(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.LessEqual))

      // Bitwise operations (& and |, needs int)
      case BitAnd(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) => 
        TypedExpr.BinaryBitwise(l, r, TypedExpr.BitwiseAndOr.BitAnd))
      case BitOr(left, right) => checkBinaryOpFixed(left, right, SemInt, c, pos)((l, r) =>
        TypedExpr.BinaryBitwise(l, r, TypedExpr.BitwiseAndOr.BitOr))

      // Literals
      case IntLiter(v) => (SemInt.satisfies(c)(using ctx, pos), TypedExpr.IntLit(v))
      case BooleanLiter(v) => (SemBool.satisfies(c)(using ctx, pos), TypedExpr.BoolLit(v))
      case CharLiter(v) => (SemChar.satisfies(c)(using ctx, pos), TypedExpr.CharLit(v))
      case StringLiter(v) => (SemString.satisfies(c)(using ctx, pos), TypedExpr.StrLit(v))
      case PairLiter() => (SemPairErased.satisfies(c)(using ctx, pos), TypedExpr.PairLit())

      case Parens(ex) => checkExpr(ex, c)

      // Array elements and identifiers reuse the l-value pipeline because lookup/inference rules are identical.
      // LValues treated as expressions
      case ae: ArrayElem =>
        TypeCheckerLR.checkLValue(LArray(ae)(ae.positionInfo), c)
      case id: Identifier =>
        TypeCheckerLR.checkLValue(LIdent(id)(id.positionInfo), c)
    }
  }

  /* Check a binary operator where both operands must have a fixed type */
  private def checkBinaryOpFixed(left: Expr, right: Expr, operandExpected: SemanticType, parentConstraint: Constraint, pos: PositionInfo)
                                (makeTyped: (TypedExpr, TypedExpr) => TypedExpr)
                                (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {

    val sideConstraint = if operandExpected == SemUnknown then Constraint.UnConstraint
      else Constraint.Is(operandExpected)

    val (_, leftTypedExpr)  = checkExpr(left, sideConstraint)
    val (_, rightTypedExpr) = checkExpr(right, sideConstraint)

    val finalTypedExpr = makeTyped(leftTypedExpr, rightTypedExpr)
    val finalTyOpt = finalTypedExpr.ty.satisfies(parentConstraint)(using ctx, pos)

    (finalTyOpt, finalTypedExpr)
  }

  /* Check comparison operators that allow either int or char operands. */
  private def checkIntChar(left: Expr, right: Expr, parentConstraint: Constraint, pos: PositionInfo)
                          (makeTyped: (TypedExpr, TypedExpr) => TypedExpr)
                          (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {
    val (leftTyOpt, leftTypedExpr) = checkExpr(left, Constraint.Either(SemInt, SemChar))

    // Let the left operand narrow the right operand when possible, but keep the union if the left stays unknown.
    val rightConstraint = leftTyOpt match {
      case Some(t) if t != SemUnknown => Constraint.Is(t)
      case _                          => Constraint.Either(SemInt, SemChar)
    }

    val (_, rightTypedExpr) = checkExpr(right, rightConstraint)
    val finalTypedExpr = makeTyped(leftTypedExpr, rightTypedExpr)

    // If one side was unknown, try to align; if both unknown, this will report CannotInferType at the operator position.
    unifyTypes(leftTypedExpr, rightTypedExpr)(using ctx, pos)

    val finalTyOpt = finalTypedExpr.ty.satisfies(parentConstraint)(using ctx, pos)
    (finalTyOpt, finalTypedExpr)
  }

  /* Check a unary operator with a fixed operand type. */
  private def checkUnaryOp(expr: Expr, expected: SemanticType, parentConstraint: Constraint, pos: PositionInfo)
                          (makeTyped: TypedExpr => TypedExpr)
                          (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {
    val (_, typedExpr) = checkExpr(expr, Constraint.Is(expected))
    val finalTypedExpr = makeTyped(typedExpr)
    val finalTyOpt = finalTypedExpr.ty.satisfies(parentConstraint)(using ctx, pos)
    (finalTyOpt, finalTypedExpr)
  }

  /* Check equality operators (==, !=). */
  private def checkEquality(left: Expr, right: Expr, parentConstraint: Constraint, pos: PositionInfo)
                           (makeTyped: (TypedExpr, TypedExpr) => TypedExpr)
                           (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {

    // Equality is intentionally looser than arithmetic/comparison: both sides are first checked without bias.
    val (_, leftTyped)  = checkExpr(left, Constraint.UnConstraint)
    val (_, rightTyped) = checkExpr(right, Constraint.UnConstraint)

    val finalTyped = makeTyped(leftTyped, rightTyped)

    val t1 = leftTyped.ty
    val t2 = rightTyped.ty

    if (SemanticType.isKnown(t1) && SemanticType.isKnown(t2) && !SemanticType.haveCommonSupertype(t1, t2)) {
      ctx.addError(TypeMismatch(SemanticType.show(t1), SemanticType.show(t2), pos))
    }

    val finalTyOpt = finalTyped.ty.satisfies(parentConstraint)(using ctx, pos)
    (finalTyOpt, finalTyped)
  }
}
