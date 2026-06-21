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
      case Neg(operand) => checkNumericUnaryOp(operand, c, pos)(e => TypedExpr.Neg(e))
      case Len(operand) => checkUnaryOp(operand, SemArray(SemUnknown, anyDimension), c, pos)(e => TypedExpr.Len(e))
      case Ord(operand) => checkUnaryOp(operand, SemChar, c, pos)(e => TypedExpr.Ord(e))
      case Chr(operand) => checkUnaryOp(operand, SemInt, c, pos)(e => TypedExpr.Chr(e))
      case BitNot(operand) => checkUnaryOp(operand, SemInt, c, pos)(e => TypedExpr.BitNot(e))

      // Arithmetic
      case Add(left, right) => checkNumericBinaryOp(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Add))
      case Sub(left, right) => checkNumericBinaryOp(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Sub))
      case Mul(left, right) => checkNumericBinaryOp(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryArithmetic(l, r, TypedExpr.ArithmeticOperation.Mul))
      case Div(left, right) => checkNumericBinaryOp(left, right, c, pos)((l, r) =>
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

      // Ordered comparisons (int/char/float, needs coordination)
      case Greater(left, right) => checkOrdered(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.Greater))
      case GreaterEqual(left, right) => checkOrdered(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.GreaterEqual))
      case Less(left, right) => checkOrdered(left, right, c, pos)((l, r) =>
        TypedExpr.BinaryCompare(l, r, TypedExpr.CompareOperation.Less))
      case LessEqual(left, right) => checkOrdered(left, right, c, pos)((l, r) =>
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
      case FloatLiter(v) => (SemFloat.satisfies(c)(using ctx, pos), TypedExpr.FloatLit(v))
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

  private def checkNumericBinaryOp(left: Expr, right: Expr, parentConstraint: Constraint, pos: PositionInfo)
                                  (makeTyped: (TypedExpr, TypedExpr) => TypedExpr)
                                  (using cts: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {
    val (_, leftTypedExpr) = checkExpr(left, Constraint.Either(SemInt, SemFloat))
    val rightConstraint = Constraint.Either(SemInt, SemFloat)
    val (_, rightTypedExpr) = checkExpr(right, rightConstraint)

    val finalTypedExpr = makeTyped(leftTypedExpr, rightTypedExpr)
    unifyTypes(leftTypedExpr, rightTypedExpr)(using cts, pos)
    val finalTyOpt = finalTypedExpr.ty.satisfies(parentConstraint)(using cts, pos)

    (finalTyOpt, finalTypedExpr)
  }

  /* Check comparison operators that allow int, float or char operands. */
  private def checkOrdered(left: Expr, right: Expr, parentConstraint: Constraint, pos: PositionInfo)
                          (makeTyped: (TypedExpr, TypedExpr) => TypedExpr)
                          (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {

    val (leftTyOpt, leftTypedExpr) = checkExpr(left, Constraint.UnConstraint)

    val rightConstraint = leftTyOpt match {
      case Some(SemInt)   => Constraint.Is(SemInt)
      case Some(SemChar)  => Constraint.Is(SemChar)
      case Some(SemFloat) => Constraint.Is(SemFloat)
      case _              => Constraint.UnConstraint
    }

    val (_, rightTypedExpr) = checkExpr(right, rightConstraint)

    val leftTy = leftTypedExpr.ty
    val rightTy = rightTypedExpr.ty

    def isOrderedType(t: SemanticType): Boolean =
      t == SemInt || t == SemChar || t == SemFloat || t == SemUnknown

    if (!isOrderedType(leftTy)) {
      ctx.addError(TypeMismatch("int, char or float", SemanticType.show(leftTy), pos))
    }

    if (!isOrderedType(rightTy)) {
      ctx.addError(TypeMismatch("int, char or float", SemanticType.show(rightTy), pos))
    }

    unifyTypes(leftTypedExpr, rightTypedExpr)(using ctx, pos)

    val finalTypedExpr = makeTyped(leftTypedExpr, rightTypedExpr)
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

  private def checkNumericUnaryOp(expr: Expr, parentConstraint: Constraint, pos: PositionInfo)
                                 (makeTyped: TypedExpr => TypedExpr)
                                 (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedExpr) = {
    val (_, typedExpr) = checkExpr(expr, Constraint.Either(SemInt, SemFloat))
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
