package wacc.frontend.typeCheck

import wacc.common.PositionInfo
import wacc.frontend.typeCheck.TypeCheckError.*
import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.typedAST.*

// A constraint describes what semantic type it is expected to have
sealed trait Constraint
object Constraint {
  // It must have exactly the same semantic type
  final case class Is(ty: SemanticType) extends Constraint

  // It must be one of the two possible semantic types
  final case class Either(ty1: SemanticType, ty2: SemanticType) extends Constraint

  // No constraint
  case object UnConstraint extends Constraint
}

object TypeCheckerHelpers {

  val anyDimension: Int = -1 // Special marker for "any array dimension"

  // Report a type mismatch error using semantic type names
  private def mismatch(expected: SemanticType, actual: SemanticType, pos: PositionInfo)
                      (using ctx: TypeChecker.TypeCheckerCtx): Unit = {
    ctx.addError(TypeMismatch(SemanticType.show(expected), SemanticType.show(actual), pos))
  }

  private def mismatchStr(expected: String, actual: SemanticType, pos: PositionInfo)
                         (using ctx: TypeChecker.TypeCheckerCtx): Unit = {
    ctx.addError(TypeMismatch(expected, SemanticType.show(actual), pos))
  }

  // Extension method: check whether a semantic type satisfies a constraint
  extension (ty: SemanticType)
    def satisfies(c: Constraint, isId: Boolean = false)
                 (using ctx: TypeChecker.TypeCheckerCtx, pos: PositionInfo): Option[SemanticType] =
      (ty, c) match {

        case (_, Constraint.UnConstraint) => Some(ty)
        case (SemUnknown, Constraint.Is(refTy)) => Some(refTy)
        case (SemUnknown, _) => Some(SemUnknown)
        case (_, Constraint.Is(SemUnknown)) => Some(ty)

        // array assignment
        case (arr1 @ SemArray(t1, n1), Constraint.Is(arr2 @ SemArray(t2, n2))) =>
          if n1 >= n2 && compatible(t1, t2) then Some(arr1)
          else {
            mismatch(arr2, arr1, pos)
            Some(SemUnknown)
          }

        // pair element erased rules
        case (sp @ SemPair(SemPair(_, _), _),
        Constraint.Is(SemPair(SemPair(SemUnknown, SemUnknown), _))) => Some(sp)

        case (sp @ SemPair(_, SemPair(_, _)),
        Constraint.Is(SemPair(_, SemPair(SemUnknown, SemUnknown)))) => Some(sp)

        // pair compatibility
        case (sp @ SemPair(_, _), Constraint.Is(refTy @ SemPair(_, _))) =>
          if !isId then
            if compatible(sp, refTy) then Some(refTy)
            else {
              mismatch(refTy, sp, pos)
              Some(SemUnknown)
            }
          else
            if sp == refTy then Some(refTy)
            else {
              mismatch(refTy, sp, pos)
              Some(SemUnknown)
            }

        case (ty0, Constraint.Is(refTy)) =>
          if compatible(ty0, refTy) then Some(ty0)
          else {
            mismatch(refTy, ty0, pos)
            Some(SemUnknown)
          }

        // Either(array, pair) — actual is array
        case (sa1 @ SemArray(_, _), Constraint.Either(sa2 @ SemArray(_, _), SemPair(_, _))) =>
          if compatible(sa1, sa2) then Some(sa1)
          else {
            mismatch(sa2, sa1, pos)
            Some(SemUnknown)
          }

        // Either(array, pair) — actual is pair
        case (sp1 @ SemPair(_, _), Constraint.Either(SemArray(_, _), sp2 @ SemPair(_, _))) =>
          if compatible(sp1, sp2) then Some(sp1)
          else {
            mismatch(sp2, sp1, pos)
            Some(SemUnknown)
          }

        case (ty0, Constraint.Either(refTy1, refTy2)) =>
          if compatible(ty0, refTy1) || compatible(ty0, refTy2) then Some(ty0)
          else {
            val expected = SemanticType.show(refTy1) + " or " + SemanticType.show(refTy2)
            mismatchStr(expected, ty0, pos)
            Some(SemUnknown)
          }
      }

  // Unify two typed expressions after they are checked
  def unifyTypes(left: TypedExpr, right: TypedExpr)
                (using ctx: TypeChecker.TypeCheckerCtx, pos: PositionInfo): Unit =
    (left.ty, right.ty) match {
      case (SemUnknown, SemUnknown) =>
        ctx.addError(CannotInferType(pos))
      case (SemUnknown, ty) =>
        left.ty = ty
      case (ty, SemUnknown) =>
        right.ty = ty
      case (_, _) => ()
    }
}
