package wacc.frontend.typeCheck

import wacc.common.PositionInfo
import wacc.frontend.ast.*
import wacc.frontend.typeCheck.TypeCheckError.*
import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.typedAST.*
import TypeCheckerHelpers.*
import wacc.frontend.ast.LValue.*
import wacc.frontend.ast.RValue.*

object TypeCheckerLR {

  def checkRValue(rvalue: RValue, c: Constraint)
                 (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedRValue) = rvalue match {

    // An expression used as an rvalue
    case RExpr(expr) =>
      val (exprTyOpt, typedExpr) = TypeCheckerExpr.checkExpr(expr, c)
      (exprTyOpt, TypedRValue.ExprR(typedExpr))

    // Array literal values
    case RArrayLiter(elems) =>

      // Infer the expected element type
      val elemTy = c match {
        case Constraint.Is(SemArray(ty, 1)) => ty
        case Constraint.Is(SemArray(ty, n)) => SemArray(ty, n - 1)
        case Constraint.Is(SemString) => SemChar
        case _ => SemUnknown
      }

      // Type-check each element under the inferred element constraint
      val typedExprs = elems.map(TypeCheckerExpr.checkExpr(_, Constraint.Is(elemTy))._2)

      // Reconstruct the full array type from the element type
      val outerTy = constructArrayType(SemArray(elemTy, 1))

      (outerTy.satisfies(c)(using ctx, rvalue.positionInfo), TypedRValue.ArrayLit(typedExprs, outerTy))

    // newpair(fst, snd)
    case RNewPair(fst, snd) =>
      val (fstTyOpt, fstTypedExpr) = TypeCheckerExpr.checkExpr(fst, Constraint.UnConstraint)
      val (sndTyOpt, sndTypedExpr) = TypeCheckerExpr.checkExpr(snd, Constraint.UnConstraint)

      // Construct pair type from inferred component types
      val pairTy = SemPair(fstTyOpt.getOrElse(SemUnknown), sndTyOpt.getOrElse(SemUnknown))
      val finalTy = pairTy.satisfies(c)(using ctx, rvalue.positionInfo)

      (finalTy,
        TypedRValue.NewPair(
          fstTypedExpr,
          sndTypedExpr,
          fstTyOpt.getOrElse(SemUnknown),
          sndTyOpt.getOrElse(SemUnknown)
        )
      )

    // Pair-elem
    case RPairElem(pe) =>
      val (pairElemTyOpt, typedPairElem) = checkPairElem(pe, c)
      (pairElemTyOpt, TypedRValue.PairElemR(typedPairElem))

    // Function call
    case RCall(fn, args) =>
      val name = fn.ident
      val overloads = ctx.returnFuncOverloads(name)

      // Always type-check arguments once to recover as much type info as possible.
      val (argTypes, unconstrainedArgs) = args.map { arg =>
        val (argTypeOpt, argTypedExpr) = TypeCheckerExpr.checkExpr(arg, Constraint.UnConstraint)
        (argTypeOpt.getOrElse(SemUnknown), argTypedExpr)
      }.unzip

      if (overloads.isEmpty) then {
        val retTy = SemUnknown
        (retTy.satisfies(c)(using ctx, fn.positionInfo), TypedRValue.Call(name, name, unconstrainedArgs, retTy, argTypes))

      } else {
        val sameArity = overloads.filter(_.paramTypes.length == args.length)

        if (sameArity.isEmpty) {
          ctx.addError(FuncArityMismatch(name, overloads.map(_.paramTypes.length), args.length, fn.positionInfo))
          val retTy = SemUnknown
          (retTy.satisfies(c)(using ctx, fn.positionInfo), TypedRValue.Call(name, name, unconstrainedArgs, retTy, argTypes))

        } else {
          val matches = sameArity.filter { ov =>
            (argTypes zip ov.paramTypes).forall { case (argTy, paramTy) => compatible(argTy, paramTy) }
          }

          if (matches.isEmpty) {
            ctx.addError(
              FuncNoMatchingOverload(
                funcName = name,
                argTypes = argTypes,
                candidates = sameArity.map(ov => (ov.paramTypes, ov.returnType)),
                pos = fn.positionInfo
              )
            )
            val retTy = SemUnknown
            (retTy.satisfies(c)(using ctx, fn.positionInfo), TypedRValue.Call(name, name, unconstrainedArgs, retTy, argTypes))

          } else {
            val ranked = matches.map(ov => ov -> overloadScore(argTypes, ov.paramTypes))
            val bestScore = ranked.maxBy(_._2)._2
            val bestMatches = ranked.collect { case (ov, score) if score == bestScore => ov }

            if (bestMatches.length > 1) {
              ctx.addError(
                FuncAmbiguousOverload(
                  funcName = name,
                  argTypes = argTypes,
                  matches = bestMatches.map(ov => (ov.paramTypes, ov.returnType)),
                  pos = fn.positionInfo
                )
              )
              val retTy = SemUnknown
              (retTy.satisfies(c)(using ctx, fn.positionInfo), TypedRValue.Call(name, name, unconstrainedArgs, retTy, argTypes))

            } else {
              val chosen = bestMatches.head

              // Re-check arguments with the chosen overload to enforce expected param types.
              val (checkedArgTypes, checkedArgs) =
                (chosen.paramTypes zip args).map { (paramTy, arg) =>
                  val (argTypeOpt, argTypedExpr) = TypeCheckerExpr.checkExpr(arg, Constraint.Is(paramTy))
                  (argTypeOpt.getOrElse(SemUnknown), argTypedExpr)
                }.unzip

              (
                chosen.returnType.satisfies(c)(using ctx, fn.positionInfo),
                TypedRValue.Call(name, chosen.resolvedName, checkedArgs, chosen.returnType, checkedArgTypes)
              )
            }
          }
        }
      }
  }

  def checkLValue(lvalue: LValue, c: Constraint)
                 (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedLValue) = lvalue match {

    // A variable identifier
    case LIdent(ident) =>
      val ty = ctx.returnVariableType(ident.ident)
      (ty.satisfies(c, isId = true)(using ctx, lvalue.positionInfo), TypedLValue.Id(ident.ident, ty))

    // Array element with one or more index expressions.
    case LArray(arr @ Expr.ArrayElem(ident, indices)) =>

      // Type check the identifier
      val (_, typedId0) =
        checkLValue(LIdent(ident)(ident.positionInfo), Constraint.UnConstraint)

      val baseTy = typedId0 match {
        case id: TypedLValue.Id => id.semType
        case _ => SemUnknown
      }

      // Determine the element type
      val elemTy: SemanticType =
        baseTy match {
          case SemString =>
            ctx.addError(TypeMismatch("char[]", "string", arr.positionInfo))
            SemChar

          case _ =>
            if ident.ident == "out-of-scope" then SemUnknown
            else unwrapArrayType(baseTy, indices.length, arr.positionInfo)
        }

      // Each index expression must be int
      val typedExprs = indices.map { e =>
        val (_, te) = TypeCheckerExpr.checkExpr(e, Constraint.Is(SemInt))
        te
      }.toList

      val finalTyOpt = elemTy.satisfies(c)(using ctx, arr.positionInfo)

      (finalTyOpt, TypedLValue.ArrayElem(typedId0.asInstanceOf[TypedLValue.Id], typedExprs, elemTy))

    // Pair-elem
    case LPairElem(pe) => checkPairElem(pe, c)
  }

  def checkPairElem(pe: PairElem, c: Constraint)
                   (using ctx: TypeChecker.TypeCheckerCtx): (Option[SemanticType], TypedLValue) = pe match {

    // fst p
    case PFst(lv) =>
      val (fstTypeOpt, typedFst) = checkLValue(lv, Constraint.UnConstraint)
      val fstTy = fstTypeOpt match {
        case Some(SemPair(fst, _)) => fst
        case Some(SemPairErased) => SemUnknown
        case Some(SemUnknown) => SemUnknown
        case Some(nonPair) =>
          ctx.addError(TypeMismatch("pair", SemanticType.show(nonPair), pe.positionInfo))
          SemUnknown
        case None =>
          ctx.addError(TypeMismatch("pair", "unknown", pe.positionInfo))
          SemUnknown
      }
      (fstTy.satisfies(c)(using ctx, pe.positionInfo), TypedLValue.PairElemFst(typedFst, fstTy))

    // snd p
    case PSnd(lv) =>
      val (sndTypeOpt, typedSnd) = checkLValue(lv, Constraint.UnConstraint)
      val sndTy = sndTypeOpt match {
        case Some(SemPair(_, snd)) => snd
        case Some(SemPairErased) => SemUnknown
        case Some(SemUnknown) => SemUnknown
        case Some(nonPair) =>
          ctx.addError(TypeMismatch("pair", SemanticType.show(nonPair), pe.positionInfo))
          SemUnknown
        case None =>
          ctx.addError(TypeMismatch("pair", "unknown", pe.positionInfo))
          SemUnknown
      }
      (sndTy.satisfies(c)(using ctx, pe.positionInfo), TypedLValue.PairElemSnd(typedSnd, sndTy))
  }

  // Merge nested array types into a single dimension count
  private def constructArrayType(semType: SemanticType): SemanticType = semType match {
    case SemArray(SemArray(ty, idxIn), idxOut) => SemArray(ty, idxIn + idxOut)
    case ty => ty
  }

  // Remove `arity` levels of array indexing from a base type
  private def unwrapArrayType(baseTy: SemanticType, arity: Int, pos: PositionInfo)
                             (using ctx: TypeChecker.TypeCheckerCtx): SemanticType = baseTy match {
    case SemArray(elem, idx) if idx > arity => SemArray(elem, idx - arity)
    case SemArray(elem, idx) if idx == arity => elem
    case SemArray(_, idx) if idx < arity =>
      ctx.addError(TypeMismatch(s"array with at least $arity dimensions", SemanticType.show(baseTy), pos))
      SemUnknown
    case nonArray =>
      ctx.addError(TypeMismatch("array", SemanticType.show(nonArray), pos))
      SemUnknown
  }
  private def overloadScore(argTypes: List[SemanticType], paramTypes: List[SemanticType]): (Int, Int) = {
    val exactMatches = (argTypes zip paramTypes).count { case (argTy, paramTy) => argTy == paramTy }
    val weakMatches = (argTypes zip paramTypes).count {
      case (argTy, paramTy) => argTy != paramTy && SemanticType.canWeaken(argTy, paramTy)
    }
    // Prefer more exact matches, then fewer weakenings.
    (exactMatches, -weakMatches)
  }
}


