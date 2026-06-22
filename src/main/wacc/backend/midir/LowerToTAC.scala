package wacc.backend.midir

import wacc.backend.midir.TAC.*
import wacc.frontend.typedAST.*
import wacc.backend.label.{Label, LabelGen}
import wacc.backend.midir.LowerToTacConstant.*
import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.typedAST.TypedException.*
import wacc.frontend.ast.WACCExceptionType
import wacc.backend.midir.StringContext
import wacc.backend.midir.FuncContext
import wacc.backend.BackendCommon.BackendConstant.*

private final case class LoopTarget(breakLabel: Label, continueLabel: Label)

object LowerToTAC {

  def fromTypedProgram(p: TypedProgram, wordSize: Int): TACProgram = {
    given ctx: StringContext = new StringContext(wordSize)
    given lg: LabelGen = new LabelGen()
    given mainCtx: FuncContext = new FuncContext(isMain = true, returnType = None)

    val funcs = p.funcs.map(lowerFunction)

    // Main starts with a clean exception state so runtime checks during statement lowering can rely on it.
    mainCtx.emit(TACClearException())
    lowerStmts(p.stmts)

    TACProgram(ctx.strings, funcs, mainCtx.instrs, mainCtx.locals)
  }

  private def lowerFunction(func: TypedFunction)(using progCtx: StringContext, lg: LabelGen): TACFunction = {
    given ftx: FuncContext = new FuncContext(isMain = false, returnType = Some(func.returnType))

    val paramTemps: List[Temp] =
      func.params.map { case TypedLValue.Id(name, ty) =>
        ftx.declare(name, sizeof(ty))
      }

    lowerStmts(func.stmts)
    val body0 = ftx.instrs

    val body =
      body0.lastOption match {
        case Some(_: TACReturn) => body0
        // Keep TAC functions total even if the source missed an explicit final return on some path.
        case _ => body0 :+ TACReturn(ftx.defaultReturnValue)
      }

    TACFunction(Label(func.resolvedIdent), paramTemps, ftx.locals, body)
  }

  private def lowerStmts(stmts: List[TypedStmt])(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit =
    // Statement lowering is purely append-only: each helper emits into the current function context.
    stmts.foreach(lowerStmt)

  private def lowerStmt(s: TypedStmt)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = s match {
    case TypedStmt.Skip() => ftx.emit(TACSkip())

    case TypedStmt.BeginEnd(inner) => lowerStmts(inner)

    case TypedStmt.Decl(ident, value) =>
      // Declarations reserve storage first so recursive RHS lowering can refer to already-allocated temps if needed later.
      val temp = ftx.declare(ident.name, sizeof(ident.ty))
      val rhs = lowerExpr(value)
      ftx.emit(TACAssign(temp, rhs))

    case TypedStmt.Assign(left, right) =>
      left match {
        case a @ TypedLValue.ArrayElem(_, _, _) =>
          // Array stores need the base pointer and final index separately so bounds checks happen before the write.
          val (basePtr, lastIdxRhs, elemLen) = lowerArrayElemForStore(a)
          val rhsVal = lowerExpr(right)
          val lastIdx = idxAsOffset(lastIdxRhs)
          ftx.emit(TACAssign(IndirectTemp(basePtr, lastIdx, isArray = true, len = elemLen), rhsVal))

        case _ =>
          val lhs = findLhs(left)
          val rhs = lowerExpr(right)
          ftx.emit(TACAssign(lhs, rhs))
      }

    case TypedStmt.Read(lv) =>
      // Read lowers directly into an l-value slot because the runtime helper writes into an addressable destination.
      val lhs = findLhs(lv)
      val rhs = lv.ty match {
        case SemInt => TACRead(lhs, ReadType.Int)
        case SemChar => TACRead(lhs, ReadType.Char)
        case SemFloat => TACRead(lhs, ReadType.Float)
        case _ => throw new Exception(ERR_INVALID_READ_TYPE)
      }
      ftx.emit(rhs)

    case TypedStmt.Free(expr) =>
      expr match {
        case TypedLValue.Id(name, ty) =>
          val t = ftx.lookup(name)
          val isPair: Boolean = ty match {
            case SemPair(_, _) | SemPairErased => true
            case SemArray(_, _) => false
            case SemString => throw new Exception(ERR_FREE_STRING_ILLEGAL)
            case other => throw new Exception(s"$ERR_FREE_UNSUPPORTED_PREFIX $other")
          }
          // Pairs require a null check because freeing a null/freed pair is a runtime error in WACC.
          if (isPair) emitNullDereferenceIfNeeded(t)
          ftx.emit(TACFree(t, isPair))

        case other =>
          throw new UnsupportedOperationException(s"$ERR_FREE_EXPR_UNIMPLEMENTED_PREFIX $other")
      }

    case TypedStmt.Return(expr) =>
      val rhs = lowerExpr(expr)
      ftx.emit(TACReturn(rhs))

    case TypedStmt.Throw(e) => lowerTypedException(e)

    case TypedStmt.Exit(expr) =>
      val v = lowerExpr(expr)
      ftx.emit(TACExit(v))

    case TypedStmt.IfElse(cond, thn, els) => lowerIfElse(cond, thn, els)

    case TypedStmt.If(cond, thn) => lowerIf(cond, thn)

    case TypedStmt.Switch(selector, cases) => lowerSwitch(selector, cases)

    case TypedStmt.While(cond, body) => lowerWhile(cond, body)

    case TypedStmt.TryCatch(tryBody, handlers) => {
      val catchLabel = lg.fresh(CATCH_DISPATCH)
      val endLabel = lg.fresh(TRY_END)

      // The active handler is a single entry label; individual catches dispatch from there by exception id.
      // Push the entry point for the catch logic
      ftx.pushExceptionHandler(catchLabel)
      lowerStmts(tryBody)
      ftx.popExceptionHandler()

      // If try completes without exception, skip handlers
      ftx.emit(Jmp(endLabel))

      ftx.emit(Mark(catchLabel))

      // Load the pointer to our [ID, Msg] object
      val exPtr = ftx.fresh(BitLength._ptr)
      ftx.emit(TACLoadExceptionValue(exPtr))

      // Extract the ID (offset 0) for the 'targetId' comparison
      val runtimeExId = ftx.fresh(BitLength._32)
      ftx.emit(TACAssign(runtimeExId, IndirectTemp(exPtr, ImmValue(0), isArray = false, len = BitLength._32)))

      handlers.foreach { h => lowerCatchHandler(h, runtimeExId, exPtr, endLabel) }

      lowerUnhandledException(exPtr)
      ftx.emit(Mark(endLabel))
    }

    case TypedStmt.For(init, cond, update, body) => lowerFor(init, cond, update, body)

    case TypedStmt.DoWhile(body, cond) => lowerDoWhile(body, cond)

    case TypedStmt.Break() =>
      val target = ftx.currentLoop.getOrElse(throw new Exception(ERR_BREAK_OUTSIDE_LOOP))
      ftx.emit(Jmp(target.breakLabel))

    case TypedStmt.Continue() =>
      val target = ftx.currentLoop.getOrElse(throw new Exception(ERR_CONTINUE_OUTSIDE_LOOP))
      ftx.emit(Jmp(target.continueLabel))

    case TypedStmt.Print(expr) =>
      val rhs = lowerExpr(expr)
      val printInstr = expr.ty match {
        // Non-char arrays and pairs print as addresses because the runtime has no structural pretty-printer for them.
        case SemInt => PrintInt(rhs)
        case SemBool => PrintBool(rhs)
        case SemChar => PrintChar(rhs)
        case SemFloat => PrintFloat(rhs)
        case SemString => PrintStr(rhs)
        case SemArray(SemChar, _) => PrintStr(rhs)
        case SemArray(_, _) => PrintPointer(rhs)
        case SemPair(_, _) => PrintPointer(rhs)
        case SemPairErased => PrintPointer(rhs)
        case SemUnknown => throw new Exception(ERR_FRONTEND_UNKNOWN_TYPE)
        case SemException => throw new Exception(ERR_PRINT_EXCEPTION)
      }
      ftx.emit(printInstr)

    case TypedStmt.Println(expr) =>
      lowerStmt(TypedStmt.Print(expr))
      ftx.emit(PrintLn())
  }

  private def lowerCatchHandler(handler: TypedCatchHandler, runtimeExId: Temp, exPtr: Temp, endLabel: Label)
                               (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val nextHandler = lg.fresh(NEXT_HANDLER)
    val handlerMatch = lg.fresh(HANDLER_MATCH)

    val targetId = getExceptionId(handler.exType)
    // Each handler is compiled as a guarded block in a linear dispatch chain.
    ftx.emit(CmpJmp(CondOp.EQ, runtimeExId, ImmValue(targetId), handlerMatch))
    ftx.emit(Jmp(nextHandler))

    ftx.emit(Mark(handlerMatch))

    // Extract the Message String (offset wordSize) to populate 'e'
    val caughtMsgTemp = ftx.declare(handler.exName.name, BitLength._ptr)
    ftx.emit(TACAssign(caughtMsgTemp, IndirectTemp(exPtr, ImmValue(ctx.wordSize), isArray = false, len = BitLength._ptr)))

    // Entering a handler consumes the pending exception so nested throws start from a clean state.
    ftx.emit(TACClearException())
    lowerStmts(handler.body)
    ftx.emit(Jmp(endLabel))

    ftx.emit(Mark(nextHandler))
  }

  private def getExceptionId(ex: WACCExceptionType): Int = ex match {
    case wacc.frontend.ast.Arithmetic() => ARITHMETIC_EXCEPTION
    case wacc.frontend.ast.BadChar() => BADCHAR_EXCEPTION
    case wacc.frontend.ast.ArrayOutOfBounds() => ARRAY_OUT_OF_BOUNDS_EXCEPTION
    case wacc.frontend.ast.IntegerOverflow() => INTEGER_OVERFLOW_EXCEPTION
    case wacc.frontend.ast.NullDereference() => NULL_DEREFERENCE_EXCEPTION
    case wacc.frontend.ast.General() => GENERAL_EXCEPTION
  }

  private def lowerUnhandledException(currentValue: Rhs)(using ftx: FuncContext): Unit =
    ftx.currentExceptionHandler match {
      // Re-throw to the innermost active try first; only top-level code reports and exits directly.
      case Some(handler) => ftx.emit(Jmp(handler))
      case None if ftx.isMain =>
        ftx.emit(TACReportError(currentValue))
        ftx.emit(TACExit(ImmValue(RUNTIME_ERROR_EXIT_CODE)))
      case None => ftx.emit(TACReturn(ftx.defaultReturnValue))
    }

  private def lowerExceptionAfterCall()(using ftx: FuncContext, lg: LabelGen): Unit = {
    val flag = ftx.fresh(BitLength._32)
    val noException = lg.fresh(NO_EXCEPTION)

    // Calls communicate exceptional completion through the global exception slot rather than the return temp.
    ftx.emit(TACLoadExceptionFlag(flag))
    ftx.emit(CmpJmp(CondOp.EQ, flag, ImmValue(0), noException))

    ftx.currentExceptionHandler match {
      case Some(handler) => ftx.emit(Jmp(handler))
      case None if ftx.isMain =>
        val exValue = ftx.fresh(BitLength._ptr)
        ftx.emit(TACLoadExceptionValue(exValue))
        ftx.emit(TACReportError(exValue))
        ftx.emit(TACExit(ImmValue(RUNTIME_ERROR_EXIT_CODE)))

      case None => ftx.emit(TACReturn(ftx.defaultReturnValue))
    }

    ftx.emit(Mark(noException))
  }

  private def lowerCall(resolvedIdent: String, args: List[TypedExpr], retTy: SemanticType)
                       (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Temp = {
    // Arguments are lowered left-to-right before the call instruction is emitted.
    val argVals = args.map(lowerExpr)
    val dst = ftx.fresh(sizeof(retTy))
    ftx.emit(TACCall(dst, Label(resolvedIdent), argVals))
    lowerExceptionAfterCall()
    dst
  }

  private def lowerUnaryOp(operand: TypedExpr, ty: SemanticType, op: UnaryOp)
                          (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val ov = lowerExpr(operand)
    val dst = ftx.fresh(sizeof(ty))
    ftx.emit(UnOp(dst, op, ov))
    dst
  }

  private def lowerBinaryOp(left: TypedExpr, right: TypedExpr, ty: SemanticType, op: BinaryOp)
                           (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    // Generic binary lowering covers arithmetic, comparisons, and bitwise ops once checks are handled elsewhere.
    val lv = lowerExpr(left)
    val rv = lowerExpr(right)
    val dst = ftx.fresh(sizeof(ty))
    ftx.emit(BinOp(dst, op, lv, rv))
    dst
  }

  private def lowerAsFloat(expr: TypedExpr)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val value = lowerExpr(expr)
    expr.ty match {
      case SemFloat => value
      case SemInt =>
        val converted = ftx.fresh(BitLength._32)
        ftx.emit(IntToFloat(converted, value))
        converted
      case other => throw new Exception(s"cannot convert ${SemanticType.show(other)} to float")
    }
  }

  private def lowerFloatBinaryOp(left: TypedExpr, right: TypedExpr, op: FloatArithOp)
                                (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val lv = lowerAsFloat(left)
    val rv = lowerAsFloat(right)
    val dst = ftx.fresh(BitLength._32)
    ftx.emit(BinOp(dst, op, lv, rv))
    dst
  }

  private def lowerShortCircuitBool(left: TypedExpr, right: TypedExpr, op: TypedExpr.BoolOperation)
                                   (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val dst = ftx.fresh(BitLength._8)
    val lv = lowerExpr(left)

    // The RHS is only lowered into the fallthrough block; the branch path materializes the shortcut result directly.
    op match {
      case TypedExpr.BoolOperation.And =>
        lowerShortCircuitBranch(dst, lv, right, CondOp.EQ, 0, SC_LABEL_PREFIX_AND)
      case TypedExpr.BoolOperation.Or =>
        lowerShortCircuitBranch(dst, lv, right, CondOp.NEQ, 1, SC_LABEL_PREFIX_OR)
    }
  }

  private def lowerExpr(e: TypedExpr)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = e match {
    case TypedExpr.IntLit(v) => ImmValue(v.toLong)
    case TypedExpr.FloatLit(v) => FloatValue(v)
    case TypedExpr.BoolLit(value) => if value then ImmValue(1, BitLength._8) else ImmValue(0, BitLength._8)
    case TypedExpr.CharLit(value) => ImmValue(value.toLong, BitLength._8)
    case TypedExpr.StrLit(value) => ctx.getString(value)
    case TypedExpr.PairLit() => ImmValue(0, BitLength._ptr)

    // Arithmetic only uses checked variants when an enclosing try-catch can observe the thrown runtime exception.
    case neg @ TypedExpr.Neg(operand) =>
      if neg.ty == SemFloat then lowerBinaryOp(TypedExpr.FloatLit(0.0f), operand, SemFloat, FloatArithOp.Sub)
      else if ftx.currentExceptionHandler.isDefined then lowerCheckedNeg(operand, neg.ty)
      else lowerUnaryOp(operand, neg.ty, UnaryOp.Neg)
    case not @ TypedExpr.Not(operand) => lowerUnaryOp(operand, not.ty, UnaryOp.Not)
    case len @ TypedExpr.Len(operand) => lowerUnaryOp(operand, len.ty, UnaryOp.Len)
    case ord @ TypedExpr.Ord(operand) => lowerUnaryOp(operand, ord.ty, UnaryOp.Ord)
    case chr @ TypedExpr.Chr(operand) => lowerCheckedChr(operand, chr.ty)
    case bitNot @ TypedExpr.BitNot(operand) => lowerUnaryOp(operand, bitNot.ty, UnaryOp.BitNot)

    case ba @ TypedExpr.BinaryArithmetic(left, right, op) =>
      if ba.ty == SemFloat then {
        val floatOp = op match {
          case TypedExpr.ArithmeticOperation.Add => FloatArithOp.Add
          case TypedExpr.ArithmeticOperation.Sub => FloatArithOp.Sub
          case TypedExpr.ArithmeticOperation.Mul => FloatArithOp.Mul
          case TypedExpr.ArithmeticOperation.Div => FloatArithOp.Div
          case TypedExpr.ArithmeticOperation.Mod => throw new Exception("float modulo is not supported")
        }
        lowerFloatBinaryOp(left, right, floatOp)
      } else op match {
        case TypedExpr.ArithmeticOperation.Add => if ftx.currentExceptionHandler.isDefined then lowerCheckedArithmetic(left, right, ba.ty, ArithOp.Add) else lowerBinaryOp(left, right, ba.ty, ArithOp.Add)
        case TypedExpr.ArithmeticOperation.Sub => if ftx.currentExceptionHandler.isDefined then lowerCheckedArithmetic(left, right, ba.ty, ArithOp.Sub) else lowerBinaryOp(left, right, ba.ty, ArithOp.Sub)
        case TypedExpr.ArithmeticOperation.Mul => if ftx.currentExceptionHandler.isDefined then lowerCheckedArithmetic(left, right, ba.ty, ArithOp.Mul) else lowerBinaryOp(left, right, ba.ty, ArithOp.Mul)
        case TypedExpr.ArithmeticOperation.Div => lowerCheckedDivision(left, right, ba.ty, ArithOp.Div)
        case TypedExpr.ArithmeticOperation.Mod => lowerCheckedDivision(left, right, ba.ty, ArithOp.Mod)
      }

    case bc @ TypedExpr.BinaryCompare(left, right, op) =>
      val comparisonOp: BinaryOp = if left.ty == SemFloat then op match {
        case TypedExpr.CompareOperation.Less => FloatCondOp.LT
        case TypedExpr.CompareOperation.LessEqual => FloatCondOp.LEQ
        case TypedExpr.CompareOperation.Greater => FloatCondOp.GT
        case TypedExpr.CompareOperation.GreaterEqual => FloatCondOp.GEQ
        case TypedExpr.CompareOperation.Equal => FloatCondOp.EQ
        case TypedExpr.CompareOperation.NotEqual => FloatCondOp.NEQ
      } else op match {
        case TypedExpr.CompareOperation.Less => CondOp.LT
        case TypedExpr.CompareOperation.LessEqual => CondOp.LEQ
        case TypedExpr.CompareOperation.Greater => CondOp.GT
        case TypedExpr.CompareOperation.GreaterEqual => CondOp.GEQ
        case TypedExpr.CompareOperation.Equal => CondOp.EQ
        case TypedExpr.CompareOperation.NotEqual => CondOp.NEQ
      }
      lowerBinaryOp(left, right, bc.ty, comparisonOp)

    case bb @ TypedExpr.BinaryBitwise(left, right, op) => op match {
      case TypedExpr.BitwiseAndOr.BitAnd => lowerBinaryOp(left, right, bb.ty, BitwiseOp.BitAnd)
      case TypedExpr.BitwiseAndOr.BitOr => lowerBinaryOp(left, right, bb.ty, BitwiseOp.BitOr)
    }

    case TypedExpr.BinaryBool(left, right, op) => lowerShortCircuitBool(left, right, op)

    case TypedLValue.Id(name, _) => ftx.lookup(name)

    case a @ TypedLValue.ArrayElem(_, _, _) => lowerArrayElemAsRhs(a)

    case TypedRValue.ArrayLit(exprs, semTy) =>
      // Array literals lower to a heap object whose element width is derived from the remaining semantic nesting level.
      val loweredExprs = exprs.map(lowerExpr)
      val elemTy = semTy match {
        case SemArray(ty, 1) => ty
        case SemArray(ty, n) => SemArray(ty, n - 1)
        case _ => throw new Exception(ERR_REQUIRE_ARRAY)
      }
      Array(loweredExprs, sizeof(elemTy))

    case p @ TypedLValue.PairElemFst(_, _) => findLhs(p)
    case p @ TypedLValue.PairElemSnd(_, _) => findLhs(p)
    case TypedRValue.ExprR(expr) => lowerExpr(expr)
    case TypedRValue.PairElemR(pe) => findLhs(pe)
    case TypedRValue.NewPair(fst, snd, _, _) => Pair(lowerExpr(fst), lowerExpr(snd))
    case TypedRValue.Call(_, resolvedIdent, args, retTy, _) => lowerCall(resolvedIdent, args, retTy)
  }

  private def lowerTypedException(e: TypedException)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = e match {
    case ArithmeticEx(msg) => lowerException(msg, ARITHMETIC_EXCEPTION)
    case BadCharEx(msg) => lowerException(msg, BADCHAR_EXCEPTION)
    case ArrayOutOfBoundsEx(msg) => lowerException(msg, ARRAY_OUT_OF_BOUNDS_EXCEPTION)
    case IntegerOverflowEx(msg) => lowerException(msg, INTEGER_OVERFLOW_EXCEPTION)
    case NullDereferenceEx(msg) => lowerException(msg, NULL_DEREFERENCE_EXCEPTION)
    case GeneralEx(msg) => lowerException(msg, GENERAL_EXCEPTION)
  }

  // Helper function to lower an exception expression
  private def lowerException(msg: TypedExpr, id: Int)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val msgVal = lowerExpr(msg)

    // Pack [ID, Msg] into a pair-like structure on the heap
    // This uses backend logic for creating pairs
    val exObject = ftx.fresh(BitLength._ptr)
    ftx.emit(TACAssign(exObject, Pair(ImmValue(id.toLong), msgVal)))

    // TACStoreException stores a pointer
    ftx.emit(TACStoreException(exObject))
    lowerUnhandledException(exObject)
    exObject
  }


  private def lowerImplicitException(msg: String, id: Int)(using ftx: FuncContext, ctx: StringContext): Unit = {
    // Implicit exceptions share the same heap layout as explicit `throw` expressions.
    val exObject = ftx.fresh(BitLength._ptr)
    ftx.emit(TACAssign(exObject, Pair(ImmValue(id.toLong), ctx.getString(msg))))
    ftx.emit(TACStoreException(exObject))
    lowerUnhandledException(exObject)
  }

  private def lowerCheck(okCond: CondOp, lhs: Rhs, rhs: Rhs, exId: Int, msg: String)
                        (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val okLabel = lg.fresh(RUNTIME_CHECK_OK)
    // Runtime checks are lowered as guard branches that synthesize and throw an implicit exception on failure.
    ftx.emit(CmpJmp(okCond, lhs, rhs, okLabel))
    lowerImplicitException(msg, exId)
    ftx.emit(Mark(okLabel))
  }

  private def emitNullDereferenceIfNeeded(ptr: Rhs)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit =
    lowerCheck(CondOp.NEQ, ptr, ImmValue(0, BitLength._ptr), NULL_DEREFERENCE_EXCEPTION, ERR_NULL_DEREFERENCE_RUNTIME)

  private def emitArrayBoundsChecks(base: Temp, index: Rhs)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    emitNullDereferenceIfNeeded(base)
    // Array length is stored in the word immediately before the first element payload.
    val len = ftx.fresh(BitLength._32)
    ftx.emit(TACAssign(len, IndirectTemp(base, ImmValue(-4), isArray = false, len = BitLength._32)))
    lowerCheck(CondOp.GEQ, index, ImmValue(0), ARRAY_OUT_OF_BOUNDS_EXCEPTION, ERR_ARRAY_INDEX_OUT_OF_BOUNDS)
    lowerCheck(CondOp.LT, index, len, ARRAY_OUT_OF_BOUNDS_EXCEPTION, ERR_ARRAY_INDEX_OUT_OF_BOUNDS)
  }

  private def lowerCheckedDivision(left: TypedExpr, right: TypedExpr, ty: SemanticType, op: ArithOp)
                                  (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val lv = lowerExpr(left)
    val rv = lowerExpr(right)
    // Division and modulo always guard against zero, regardless of whether the caller installs a catch handler.
    lowerCheck(CondOp.NEQ, rv, ImmValue(0), ARITHMETIC_EXCEPTION, ERR_DIVIDE_BY_ZERO)
    val dst = ftx.fresh(sizeof(ty))
    ftx.emit(BinOp(dst, op, lv, rv))
    dst
  }

  private def lowerCheckedArithmetic(left: TypedExpr, right: TypedExpr, ty: SemanticType, op: ArithOp)
                                    (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val lv = lowerExpr(left)
    val rv = lowerExpr(right)
    val dst = ftx.fresh(sizeof(ty))
    val overflowLabel = lg.fresh(RUNTIME_CHECK_OVERFLOW)
    val endLabel = lg.fresh(RUNTIME_CHECK_END)
    // Checked ops branch to a synthesized exception path instead of encoding overflow in the result temp.
    ftx.emit(CheckedBinOp(dst, op, lv, rv, overflowLabel))
    ftx.emit(Jmp(endLabel))
    ftx.emit(Mark(overflowLabel))
    lowerImplicitException(ERR_INTEGER_OVERFLOW, INTEGER_OVERFLOW_EXCEPTION)
    ftx.emit(Mark(endLabel))
    dst
  }

  private def lowerCheckedNeg(operand: TypedExpr, ty: SemanticType)
                             (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val ov = lowerExpr(operand)
    val dst = ftx.fresh(sizeof(ty))
    val overflowLabel = lg.fresh(RUNTIME_CHECK_OVERFLOW)
    val endLabel = lg.fresh(RUNTIME_CHECK_END)
    ftx.emit(CheckedUnOp(dst, UnaryOp.Neg, ov, overflowLabel))
    ftx.emit(Jmp(endLabel))
    ftx.emit(Mark(overflowLabel))
    lowerImplicitException(ERR_INTEGER_OVERFLOW, INTEGER_OVERFLOW_EXCEPTION)
    ftx.emit(Mark(endLabel))
    dst
  }

  private def lowerCheckedChr(operand: TypedExpr, ty: SemanticType)
                             (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Rhs = {
    val ov = lowerExpr(operand)
    // `chr` lowers as two explicit range checks before the actual narrowing conversion.
    lowerCheck(CondOp.GEQ, ov, ImmValue(ASCII_MIN), BADCHAR_EXCEPTION, ERR_BAD_CHAR_RANGE)
    lowerCheck(CondOp.LEQ, ov, ImmValue(ASCII_MAX), BADCHAR_EXCEPTION, ERR_BAD_CHAR_RANGE)
    val dst = ftx.fresh(sizeof(ty))
    ftx.emit(UnOp(dst, UnaryOp.Chr, ov))
    dst
  }

  private def lowerIf(cond: TypedExpr, thenBranch: List[TypedStmt])
                     (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val lEnd = lg.fresh(IF_LABEL_END)
    lowerCondJumpFalse(cond, lEnd)
    lowerStmts(thenBranch)
    ftx.emit(Mark(lEnd))
  }

  private def lowerIfElse(cond: TypedExpr, thenBranch: List[TypedStmt], elseBranch: List[TypedStmt])
                         (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val lElse = lg.fresh(IF_LABEL_ELSE)
    val lEnd = lg.fresh(IF_LABEL_END)

    // `lowerCondBodyJump` emits the then-branch inline and jumps around the else-branch on success.
    lowerCondBodyJump(cond, thenBranch, lElse, lEnd)
    lowerStmts(elseBranch)
    ftx.emit(Mark(lEnd))
  }

  private def lowerSwitch(selector: TypedExpr, cases: List[TypedSwitchCaseBody])
                         (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val selectorValue = lowerExpr(selector)
    val endLabel = lg.fresh(SWITCH_END)
    val caseLabels = cases.map(_ => lg.fresh(SWITCH_CASE))

    val defaultTarget = cases.zip(caseLabels).collectFirst {
      case (switchCase, target)
          if switchCase.labels.exists {
            case TypedSwitchLabel.TypedDefaultLabel() => true
            case _ => false
          } => target
    }

    // Dispatch is emitted before every body so a match cannot accidentally execute
    // label expressions belonging to a later case.
    cases.zip(caseLabels).foreach { (switchCase, target) =>
      switchCase.labels.foreach {
        case TypedSwitchLabel.TypedCaseLabel(value) =>
          val caseValue = lowerExpr(value)
          if selector.ty == SemFloat then {
            val matches = ftx.fresh(BitLength._8)
            ftx.emit(BinOp(matches, FloatCondOp.EQ, selectorValue, caseValue))
            ftx.emit(CmpJmp(CondOp.NEQ, matches, ImmValue(0, BitLength._8), target))
          } else {
            ftx.emit(CmpJmp(CondOp.EQ, selectorValue, caseValue, target))
          }
        case TypedSwitchLabel.TypedDefaultLabel() => ()
      }
    }

    ftx.emit(Jmp(defaultTarget.getOrElse(endLabel)))

    cases.zip(caseLabels).foreach { (switchCase, target) =>
      ftx.emit(Mark(target))
      lowerStmts(switchCase.body)
      ftx.emit(Jmp(endLabel))
    }
    ftx.emit(Mark(endLabel))
  }

  private def lowerCondJumpFalse(cond: TypedExpr, elseLabel: Label)
                                (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val v = lowerExpr(cond)
    // Conditions are normalized to 8-bit booleans, so false is always the literal zero byte.
    ftx.emit(CmpJmp(CondOp.EQ, v, ImmValue(0, BitLength._8), elseLabel))
  }

  private def lowerCondJumpTrue(cond: TypedExpr, targetLabel: Label)
                               (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val v = lowerExpr(cond)
    ftx.emit(CmpJmp(CondOp.NEQ, v, ImmValue(0, BitLength._8), targetLabel))
  }

  private def lowerWhile(cond: TypedExpr, body: List[TypedStmt])
                        (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val lHead = lg.fresh(WHILE_LABEL_HEAD)
    val lEnd = lg.fresh(WHILE_LABEL_END)

    // While loops reuse the head label as the `continue` target so the condition is re-evaluated before each iteration.
    ftx.pushLoop(LoopTarget(lEnd, lHead))
    try {
      ftx.emit(Mark(lHead))
      lowerCondBodyJump(cond, body, lEnd, lHead)
    } finally {
      ftx.popLoop()
    }
  }

  private def lowerFor(init: List[TypedStmt], cond: TypedExpr, update: List[TypedStmt], body: List[TypedStmt])
                      (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val lHead = lg.fresh(WHILE_LABEL_HEAD)
    val lUpdate = lg.fresh(FOR_UPDATE)
    val lEnd = lg.fresh(WHILE_LABEL_END)

    // `continue` targets the update block, matching source-level for-loop semantics.
    ftx.pushLoop(LoopTarget(lEnd, lUpdate))
    try {
      lowerStmts(init)
      ftx.emit(Mark(lHead))
      lowerCondJumpFalse(cond, lEnd)

      lowerStmts(body)
      ftx.emit(Mark(lUpdate))
      lowerStmts(update)

      ftx.emit(Jmp(lHead))
      ftx.emit(Mark(lEnd))
    } finally {
      ftx.popLoop()
    }
  }

  private def lowerDoWhile(body: List[TypedStmt], cond: TypedExpr)
                          (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    val lHead = lg.fresh(WHILE_LABEL_HEAD)
    val lCond = lg.fresh(DO_WHILE_COND)
    val lEnd = lg.fresh(WHILE_LABEL_END)

    // Do-while uses a separate continue label so `continue` jumps to the trailing condition check, not the body start.
    ftx.pushLoop(LoopTarget(lEnd, lCond))
    try {
      ftx.emit(Mark(lHead))
      lowerStmts(body)
      ftx.emit(Mark(lCond))
      lowerCondJumpTrue(cond, lHead)
      ftx.emit(Mark(lEnd))
    } finally {
      ftx.popLoop()
    }
  }

  private def findLhs(lvalue: TypedLValue)(using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Val = lvalue match {
    // Reads of pair fields and array elements reuse the same address-finding path as writes.
    case TypedLValue.Id(name, _) => ftx.lookup(name)
    case p: TypedLValue.PairElemFst => getIndirectTemp(p)
    case p: TypedLValue.PairElemSnd => getIndirectTemp(p)
    case a: TypedLValue.ArrayElem => lowerArrayElemAsRhs(a)
  }

  private def getIndirectTemp(pair: TypedLValue.PairElemFst | TypedLValue.PairElemSnd)
                             (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Val = {
    val (lv, offset) = pair match {
      case TypedLValue.PairElemFst(fst, _) => (fst, ImmValue(0))
      case TypedLValue.PairElemSnd(snd, _) => (snd, ImmValue(ctx.wordSize))
    }
    val baseVal = findLhs(lv)
    val baseTemp = baseVal match {
      case t: Temp => t
      case it: IndirectTemp =>
        // Pair bases must live in a temp before indirect access so later passes can treat the address uniformly.
        val tmp = ftx.fresh(it.len)
        ftx.emit(TACAssign(tmp, it))
        tmp
    }
    emitNullDereferenceIfNeeded(baseTemp)
    IndirectTemp(baseTemp, offset, false, sizeof(pair.ty))
  }

  private def idxAsOffset(r: Rhs)(using ftx: FuncContext): Temp | ImmValue = r match {
    case t: Temp => t
    case i: ImmValue => i
    case other =>
      // Materialize computed indices into a temp because indirect addressing only accepts temps or immediates.
      val tmp = ftx.fresh(BitLength._32)
      ftx.emit(TACAssign(tmp, other))
      tmp
  }

  private def lowerArrayElemAsRhs(a: TypedLValue.ArrayElem)
                                 (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Val = {
    val TypedLValue.ArrayElem(_, indices, elemTy) = a
    val curArr = resolveArrayBasePointer(a)
    val lastIdxRhs = lowerExpr(indices.last)
    emitArrayBoundsChecks(curArr, lastIdxRhs)
    val lastIdx = idxAsOffset(lastIdxRhs)
    val elemLen: BitLength = sizeof(elemTy)

    // Loads materialize the selected element into a temp; stores use `lowerArrayElemForStore` to keep an l-value.
    val dst = ftx.fresh(elemLen)
    ftx.emit(TACAssign(dst, IndirectTemp(curArr, lastIdx, isArray = true, len = elemLen)))
    dst
  }

  private def lowerArrayElemForStore(a: TypedLValue.ArrayElem)
                                    (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): (Temp, Rhs, BitLength) = {
    val TypedLValue.ArrayElem(_, indices, elemTy) = a
    val curArr = resolveArrayBasePointer(a)

    val lastIdxRhs = lowerExpr(indices.last)
    emitArrayBoundsChecks(curArr, lastIdxRhs)
    val elemLen: BitLength = sizeof(elemTy)
    // The final index stays as an RHS here so the caller can decide whether it needs temp-materialization.
    (curArr, lastIdxRhs, elemLen)
  }

  private def lowerShortCircuitBranch(dst: Temp, lv: Rhs, rhs: TypedExpr, branchCond: CondOp,
                                      branchVal: Int, labelPrefix: String)
                                     (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Temp = {
    val branchLabel = lg.fresh(labelPrefix + SC_LABEL_SUFFIX_BRANCH)
    val endLabel = lg.fresh(labelPrefix + SC_LABEL_SUFFIX_END)

    // The taken branch skips RHS evaluation and writes the known boolean literal straight into `dst`.
    ftx.emit(CmpJmp(branchCond, lv, ImmValue(0, BitLength._8), branchLabel))

    val rv = lowerExpr(rhs)
    ftx.emit(TACAssign(dst, rv))
    ftx.emit(Jmp(endLabel))

    ftx.emit(Mark(branchLabel))
    ftx.emit(TACAssign(dst, ImmValue(branchVal, BitLength._8)))

    ftx.emit(Mark(endLabel))
    dst
  }

  private def lowerCondBodyJump(cond: TypedExpr, body: List[TypedStmt], jumpLabel: Label, loopBackLabel: Label)
                               (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Unit = {
    // Shared helper for if/while shapes: fail the condition to `jumpLabel`, otherwise run body and jump onward.
    lowerCondJumpFalse(cond, jumpLabel)
    lowerStmts(body)
    ftx.emit(Jmp(loopBackLabel))
    ftx.emit(Mark(jumpLabel))
  }

  private def resolveArrayBasePointer(a: TypedLValue.ArrayElem)
                                     (using ftx: FuncContext, lg: LabelGen, ctx: StringContext): Temp = {
    val TypedLValue.ArrayElem(id0, indices, _) = a
    if (indices.isEmpty) throw new Exception(ERR_ARRAYELEM_EMPTY_INDICES)

    var curArr: Temp = ftx.lookup(id0.name)

    // Walk all but the last index to land on the final nested array object before the actual element access.
    for (idxExpr <- indices.init) {
      val idxRhs = lowerExpr(idxExpr)
      emitArrayBoundsChecks(curArr, idxRhs)
      val idxR = idxAsOffset(idxRhs)
      val nextArr = ftx.fresh(BitLength._ptr)
      ftx.emit(TACAssign(nextArr, IndirectTemp(curArr, idxR, isArray = true, len = BitLength._ptr)))
      curArr = nextArr
    }

    curArr
  }
}
