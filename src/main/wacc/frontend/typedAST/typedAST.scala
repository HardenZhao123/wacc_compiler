package wacc.frontend.typedAST

import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.ast.WACCExceptionType

sealed abstract class TypedExpr(var ty: SemanticType)
object TypedExpr {
  sealed trait BinaryOperation

  /* Binary arithmetic operations: +, -, *, /, % */
  enum ArithmeticOperation extends BinaryOperation {case Add, Sub, Mul, Div, Mod}
  case class BinaryArithmetic(left: TypedExpr, right: TypedExpr, op: ArithmeticOperation)
    extends TypedExpr(
      op match {
        case ArithmeticOperation.Mod =>
          SemInt

        case _ =>
          (left.ty, right.ty) match {
            case (SemInt, SemInt)       => SemInt
            case (SemFloat, SemFloat)   => SemFloat
            case (SemFloat, SemInt)     => SemFloat
            case (SemInt, SemFloat)     => SemFloat
            case _                      => SemUnknown
          }
      }
    )

  /* Binary comparison operations: ==, !=, >, >=, <, <= */
  enum CompareOperation extends BinaryOperation {case Greater, GreaterEqual, Less, LessEqual, Equal, NotEqual}
  case class BinaryCompare(left: TypedExpr, right: TypedExpr, op: CompareOperation)
    extends TypedExpr(SemBool)

  /* Binary boolean operations: &&, || */
  enum BoolOperation extends BinaryOperation {case And, Or}
  case class BinaryBool(left: TypedExpr, right: TypedExpr, op: BoolOperation)
    extends TypedExpr(SemBool)
  
  /* Binary bitwise operations: &, | */
  enum BitwiseAndOr extends BinaryOperation { case BitAnd, BitOr }
  case class BinaryBitwise(left: TypedExpr, right: TypedExpr, op: BitwiseAndOr) 
    extends TypedExpr(SemInt)

  /* Unary operations: !, -, len, ord, chr, ~ */
  case class Not(operand: TypedExpr) extends TypedExpr(SemBool)
  case class Neg(operand: TypedExpr) extends TypedExpr(
    operand.ty match {
      case SemInt   => SemInt
      case SemFloat => SemFloat
      case _        => SemUnknown
    }
  )
  case class Len(operand: TypedExpr) extends TypedExpr(SemInt)
  case class Ord(operand: TypedExpr) extends TypedExpr(SemInt)
  case class Chr(operand: TypedExpr) extends TypedExpr(SemChar)
  case class BitNot(operand: TypedExpr) extends TypedExpr(SemInt)

  /* Literal expressions */
  case class IntLit(value: Int) extends TypedExpr(SemInt)
  case class BoolLit(value: Boolean) extends TypedExpr(SemBool)
  case class CharLit(value: Char) extends TypedExpr(SemChar)
  case class FloatLit(value: Float) extends TypedExpr(SemFloat)
  case class StrLit(value: String) extends TypedExpr(SemString)
  case class PairLit() extends TypedExpr(SemPairErased)
}

/* Typed exceptions */
sealed abstract class TypedException(var ty: SemanticType)
object TypedException {
  case class ArrayOutOfBoundsEx(msg: TypedExpr) extends TypedException(SemString)
  case class BadCharEx(msg: TypedExpr) extends TypedException(SemString)
  case class ArithmeticEx(msg: TypedExpr) extends TypedException(SemString)
  case class IntegerOverflowEx(msg: TypedExpr) extends TypedException(SemString)
  case class NullDereferenceEx(msg: TypedExpr) extends TypedException(SemString)
  case class GeneralEx(msg: TypedExpr) extends TypedException(SemString)
}

/* Typed L-values */
sealed abstract class TypedLValue(ty: SemanticType) extends TypedExpr(ty)
object TypedLValue {
  case class Id(name: String, semType: SemanticType) extends TypedLValue(semType)
  case class ArrayElem(ident: Id, indices: List[TypedExpr], semType: SemanticType)
    extends TypedLValue(semType)

  case class PairElemFst(fst: TypedLValue, semType: SemanticType) extends TypedLValue(semType)
  case class PairElemSnd(snd: TypedLValue, semType: SemanticType) extends TypedLValue(semType)
}

/* Typed R-values */
sealed abstract class TypedRValue(ty: SemanticType) extends TypedExpr(ty)
object TypedRValue {
  case class ExprR(expr: TypedExpr) extends TypedRValue(expr.ty)
  case class PairElemR(pe: TypedLValue) extends TypedRValue(pe.ty)
  case class ArrayLit(exprs: List[TypedExpr], semType: SemanticType) extends TypedRValue(semType)
  case class NewPair(fst: TypedExpr, snd: TypedExpr, fstType: SemanticType, sndType: SemanticType)
    extends TypedRValue(SemPairErased)

  case class Call(baseIdent: String, resolvedIdent: String, args: List[TypedExpr],
                  returnType: SemanticType, argTypes: List[SemanticType]) extends TypedRValue(returnType)
}

// Typed catch handler
case class TypedCatchHandler(exType: WACCExceptionType, exName: TypedLValue.Id, body: List[TypedStmt])

/* Typed Stmt */
sealed abstract class TypedStmt
object TypedStmt {
  case class Decl(ident: TypedLValue.Id, value: TypedRValue) extends TypedStmt
  case class Assign(left: TypedLValue, right: TypedRValue) extends TypedStmt
  case class Read(target: TypedLValue) extends TypedStmt
  case class Free(expr: TypedExpr) extends TypedStmt
  case class Return(expr: TypedExpr) extends TypedStmt
  case class Throw(expr: TypedException) extends TypedStmt
  case class Exit(expr: TypedExpr) extends TypedStmt
  case class Print(expr: TypedExpr) extends TypedStmt
  case class Println(expr: TypedExpr) extends TypedStmt
  case class If(cond: TypedExpr, thenBranch: List[TypedStmt], elseBranch: List[TypedStmt]) extends TypedStmt
  case class While(cond: TypedExpr, body: List[TypedStmt]) extends TypedStmt
  case class TryCatch(tryBody: List[TypedStmt], handlers: List[TypedCatchHandler]) extends TypedStmt
  case class For(init: List[TypedStmt], cond: TypedExpr, update: List[TypedStmt], body: List[TypedStmt]) extends TypedStmt
  case class DoWhile(body: List[TypedStmt], cond: TypedExpr) extends TypedStmt
  case class Break() extends TypedStmt
  case class Continue() extends TypedStmt
  case class BeginEnd(stmts: List[TypedStmt]) extends TypedStmt
  case class Skip() extends TypedStmt
}

/* Typed Function and Program */
case class TypedFunction(returnType: SemanticType, ident: String, resolvedIdent: String, params: List[TypedLValue.Id], stmts: List[TypedStmt])
case class TypedProgram(funcs: List[TypedFunction], stmts: List[TypedStmt])
