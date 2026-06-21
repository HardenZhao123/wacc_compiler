package wacc.frontend.typedAST

import wacc.frontend.ast.*

/* Represents the semantic types used during semantic analysis */
sealed trait SemanticType
object SemanticType {

  // Primitive types
  case object SemInt extends SemanticType
  case object SemBool extends SemanticType
  case object SemChar extends SemanticType
  case object SemFloat extends SemanticType
  case object SemString extends SemanticType

  // Array type τ[]
  final case class SemArray(elem: SemanticType, arity: Int) extends SemanticType

  // Non-erased pair type pair(τ, σ)
  final case class SemPair(fst: SemanticType, snd: SemanticType) extends SemanticType
  // Erased pair type
  case object SemPairErased extends SemanticType

  // Exception
  case object SemException extends SemanticType

  case object SemUnknown extends SemanticType

  /* Checks whether `from` type can be weakened to `to` type. */
  def canWeaken(from: SemanticType, to: SemanticType): Boolean = (from, to) match {
    case (SemArray(SemChar, _), SemString) => true
    case (SemPair(_, _), SemPairErased) => true
    case (SemPairErased, SemPair(_, _)) => true
    case _ => false
  }

  /* Checks whether two semantic types are compatible */
  def compatible(from: SemanticType, to: SemanticType): Boolean = (from, to) match {
    case (SemUnknown, _) => true 
    case (_, SemUnknown) => true
    case (SemArray(elem, _), SemArray(refTy, _)) => compatible(elem, refTy)
    case (SemPair(f1, s1), SemPair(f2, s2)) => (f1, s1, f2, s2) match {
      case (SemString, _, SemArray(SemChar, _), _) => false
      case (_, SemString, _, SemArray(SemChar, _)) => false
      case _ => compatible(f1, f2) && compatible(s1, s2)
    }
    case (_, SemException) => false
    case (SemException, _) => false
    case _ => (from == to) || canWeaken(from, to)
  }

  /* Checks if a semantic type is fully known (not unknown) */
  def isKnown(t: SemanticType): Boolean = t match {
    case SemInt | SemBool | SemChar | SemFloat | SemString => true
    case SemArray(_, _) => true
    case SemPair(_, _) => true
    case SemException => true
    case _ => false
  }

  /* Checks if two types have a common supertype */
  def haveCommonSupertype(a: SemanticType, b: SemanticType): Boolean =
    compatible(a, b) || compatible(b, a) || ((a, b) match {
      case (SemArray(SemChar, _), SemString) => true
      case (SemString, SemArray(SemChar, _)) => true
      case (SemPair(_, _), SemPairErased) => true
      case (SemPairErased, SemPair(_, _)) => true
      case _ => false
    })

  /* Converts AST Type nodes to SemanticType for type checking */
  def fromAstType(t: Type): SemanticType = t match {
    case _: IntType => SemInt
    case _: BoolType => SemBool
    case _: CharType => SemChar
    case _: FloatType => SemFloat
    case _: StringType => SemString
    case ArrayType(elem, dims) => SemArray(fromAstType(elem), dims)
    case PairType(fst, snd) => SemPair(fromAstType(fst), fromAstType(snd))
    case _: PairErasedType => SemPairErased
    case _: WACCExceptionType => SemException
  }

  /* Returns a human-readable string for a semantic type */
  def show(t: SemanticType): String = t match {
    case SemInt => "int"
    case SemBool => "bool"
    case SemChar => "char"
    case SemFloat => "float"
    case SemString => "string"
    case SemArray(e, n) => 
      if n > 0 then show(e) + "[]".repeat(n)
      else if show(e) == "<unknown>" then "array"
      else "any dimensional array of " + show(e)
    case SemPair(a, b) => 
      if showPairElem(a) == "<unknown>" || showPairElem(b) == "<unknown>" then "pair"
      else s"pair(${showPairElem(a)}, ${showPairElem(b)})"
    case SemPairErased => "pair"
    case SemUnknown => "<unknown>"
    case SemException => "Exception"
  }

  /* Helper function to print pair elements */
  private def showPairElem(t: SemanticType): String = t match {
    case SemPair(_, _) => "pair"// erased in element position
    case other => show(other)
  }
}
