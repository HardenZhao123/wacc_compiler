package wacc.frontend.typeCheck

import scala.collection.mutable
import wacc.common.PositionInfo
import wacc.frontend.ast.*
import wacc.frontend.typedAST.*
import wacc.frontend.typedAST.SemanticType.*
import wacc.frontend.ast.Stmt.{SeqStmt}
import wacc.frontend.typeCheck.TypeCheckError.FuncDuplicateOverloadSignature

/**
 * TypeEnv stores the current type information for:
 *   - `varEnv`: mapping from variable names to their semantic types
 *   - `funcEnv`: mapping from function names to all overload entries
 */
final case class TypeEnv(
                          varEnv: Map[String, SemanticType],
                          funcEnv: Map[String, List[TypeChecker.FuncEntry]]
                        )

object TypeChecker {

  final case class FuncEntry(
                              baseName: String,
                              resolvedName: String,
                              returnType: SemanticType,
                              paramTypes: List[SemanticType],
                              pos: PositionInfo
                            )

  // Context for type checking
  class TypeCheckerCtx {
    var env: TypeEnv = typeEnv
    var isFunction: Boolean = false
    var currentReturnType: Option[SemanticType] = None
    var loopDepth: Int = 0
    var breakableDepth: Int = 0

    /* Lookup the type of a variable, returns SemUnknown if not found */
    def returnVariableType(name: String): SemanticType =
      env.varEnv.get(name).getOrElse(SemanticType.SemUnknown)

    /* Lookup all overloads of a function by base name */
    def returnFuncOverloads(name: String): List[FuncEntry] =
      env.funcEnv.getOrElse(name, Nil)

    /* Resolve a declared function (for checking a function body itself) by exact signature */
    def resolveDeclaredFunc(name: String, paramTypes: List[SemanticType]): Option[FuncEntry] =
      returnFuncOverloads(name).find(_.paramTypes == paramTypes)

    /* Add a type checking error to the global list */
    def addError(err: TypeCheckError): Unit = typeErrors += err
  }

  private val varEnv0: Map[String, SemanticType] = Map.empty
  private val funcEnv0: Map[String, List[FuncEntry]] = Map.empty
  private val typeEnv: TypeEnv = TypeEnv(varEnv0, funcEnv0)

  // reset builder
  private var typeErrors: mutable.Builder[TypeCheckError, List[TypeCheckError]] =
    List.newBuilder[TypeCheckError]

  def getErrors(): List[TypeCheckError] = typeErrors.result()

  private def resetErrors(): Unit =
    typeErrors = List.newBuilder[TypeCheckError]

  /* Check the types of a program, returning a TypedProgram. */
  def checkProgram(prog: Program): TypedProgram = {
    resetErrors()
    given ctx: TypeCheckerCtx = TypeCheckerCtx()

    val Program(funcs, body) = prog

    // Build the full overload table before checking bodies so calls can resolve across the whole program.
    buildFuncEnv(funcs)

    // check types of all functions
    val typedFuncs = funcs.map(TypeCheckerStmt.checkFunc)

    // type check main program body
    val typedStmts = body match {
      case SeqStmt(stmts) => stmts.toList.flatMap(TypeCheckerStmt.checkStmt)
      case singleStmt => List(singleStmt).flatMap(TypeCheckerStmt.checkStmt)
    }

    TypedProgram(typedFuncs, typedStmts)
  }

  /* Build the function environment for type checking */
  def buildFuncEnv(funcs: List[Func])(using ctx: TypeCheckerCtx): Unit = {
    val acc = mutable.Map.empty[String, List[FuncEntry]]

    funcs.foreach { f =>
      val name = f.name.ident
      val retTy = SemanticType.fromAstType(f.ret)
      val paramTys = f.params.map(p => SemanticType.fromAstType(p.t))
      val entry = FuncEntry(name, mangleFuncName(name, paramTys), retTy, paramTys, f.positionInfo)

      val overloads = acc.getOrElse(name, Nil)
      overloads.find(_.paramTypes == paramTys) match {
        case Some(prev) =>
          ctx.addError(FuncDuplicateOverloadSignature(name, paramTys, f.positionInfo, prev.pos))
        case None =>
          // Keep every valid overload under the same base name and defer exact matching to call checking.
          acc.update(name, overloads :+ entry)
      }
    }

    ctx.env = ctx.env.copy(funcEnv = acc.toMap)
  }

  // Overload lowering: one symbol per (name, parameter types)
  def mangleFuncName(baseName: String, paramTypes: List[SemanticType]): String = {
    val sig =
      if (paramTypes.isEmpty) "V"
      else paramTypes.map(encodeType).mkString("__")
    s"${baseName}__${sig}"
  }

  // Encode a semantic type into a short string representation.
  // This is typically used for name mangling (e.g., function overloading)
  // so that functions with different parameter types have unique symbols.
  private def encodeType(ty: SemanticType): String = ty match {
    case SemInt => "I"
    case SemBool => "B"
    case SemChar => "C"
    case SemFloat => "F"
    case SemString => "S"
    case SemUnknown => "U"
    case SemPairErased => "PE"
    case SemArray(elem, arity) => s"A${arity}_${encodeType(elem)}"
    case SemPair(fst, snd) => s"P_${encodeType(fst)}_${encodeType(snd)}_Q"
    case SemException => throw new Exception("Exception type cannot be encoded")
  }
}
