package wacc.frontend.typeCheck

import wacc.common.PositionInfo
import wacc.frontend.typedAST.SemanticType

/* Base trait for all type-checking errors */
/* Each error carries a source position and a human-readable message */
sealed trait TypeCheckError { def pos: PositionInfo; def msg: String }

object TypeCheckError {

    // Raised when two types are incompatible
    final case class TypeMismatch(expected: String, actual: String, pos: PositionInfo) extends TypeCheckError {
        val msg = s"Type mismatch, expected: $expected, but got: $actual"
    }

    // Raised when a function call supplies the wrong number of arguments
    final case class FuncArityMismatch(funcName: String, expectedArities: List[Int], actual: Int, pos: PositionInfo)
      extends TypeCheckError {
        private val expectedStr = expectedArities.distinct.sorted.mkString(", ")
        val msg =
            s"""wrong number of arguments provided to function $funcName
               |unexpected $actual arguments
               |expected one of: $expectedStr""".stripMargin
    }

    // Raised when no overload matches argument types
    final case class FuncNoMatchingOverload(funcName: String, argTypes: List[SemanticType],
                                            candidates: List[(List[SemanticType], SemanticType)], pos: PositionInfo)
      extends TypeCheckError {
        private val argsStr = argTypes.map(SemanticType.show).mkString("(", ", ", ")")
        private val candsStr =
          if (candidates.isEmpty) "<none>"
          else candidates
            .map { case (params, ret) => s"${params.map(SemanticType.show).mkString("(", ", ", ")")} -> ${SemanticType.show(ret)}" }
            .mkString("; ")
        val msg = s"no matching overload for $funcName$argsStr; available overloads: $candsStr"
    }

    // Raised when more than one overload matches and resolution is ambiguous
    final case class FuncAmbiguousOverload(funcName: String, argTypes: List[SemanticType],
                                           matches: List[(List[SemanticType], SemanticType)], pos: PositionInfo)
      extends TypeCheckError {
        private val argsStr = argTypes.map(SemanticType.show).mkString("(", ", ", ")")
        private val matchStr =
          matches
            .map { case (params, ret) => s"${params.map(SemanticType.show).mkString("(", ", ", ")")} -> ${SemanticType.show(ret)}" }
            .mkString("; ")
        val msg = s"ambiguous overload for $funcName$argsStr; matching overloads: $matchStr"
    }

    // Raised when duplicate overload signatures are declared
    final case class FuncDuplicateOverloadSignature(funcName: String, signature: List[SemanticType],
                                                    pos: PositionInfo, prevPos: PositionInfo)
      extends TypeCheckError {
        private val sigStr = signature.map(SemanticType.show).mkString("(", ", ", ")")
        val msg = s"duplicate overload signature for function $funcName$sigStr; previously declared on line ${prevPos.row}"
    }

    // Raised when the type checker cannot infer a type
    final case class CannotInferType(pos: PositionInfo) extends TypeCheckError {
        val msg = s"Type cannot be inferred"
    }

    // Raised when a return statement appears outside a function body
    final case class ReturnInMain(pos: PositionInfo) extends TypeCheckError {
        val msg = s"return is only allowed inside a function"
    }

    // Raised when break is used outside a loop body
    final case class BreakOutsideLoop(pos: PositionInfo) extends TypeCheckError {
        val msg = s"break is only allowed inside a loop"
    }

    // Raised when continue is used outside a loop body
    final case class ContinueOutsideLoop(pos: PositionInfo) extends TypeCheckError {
        val msg = s"continue is only allowed inside a loop"
    }

    // Raised when attempting to exchange components of pairs
    // where both sides have completely unknown types
    final case class PairExchangeUnknownTypes(pos: wacc.common.PositionInfo) extends TypeCheckError {
        val msg: String =
            """attempting to exchange values between pairs of unknown types
              |pair exchange is only legal when the type of at least one of the sides is known or specified""".stripMargin
    }

}

