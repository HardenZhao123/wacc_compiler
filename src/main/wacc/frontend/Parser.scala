package wacc.frontend

import parsley.{Parsley, Result}
import parsley.expr.{InfixL, InfixN, InfixR, Prefix, SOps, precedence}
import parsley.quick.{atomic, many, pure, some}
import lexer.implicits.given
import lexer.{boolean, char, float, fully, identifier, implicits, integer, string}
import wacc.frontend.ast.*
import wacc.frontend.ast.Expr.*
import wacc.frontend.ast.Stmt.*
import wacc.frontend.ast.LValue.*
import wacc.frontend.ast.RValue.*
import wacc.common.{PositionInfo, withPos}
import wacc.frontend.errorMessage.SyntaxError.given
import wacc.frontend.ast.WACCException.*

object parser {

    // Small test entrypoints: each wraps the core parser with `fully(...)` so it must consume the whole input.
    def parseExpr(input: String): Result[String, Expr] = fully(expr).parse(input)
    def parseType(input: String): Result[String, Type] = fully(typeP).parse(input)
    def parseBaseType(input: String): Result[String, Type] = fully(baseTypeP).parse(input)
    def parsePairType(input: String): Result[String, Type] = fully(pairTypeP).parse(input)
    def parsePairElemType(input: String): Result[String, Type] = fully(pairElemTypeP).parse(input)

    def parseParam(input: String): Result[String, Param] = fully(paramP).parse(input)
    def parseParamList(input: String): Result[String, List[Param]] = fully(paramListP).parse(input)

    def parseFunc(input: String): Result[String, Func] = fully(funcP).parse(input)

    def parsePairElem(input: String): Result[String, PairElem] = fully(pairElemP).parse(input)
    def parseLValue(input: String): Result[String, LValue] = fully(lvalueP).parse(input)
    def parseRValue(input: String): Result[String, RValue] = fully(rvalueP).parse(input)

    def parseStmt(input: String): Result[String, Stmt] = fully(stmtP).parse(input)

    def parseProgram(input: String): Result[String, Program] = fully(program).parse(input)

    // Expression atoms: these are the leaves used by the precedence table below.
    private lazy val intAtom: Parsley[Expr] = Expr.IntLiter(integer)
    private lazy val boolAtom: Parsley[Expr] = Expr.BooleanLiter(boolean)
    private lazy val charAtom: Parsley[Expr] = Expr.CharLiter(char)
    private lazy val stringAtom: Parsley[Expr] = Expr.StringLiter(string)
    private lazy val floatAtom: Parsley[Expr] = Expr.FloatLiter(float)
    private lazy val pairAtom: Parsley[Expr] = withPos("null") { (_, p) => PairLiter()(p) }
    private lazy val identP: Parsley[Identifier] = Expr.Identifier(identifier)
    private lazy val identifierAtom: Parsley[Expr] = identP.map(id => id: Expr)

    private lazy val arrayAtom: Parsley[Expr] = ArrayElem(identP, some("[" ~> expr <~ "]"))

    private lazy val parensAtom: Parsley[Expr] = Expr.Parens("(" ~> expr <~ ")")

    private lazy val atom: Parsley[Expr] =
        floatAtom <|> intAtom <|> boolAtom <|> charAtom <|> stringAtom
    <|> pairAtom <|> atomic(arrayAtom) <|> identifierAtom
    <|> parensAtom

    // Operator precedence table: each `Expr.X from "tok"` uses your bridge traits to attach PositionInfo to AST nodes.
    private lazy val expr: Parsley[Expr] = precedence(atom)(
        SOps(Prefix)(
            Expr.Not from "!", Expr.Neg from "-", Expr.Len from "len",
            Expr.Ord from "ord", Expr.Chr from "chr", Expr.BitNot from "~"
        ),

        SOps(InfixL)(Expr.Mul from "*", Expr.Mod from "%", Expr.Div from "/"),

        SOps(InfixL)(Expr.Add from "+", Expr.Sub from "-"),

        SOps(InfixN)(
            Expr.Greater from ">", Expr.GreaterEqual from ">=",
            Expr.Less from "<", Expr.LessEqual from "<=",
        ),

        SOps(InfixN)(Expr.Equal from "==", Expr.NotEqual from "!="),

        SOps(InfixL)(Expr.BitAnd from "&"),
        SOps(InfixL)(Expr.BitOr from "|"),

        SOps(InfixR)(Expr.And from "&&"),
        SOps(InfixR)(Expr.Or  from "||"),
    )

    // Parser for WACC exception expressions.
    // Each branch parses a specific exception constructor with a message expression.
    // Example syntax: ArithmeticException("division by zero")
    private lazy val exceptionP: Parsley[WACCException] =
        ArrayOutOfBoundsException("ArrayOutOfBoundsException(" ~> expr <~ ")") <|>
          ArithmeticException("ArithmeticException(" ~> expr <~ ")") <|>
          IntegerOverflowException("IntegerOverflowException(" ~> expr <~ ")") <|>
          NullDereferenceException("NullDereferenceException(" ~> expr <~ ")") <|>
          BadCharException("BadCharException(" ~> expr <~ ")") <|>
          GeneralException("Exception(" ~> expr <~ ")")

    // Helper: parse a keyword and build a positioned base type node.
    private def baseTypeKW(s: String, make: PositionInfo => Type): Parsley[Type] =
        withPos(s) { (_, p) => make(p) }

    private lazy val baseTypeP: Parsley[Type] =
        baseTypeKW("int", IntType()) <|>
          baseTypeKW("bool", BoolType()) <|>
          baseTypeKW("char", CharType()) <|>
          baseTypeKW("float", FloatType()) <|>
          baseTypeKW("string", StringType())

    private lazy val oneDim: Parsley[Unit] = "[" *> "]"

    private lazy val arrayDims1: Parsley[Int] = some(oneDim).map(_.length)
    private lazy val arrayDims0: Parsley[Int] = many(oneDim).map(_.length)

    private lazy val pairTypeP: Parsley[Type] =
        PairType("pair" ~> "(" ~> pairElemTypeP, "," ~> pairElemTypeP <~ ")")

    private def exceptionTypeKW(s: String, make: PositionInfo => WACCExceptionType): Parsley[WACCExceptionType] =
        withPos(s) { (_, p) => make(p) }

    private lazy val exceptionTypeP: Parsley[WACCExceptionType] =
        exceptionTypeKW("ArithmeticException", Arithmetic()) <|>
          exceptionTypeKW("BadCharException", BadChar()) <|>
          exceptionTypeKW("ArrayOutOfBoundsException", ArrayOutOfBounds()) <|>
          exceptionTypeKW("IntegerOverflowException", IntegerOverflow()) <|>
          exceptionTypeKW("NullDereferenceException", NullDereference()) <|>
          exceptionTypeKW("Exception", General())

    // Pair element types: allows base types, nested pair types (atomic), arrays of atomic types, and erased `pair`.
    private lazy val pairElemTypeP: Parsley[Type] = {
        lazy val erasedPair: Parsley[Type] = withPos("pair") { (_, p) => PairErasedType()(p) }

        lazy val atomicTypeP: Parsley[Type] = baseTypeP <|> atomic(pairTypeP)

        lazy val arrayType1: Parsley[Type] = ArrayType(atomicTypeP, arrayDims1)

        atomic(arrayType1) <|> erasedPair <|> baseTypeP
    }

    // General type parser: parse a base/pair type then optionally apply zero-or-more `[]` dimensions.
    private lazy val typeP: Parsley[Type] =
        withPos(
            for {
                t <- (baseTypeP <|> pairTypeP)
                d <- arrayDims0
            } yield (t, d)
        ) {
            case ((t, 0), _) => t
            case ((t, d), p) => ArrayType(t, d)(p)
        }

    private lazy val paramP: Parsley[Param] = Param(typeP, identP)

    private lazy val paramListP: Parsley[List[Param]] =
        for {
            first <- paramP
            rest <- many("," *> paramP)
        } yield first :: rest

    // Function header is made atomic so errors inside it do not backtrack into other productions.
    private lazy val funcHeadP: Parsley[(Type, Identifier)] =
        atomic(
            for {
                ret <- typeP
                name <- identP
                _ <- "("
            } yield (ret, name)
        )

    // Full function: return type + name + params + "is" + body stmt + "end", with a single PositionInfo on the Func node.
    private lazy val funcP: Parsley[Func] =
        withPos(
            for {
                (ret, name) <- funcHeadP
                params <- (paramListP <|> pure(List.empty[Param]))
                _ <- ")"
                _ <- "is"
                body <- stmtP
                _ <- "end"
            } yield (ret, name, params, body)
        ) { case ((ret, name, params, body), p) =>
            Func(ret, name, params, body)(p)
        }

    private lazy val arrayElemP: Parsley[ArrayElem] = ArrayElem(identP, some("[" ~> expr <~ "]"))

    private lazy val pairElemP: Parsley[PairElem] = PFst("fst" ~> lvalueP) <|> PSnd("snd" ~> lvalueP)

    private lazy val lIdentP: Parsley[LValue] = LIdent(identP)

    private lazy val lArrayP: Parsley[LValue] = LArray(arrayElemP)

    private lazy val lPairElemP: Parsley[LValue] = LPairElem(pairElemP)

    // Ordering matters: pairElem/array need to be atomic so they do not partially consume input then fall back to identifier.
    private lazy val lvalueP: Parsley[LValue] = atomic(lPairElemP) <|> atomic(lArrayP) <|> lIdentP

    private lazy val nonEmptyExprListP: Parsley[List[Expr]] =
        for {
            first <- expr
            rest <- many("," *> expr)
        } yield first :: rest

    private lazy val elems = nonEmptyExprListP <|> parsley.Parsley.pure(List.empty[Expr])
    private lazy val arrayLiteralP: Parsley[RArrayLiter] = RArrayLiter("[" ~> elems <~ "]")

    private lazy val argListP: Parsley[List[Expr]] =
        (for {
            first <- expr
            rest <- many("," *> expr)
        } yield first :: rest) <|> parsley.Parsley.pure(List.empty[Expr])

    private lazy val newPairP: Parsley[RValue] = RNewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")

    private lazy val callP: Parsley[RValue] = RCall("call" ~> identP, "(" ~> argListP <~ ")")

    private lazy val rvalueP: Parsley[RValue] =
        callP <|> newPairP <|> RPairElem(pairElemP) <|> arrayLiteralP.map(al => al: RValue) <|> RExpr(expr)

    private lazy val assignP: Parsley[Stmt] = Assign(lvalueP, "=" ~> rvalueP)

    private lazy val returnP: Parsley[Stmt] = Return("return" ~> expr)

    private lazy val throwP: Parsley[Stmt] = Throw("throw" ~> exceptionP)

    private lazy val printP: Parsley[Stmt] = Print("print" ~> expr)

    private lazy val printlnP: Parsley[Stmt] = Println("println" ~> expr)

    private lazy val beginP: Parsley[Stmt] = BeginEnd("begin" ~> stmtP <~ "end")

    // Parser for a catch handler in a try-catch construct.
    // Example WACC syntax:
    // catch ArithmeticException e do
    //     <statements>
    // end
    private lazy val catchHandlerP: Parsley[CatchHandler] =
        withPos(
            for {
                _ <- "catch"
                exceptionTy <- exceptionTypeP
                ident <- identP
                _ <- "do"
                body <- stmtP
            } yield (exceptionTy, ident, body)
        ) {
            case ((exceptionTy, ident, body), p) =>
                CatchHandler(exceptionTy, ident, body)(p)
        }

    private lazy val tryCatchP: Parsley[Stmt] = TryCatch("try" ~> stmtP, some(catchHandlerP) <~ "done")

    private lazy val whileP: Parsley[Stmt] = While("while" ~> expr, "do" ~> stmtP <~ "done")

    private lazy val forP: Parsley[Stmt] = For("for (" ~> stmtP <~ ",", expr <~ ",", stmtP <~ ")", stmtP <~ "done")
    
    private lazy val doWhileP: Parsley[Stmt] = DoWhile("do" ~> stmtP, "while" ~> expr)

    private lazy val breakP: Parsley[Stmt] = Break from "Break"

    private lazy val continueP: Parsley[Stmt] = Continue from "Continue"

    private lazy val ifP: Parsley[Stmt] = If("if" ~> expr, "then" ~> stmtP, "else" ~> stmtP <~ "fi")

    private lazy val readP: Parsley[Stmt] = Read("read" ~> lvalueP)

    private lazy val freeP: Parsley[Stmt] = Free("free" ~> expr)

    private lazy val exitP: Parsley[Stmt] = Exit("exit" ~> expr)

    private lazy val declP: Parsley[Stmt] = Decl(typeP, identP, "=" ~> rvalueP)

    private lazy val skipP: Parsley[Stmt] = withPos("skip") { (_, p) => Skip()(p) }

    // Statement atoms: preference order is important because many productions share prefixes.
    private lazy val stmtAtom: Parsley[Stmt] =
        skipP <|> ifP <|> whileP <|> forP <|> beginP <|> tryCatchP
      <|> doWhileP <|> breakP <|> continueP <|> declP <|> assignP <|> readP <|> freeP
      <|> returnP <|> throwP <|> exitP <|> printP <|> printlnP

    // Sequencing: parse one-or-more statements separated by ';' and only wrap in SeqStmt when needed.
    private lazy val stmtP: Parsley[Stmt] =
        withPos(
            for {
                first <- stmtAtom
                rest <- many(";" *> stmtAtom)
            } yield first :: rest
        ) { (ss, p) =>
            ss match {
                case s :: Nil => s
                case _ => SeqStmt(ss)(p)
            }
        }

    // Program is: begin, zero-or-more function declarations, then the main body, then end.
    private lazy val program: Parsley[Program] = Program("begin" ~> many(funcP), stmtP <~ "end")
}
