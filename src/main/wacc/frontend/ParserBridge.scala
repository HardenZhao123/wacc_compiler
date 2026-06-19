package wacc.frontend

import java.nio.file.{Files, Path}
import parsley.{Failure, Result, Success}

import wacc.frontend.ast.Program
import wacc.common.PositionInfo
import parsley.Parsley
import parsley.bridges.*
import parsley.ap.*
import parsley.position.pos
import wacc.frontend.errorMessage.SyntaxError

object ParserBridge {

  // Thin adapter layer: exposes file/string entry points and provides position-aware bridge traits for AST construction.

  // Parse a full WACC program from a raw source string and return either a formatted syntax error or the AST.
  def parseProgramString(input: String, sourceName: String = "unknown"): Either[String, Program] = {
    val r: Result[String, Program] = parser.parseProgram(input)
    toEither(r, sourceName)
  }

  // Convenience wrapper that reads a file and parses it using the file name as the source label in error messages.
  def parseProgramFile(path: Path): Either[String, Program] = {
    val src = Files.readString(path)
    parseProgramString(src, path.getFileName.toString)
  }

  // Normalize Parsley Result into Either, rewriting the error header to use the provided source name.
  private def toEither[A](r: Result[String, A], sourceName: String): Either[String, A] =
    r match {
      case Success(a) => Right(a)
      case Failure(e) => Left(SyntaxError.attachSource(e, sourceName))
    }

  // Base for position-aware bridges: captures the current (row, col) and threads PositionInfo into the constructor.
  trait ParserSingletonBridgePos[+A] extends ErrorBridge {
    protected def con(pos: PositionInfo): A
    // Attach an error label to `op` while producing an A built from the current parser position.
    infix def from(op: Parsley[Any]): Parsley[A] = error(
      pos.map { case (r, c) => con(PositionInfo(r, c)) } <* op
    )
    // Operator alias for `from`, matching Parsley bridge style used elsewhere in the project.
    final def <#(op: Parsley[Any]): Parsley[A] = this from op
  }

  // Bridge for constructors of arity 1 that also require PositionInfo.
  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: PositionInfo): B
    def apply(x: Parsley[A]): Parsley[B] = error(ap1(
      pos.map { case (r, c) => con(PositionInfo(r, c)) }, x
    ))
    override final def con(pos: PositionInfo): A => B = this.apply(_)(pos)
  }

  // Bridge for constructors of arity 2 that also require PositionInfo.
  trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: PositionInfo): C
    def apply(x: Parsley[A], y: =>Parsley[B]): Parsley[C] = error(ap2(
      pos.map { case (r, c) => con(PositionInfo(r, c)) }, x, y
    ))
    override final def con(pos: PositionInfo): (A, B) => C = this.apply(_, _)(pos)
  }

  // Bridge for constructors of arity 3 that also require PositionInfo.
  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: PositionInfo): D
    def apply(x: Parsley[A], y: =>Parsley[B], z: =>Parsley[C]): Parsley[D] = error(ap3(
      pos.map { case (r, c) => con(PositionInfo(r, c)) }, x, y, z
    ))
    override final def con(pos: PositionInfo): (A, B, C) => D = this.apply(_, _, _)(pos)
  }

  // Bridge for constructors of arity 4 that also require PositionInfo.
  trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    def apply(x: A, y: B, z: C, w: D)(pos: PositionInfo): E
    def apply(x: Parsley[A], y: =>Parsley[B], z: =>Parsley[C], w: =>Parsley[D]): Parsley[E] = error(ap4(
      pos.map { case (r, c) => con(PositionInfo(r, c)) }, x, y, z, w
    ))
    override final def con(pos: PositionInfo): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
  }
}
