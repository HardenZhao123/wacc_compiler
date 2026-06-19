package wacc.common

import parsley.position.pos
import parsley.{Parsley}

// A class to track source positions for error checks.
case class PositionInfo(row: Int, column: Int)

/* Attaches source position information to the result AST after parsing. */
def withPos[A, B](p: Parsley[A])(make: (A, PositionInfo) => B): Parsley[B] = {
  pos.map(p => PositionInfo(p._1, p._2)) // type: Parsley[PositionInfo]
    .flatMap { posInfo =>                // posInfo type: PositionInfo
      p.map { value =>                   // p.map{...} type: Parsley[B]
        make(value, posInfo)
      }
    }
}
