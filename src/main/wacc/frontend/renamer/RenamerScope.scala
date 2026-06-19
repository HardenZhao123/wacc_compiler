package wacc.frontend.renamer

import scala.collection.mutable
import wacc.common.PositionInfo
import wacc.frontend.ast.Expr.*
import wacc.frontend.ast.Type

type Unrenamed = String

// a uniquely renamed variable using its original name and a fresh numeric suffix
final case class Renamed(base: String, id: Int) {
  override def toString: String = s"$base-$id"
}

// the current variable scope and its parent scope during renaming
final class RenamerScope(
                          val cur: mutable.Map[Unrenamed, Renamed],
                          val parent: Map[Unrenamed, Renamed]
                        ) {
  // look up a variable in the current scope also in parent scopes
  def lookup(x: Unrenamed): Option[Renamed] =
    cur.get(x).orElse(parent.get(x))

  // create a fresh child scope with the current scope(all the scope above) as its parent
  def newScope: RenamerScope =
    // Child scopes see a frozen snapshot of all visible bindings while keeping new declarations local.
    new RenamerScope(mutable.Map.empty, parent ++ cur)

  // return all variables in current scope
  def visible: Map[Unrenamed, Renamed] = parent ++ cur
}

object RenamerScope {
  // dummy name for out of scope variable
  private val OutOfScopeName = "out-of-scope"

  // Convert an internal renamed variable into a concrete Identifier AST node
  private def asIdentifier(r: Renamed, pos: PositionInfo): Identifier =
    Identifier(r.toString)(pos)

  // resolve a variable by looking it up in the current scope, emitting an error if undefined
  def fromScope(id: Identifier)(using ctx: RenamerScope, st: RenamerState): Identifier = {
    val base = id.ident
    ctx.lookup(base) match {
      case Some(r) => asIdentifier(r, id.positionInfo)

      case None =>
        // look for all the current in-scope variable and make a list of entries
        val entries: List[ScopeEntry] =
          ctx.visible.toList.flatMap { case (unrenamed, renamed) =>
            st.declInfo.get(renamed).map { info =>
              ScopeEntry(unrenamed, info.t, info.declaredAt)
            }
          }
        st.emit(RenameError.OutOfScope(base, id.positionInfo, entries))
        Identifier(OutOfScopeName)(id.positionInfo)
    }
  }

  // declare a new variable in the current scope, generating a fresh name and reporting redeclarations err
  def declare(id: Identifier, t: Type)(using ctx: RenamerScope, st: RenamerState): Identifier = {
    val base = id.ident
    ctx.cur.get(base).foreach { prevRenamed =>
      st.declInfo.get(prevRenamed).foreach { prevInfo =>
        st.emit(RenameError.Redeclaration(base, id.positionInfo, prevInfo.declaredAt))
      }
    }
    // Fresh names are allocated even after an error so later passes can continue on a consistent AST.
    val r = st.fresh(base, t, id.positionInfo)
    ctx.cur(base) = r
    asIdentifier(r, id.positionInfo)
  }
}
