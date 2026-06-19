package wacc.frontend.renamer

import wacc.common.PositionInfo
import wacc.frontend.ast.Type
import scala.collection.mutable

// information for a declared variable after renaming
final case class DeclInfo(t: Type, declaredAt: PositionInfo)

// global mutable state for the renamer,
// tracking fresh-name counters and collected errors
final class RenamerState(
                          val nextId: mutable.Map[Unrenamed, Int],
                          val declInfo: mutable.Map[Renamed, DeclInfo],
                          val errs: mutable.Builder[RenameError, List[RenameError]]
                        ) {

  // record a renaming error
  def emit(e: RenameError): Unit = errs += e

  // generate a fresh renamed variable for a given base name using increasing id
  def fresh(base: Unrenamed, t: Type, declaredAt: PositionInfo): Renamed = {
    val id = nextId.getOrElse(base, 0)
    nextId(base) = id + 1
    // when a new varibale being declared, the mapping(Renamed, DeclInfo) is added to the declInfo
    val r = Renamed(base, id)
    declInfo(r) = DeclInfo(t, declaredAt)
    r
  }
}