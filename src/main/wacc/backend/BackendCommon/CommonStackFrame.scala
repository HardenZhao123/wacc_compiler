package wacc.backend.BackendCommon

import scala.collection.mutable

import wacc.backend.midir.TAC.*

// Shared stack-frame allocator used by both target backends.
trait CommonStackFrame() {

  val slots: Seq[Temp]
  val slotSize: Int
  val stackAlignment: Int
  val argRegCount: Int
  val spillTmpAreaSlots: Int
  val frameOverhead: Int

  // Tracks the next available local offset
  private var nextLocalOff: Int = 0

  // Maps compiler temporaries and spill slots to frame offsets.
  private val tempSlots: mutable.Map[Temp, Int] = mutable.Map.empty
  private val spillSlots: mutable.Map[Any, Int] = mutable.Map.empty

  // Allocates one spill slot lazily and reuses it for the same logical key.
  private def allocSpillSlot(key: Any): Int =
    spillSlots.getOrElseUpdate(key, {
      nextLocalOff -= slotSize
      nextLocalOff
    })

  private def spillArgKey(i: Int): String = s"spill_arg_x$i"

  private def spillTmpKey(i: Int): String = s"spill_tmp_$i"

  // Total size of the locals area after applying the target alignment rule.
  def localsAreaSize: Int = align(-nextLocalOff, stackAlignment)

  // Rounds a byte count up to the next required stack alignment.
  private def align(n: Int, a: Int): Int = {
    val r = n % a
    if (r == 0) n else n + (a - r)
  }

  // Assigns stack slots for all tracked temporaries.
  def buildTempSlots(): Unit = {
    val all = slots.distinct
    all.foreach { t =>
      nextLocalOff -= slotSize
      tempSlots(t) = nextLocalOff
    }
  }

  // Reserves stack space used to spill incoming arguments and scratch temporaries.
  def reserveArgSpillArea(): Unit = {
    for (i <- 0 until argRegCount) {
      allocSpillSlot(spillArgKey(i))
    }
    for (i <- 0 until spillTmpAreaSlots) {
      allocSpillSlot(spillTmpKey(i))
    }
  }

  // Returns the frame offset assigned to a temporary.
  def offsetOf(t: Temp): Int =
    tempSlots.getOrElse(t, throw new IllegalStateException(s"Temp has no slot: $t"))

  // Returns the caller-frame offset of an incoming argument passed on the stack.
  def incomingArgOffset(i: Int): Int = {
    if (i < argRegCount)
      throw new IllegalArgumentException(s"param$i is in r$i (register), not on stack")
    frameOverhead + (i - argRegCount) * slotSize
  }
}
