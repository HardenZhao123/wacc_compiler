package wacc.backend.BackendCommon

import scala.collection.mutable

// Base trait for all operands
trait Operand
// Common register abstraction used by shared backend helpers.
trait Register extends Operand {
  def name: String
  override def toString: String = name
}

// Small scoped allocator for scratch registers during code generation.
abstract class RegisterAllocator[T <: Register](val scratchRegisters: List[T]) {

  // Queue of currently available registers
  val availableRegisters = mutable.Queue(scratchRegisters*)

  // Set of currently allocated registers
  val allocatedRegisters = mutable.Set.empty[T]

  // allocate an available register
  private def allocateRegister(): T = {
    if (availableRegisters.isEmpty) {
      throw new Exception("No more scratch registers available to be allocated.")
    }

    val register = availableRegisters.dequeue()
    allocatedRegisters.add(register)
    register
  }

  // true if the register reg is currently being allocated
  private def currentlyUsed(reg: T): Boolean = allocatedRegisters.contains(reg)

  // free a currently allocated register
  private def freeRegister(reg: T): Unit = {
    if (currentlyUsed(reg)) {
      allocatedRegisters.remove(reg)
      availableRegisters.enqueue(reg)
    }
  }

  // Allocate a register, use it in a block, then automatically free it
  def withRegister[A](f: T => A): A = {
    val reg = allocateRegister()
    val result = f(reg)
    freeRegister(reg)
    result
  }

  // Allocate two registers at once
  def withRegisters2[A](f: (T, T) => A): A =
    withRegister { reg1 =>
      withRegister { reg2 =>
        f(reg1, reg2)
      }
    }

  // Allocate six registers for multi-step lowering that needs a fixed scratch set.
  def withRegisters6[A](f: (T, T, T, T, T, T) => A): A =
    withRegisters2 { (reg1, reg2) =>
      withRegisters2 { (reg3, reg4) =>
        withRegisters2 { (reg5, reg6) =>
          f(reg1, reg2, reg3, reg4, reg5, reg6)
        }
      }
    }
}
