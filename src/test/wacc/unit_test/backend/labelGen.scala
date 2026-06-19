package wacc.unit_test.backend

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import wacc.backend.label.LabelGen

final class labelGen extends AnyFunSuite with Matchers {

  test("LabelGen should generate unique labels") {
    val g = new LabelGen
    val a = g.fresh("if_end").name
    val b = g.fresh("if_end").name
    a should not equal b
  }

  test("LabelGen should preserve prefix intent and sanitize unsafe chars") {
    val g = new LabelGen
    val x = g.fresh("if end!").name
    x should startWith ("if_end_")
  }
}
