package wacc.unit_test.frontend

import org.scalatest.flatspec.AnyFlatSpec
import parsley.{Success, Failure}

trait ParserTestHelpers { self: AnyFlatSpec =>

  protected def expectFailure[T](res: parsley.Result[String, T]): Unit = res match {
    case Failure(_) => succeed: Unit
    case Success(x) => fail(s"Expected parsing to fail, but it succeeded with: $x")
  }
}
