package fpinscala.chapter2

import org.scalatest.FunSuite
import Exercise3.curry

class Exercise3TestSuite extends FunSuite {
  test("returns expected result for a function where currying is applied") {
    val g = curry[Int, Int, Int]((a, b) => a * b)(5)
    assert(g(2) == 10)
  }
}
