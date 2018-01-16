package fpinscala.chapter2

import org.scalatest.FunSuite
import Exercise4.uncurry

class Exercise4TestSuite extends FunSuite {
  test("returns expected result for an uncurried function") {
    val g = uncurry[Int, Int, Int](a => b => a * b)
    assert(g(2, 5) == 10)
  }
}
