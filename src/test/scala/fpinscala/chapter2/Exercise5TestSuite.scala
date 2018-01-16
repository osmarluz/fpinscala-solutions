package fpinscala.chapter2

import org.scalatest.FunSuite
import Exercise5.compose

class Exercise5TestSuite extends FunSuite {
  test("returns the expected result of two composed functions f(g(a))") {
    val f = compose[Int, Int, Double](b => b / 2, a => a * a)
    assert(f(4) == 8)
  }
}
