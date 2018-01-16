package fpinscala.chapter2

import org.scalatest.FunSuite
import Exercise2.isSorted

class Exercise2TestSuite extends FunSuite {
  val greaterThan = (a: Int, b: Int) => a > b

  test("returns true for an empty array") {
    val as = Array[Int]()
    assert(isSorted[Int](as, greaterThan))
  }

  test("returns true for an array with only one position") {
    val as = Array(1)
    assert(isSorted[Int](as, greaterThan))
  }

  test("returns true for an array with equal elements") {
    val as = Array(3, 3, 3)
    assert(isSorted[Int](as, greaterThan))
  }

  test("returns true for a large ordered array") {
    val as = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ,15)
    assert(isSorted[Int](as, greaterThan))
  }

  test("returns false for a large unordered array") {
    val as = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15 ,14)
    assert(!isSorted[Int](as, greaterThan))
  }
}
