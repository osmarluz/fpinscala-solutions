package fpinscala.chapter5

import org.scalatest.FunSuite

class StreamTestSuite extends FunSuite {
  //Exercise 5.1

  test("returns empty list when toList is applied to an empty stream") {
    assert(Empty.toList == Nil)
    assert(Empty.toList == List())
    assert(Stream().toList == Nil)
    assert(Stream().toList == List())
  }

  test("returns the stream converted to a list when toList is applied to a non-empty stream") {
    assert(Stream(1, 2, 3, 4, 5).toList == List(1, 2, 3, 4, 5))
  }
}