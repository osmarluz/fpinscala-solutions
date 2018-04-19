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

  //Exercise 5.2

  test("returns empty stream when take is applied to an empty stream") {
    assert(Empty.take(1) == Empty)
    assert(Stream().take(1) == Empty)
  }

  test("returns the first n elements of the original stream when take is applied to a non-empty stream") {
    assert(Stream(1).take(1).toList == Stream(1).toList)
    assert(Stream(1).take(10).toList == Stream(1).toList)
    assert(Stream(1, 2, 3, 4, 5).take(3).toList == Stream(1, 2, 3).toList)
    assert(Stream(1, 2, 3, 4, 5).take(10).toList == Stream(1, 2, 3, 4, 5).toList)
  }

  test("returns empty stream when drop is applied to an empty stream") {
    assert(Empty.drop(1) == Empty)
    assert(Stream().drop(1) == Empty)
  }

  test("returns empty stream when the number of elements to drop is equal or larger than the stream size") {
    assert(Stream(5).drop(1) == Empty)
    assert(Stream(5).drop(10) == Empty)
    assert(Stream(1, 2, 3, 4, 5).drop(10) == Empty)
  }

  test("discards the first n elements of the original stream when the number of elements to drop is smaller than the stream size") {
    assert(Stream(1, 2, 3, 4, 5).drop(1).toList == Stream(2, 3, 4, 5).toList)
    assert(Stream(1, 2, 3, 4, 5).drop(3).toList == Stream(4, 5).toList)
  }
}