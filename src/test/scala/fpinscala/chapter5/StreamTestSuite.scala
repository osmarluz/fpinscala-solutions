package fpinscala.chapter5

import org.scalatest.FunSuite

class StreamTestSuite extends FunSuite {
  //Exercise 5.1

  test("returns empty list when toListRecursive is applied to an empty stream") {
    assert(Empty.toListRecursive == Nil)
    assert(Empty.toListRecursive == List())
    assert(Stream().toListRecursive == Nil)
    assert(Stream().toListRecursive == List())
  }

  test("returns the stream converted to a list when toListRecursive is applied to a non-empty stream") {
    assert(Stream(1, 2, 3, 4, 5).toListRecursive == List(1, 2, 3, 4, 5))
  }

  //Exercise 5.2

  test("returns empty stream when take is applied to an empty stream") {
    assert(Empty.take(1) == Empty)
    assert(Stream().take(1) == Empty)
  }

  test("returns the first n elements of the original stream when take is applied to a non-empty stream") {
    assert(Stream(1).take(1).toListRecursive == Stream(1).toListRecursive)
    assert(Stream(1).take(10).toListRecursive == Stream(1).toListRecursive)
    assert(Stream(1, 2, 3, 4, 5).take(3).toListRecursive == Stream(1, 2, 3).toListRecursive)
    assert(Stream(1, 2, 3, 4, 5).take(10).toListRecursive == Stream(1, 2, 3, 4, 5).toListRecursive)
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
    assert(Stream(1, 2, 3, 4, 5).drop(1).toListRecursive == Stream(2, 3, 4, 5).toListRecursive)
    assert(Stream(1, 2, 3, 4, 5).drop(3).toListRecursive == Stream(4, 5).toListRecursive)
  }

  //Exercise 5.3

  test("returns empty stream when takeWhile is applied to an empty stream") {
    assert(Empty.takeWhile((n: Int) => n > 5) == Empty)
    assert(Stream().takeWhile((n: Int) => n > 5) == Empty)
  }

  test("returns all starting elements of the original stream that match the given predicate") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile((n: Int) => n > 5) == Empty)
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile((n: Int) => n < 5).toListRecursive == Stream(1, 2, 3, 4).toListRecursive)
  }
}