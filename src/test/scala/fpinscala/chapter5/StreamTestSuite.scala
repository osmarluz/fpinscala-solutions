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

  test("returns false when exists is applied to an empty stream") {
    assert(!Empty.exists((n: Int) => n > 5))
  }

  test("returns true when the exists predicate is satisfied for an element") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).exists((n: Int) => n < 5))
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).exists((n: Int) => n > 5))
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).exists((n: Int) => n >= 10))
  }

  test("returns false when the exists predicate is not satisfied for all elements") {
    assert(!Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).exists((n: Int) => n == 11))
  }

  //Exercise 5.4

  test("returns true when forAll is applied to an empty stream") {
    assert(Empty.forAll((n: Int) => n > 5))
  }

  test("returns false when the forAll predicate is not satisfied for an element") {
    assert(!Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).forAll((n: Int) => n > 5))
    assert(!Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).forAll((n: Int) => n < 5))
    assert(!Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).forAll((n: Int) => n < 10))
  }

  test("returns true when the forAll predicate is satisfied for all elements") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).forAll((n: Int) => n <= 10))
  }

  //Exercise 5.5

  test("returns empty stream when takeWhile_1 is applied to an empty stream") {
    assert(Empty.takeWhile_1((n: Int) => n > 5) == Empty)
    assert(Stream().takeWhile_1((n: Int) => n > 5) == Empty)
  }

  test("returns all starting elements of the original stream that match the given predicate using takeWhile_1") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile_1((n: Int) => n > 5) == Empty)
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile_1((n: Int) => n < 5).toListRecursive == Stream(1, 2, 3, 4).toListRecursive)
  }

  //Exercise 5.6

  test("returns None when headOption is applied to an empty stream") {
    assert(Empty.headOption.isEmpty)
    assert(Stream().headOption.isEmpty)
  }

  test("returns the head of the stream within the Some type") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).headOption.contains(1))
  }

  //Exercise 5.7

  test("returns Empty when map is applied to an empty stream") {
    assert(Empty.map((a: Int) => a + 1) == Empty)
    assert(Stream().map((a: Int) => a + 1) == Empty)
  }

  test("returns a stream with 1 added to each element using map") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map((a: Int) => a + 1).toListRecursive == Stream(2, 3, 4, 5, 6, 7, 8, 9, 10, 11).toListRecursive)
  }

  test("returns Empty when filter is applied to an empty stream") {
    assert(Empty.filter((a: Int) => a < 5) == Empty)
    assert(Stream().filter((a: Int) => a < 5) == Empty)
  }

  test("returns a filtered list with elements that match the input predicate") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter((a: Int) => a < 5).toListRecursive == Stream(1, 2, 3, 4).toListRecursive)
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter((a: Int) => a > 5).toListRecursive == Stream(6, 7, 8, 9, 10).toListRecursive)
  }

  test("returns input list when the stream is empty") {
    assert(Empty.append(Stream(5, 6, 7, 8)).toListRecursive == Stream(5, 6, 7, 8).toListRecursive)
    assert(Stream().append(Stream(5, 6, 7, 8)).toListRecursive == Stream(5, 6, 7, 8).toListRecursive)
  }

  test("returns the stream when the input stream is empty") {
    assert(Stream(1, 2, 3, 4).append(Empty).toListRecursive == Stream(1, 2, 3, 4).toListRecursive)
    assert(Stream(1, 2, 3, 4).append(Empty).toListRecursive == Stream(1, 2, 3, 4).toListRecursive)
  }

  test("returns the streams appended") {
    assert(Stream(1, 2, 3, 4).append(Stream(5, 6, 7, 8)).toListRecursive == Stream(1, 2, 3, 4, 5, 6, 7, 8).toListRecursive)
  }

  test("returns Empty when flatMap is applied to an empty stream") {
    assert(Empty.flatMap(i => Stream(i, i)) == Empty)
    assert(Stream().flatMap(i => Stream(i, i)) == Empty)
  }

  test("returns a list with duplicated elements from the original list") {
    assert(Stream(5, 6, 7, 8).flatMap(i => Stream(i, i)).toListRecursive == Stream(5, 5, 6, 6, 7, 7, 8, 8).toListRecursive)
  }

  //Exercise 5.8

  test("returns a list generated with the constant method defined for Stream") {
    assert(Stream.constant(5).take(4).toList == List(5, 5, 5, 5))
  }

  //Exercise 5.9

  test("returns a list generated with the from method defined for Stream") {
    assert(Stream.from(5).take(4).toList == List(5, 6, 7, 8))
  }

  //Exercise 5.10

  test("returns a list with the first Fibonacci number") {
    assert(Stream.fibs.take(1).toList == List(0))
  }

  test("returns a list with the first two Fibonacci number") {
    assert(Stream.fibs.take(2).toList == List(0, 1))
  }

  test("returns a list with the first ten Fibonacci numbers") {
    assert(Stream.fibs.take(10).toList == List(0, 1, 1,	2, 3,	5, 8, 13, 21, 34 ))
  }

  //Exercise 5.12

  test("returns a list with the first Fibonacci number using fibsViaUnfold") {
    assert(Stream.fibsViaUnfold.take(1).toList == List(0))
  }

  test("returns a list with the first two Fibonacci number using fibsViaUnfold") {
    assert(Stream.fibsViaUnfold.take(2).toList == List(0, 1))
  }

  test("returns a list with the first ten Fibonacci numbers using fibsViaUnfold") {
    assert(Stream.fibsViaUnfold.take(10).toList == List(0, 1, 1,	2, 3,	5, 8, 13, 21, 34 ))
  }

  test("returns a list generated with the fromViaUnfold method defined for Stream") {
    assert(Stream.fromViaUnfold(5).take(4).toList == List(5, 6, 7, 8))
  }

  test("returns a list generated with the constant method defined for Stream using constantViaUnfold") {
    assert(Stream.constantViaUnfold(5).take(4).toList == List(5, 5, 5, 5))
  }

  test("returns a list generated from the onesViaUnfold val defined for Stream") {
    assert(Stream.onesViaUnfold.take(4).toList == List(1, 1, 1, 1))
  }

  //Exercise 5.13

  test("returns Empty when mapViaUnfold is applied to an empty stream") {
    assert(Empty.mapViaUnfold((a: Int) => a + 1) == Empty)
    assert(Stream().mapViaUnfold((a: Int) => a + 1) == Empty)
  }

  test("returns a stream with 1 added to each element using mapViaUnfold") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).mapViaUnfold((a: Int) => a + 1).toListRecursive == Stream(2, 3, 4, 5, 6, 7, 8, 9, 10, 11).toListRecursive)
  }

  test("returns empty stream when takeViaUnfold is applied to an empty stream") {
    assert(Empty.takeViaUnfold(1) == Empty)
    assert(Stream().takeViaUnfold(1) == Empty)
  }

  test("returns the first n elements of the original stream when takeViaUnfold is applied to a non-empty stream") {
    assert(Stream(1).takeViaUnfold(1).toListRecursive == Stream(1).toListRecursive)
    assert(Stream(1).takeViaUnfold(10).toListRecursive == Stream(1).toListRecursive)
    assert(Stream(1, 2, 3, 4, 5).takeViaUnfold(3).toListRecursive == Stream(1, 2, 3).toListRecursive)
    assert(Stream(1, 2, 3, 4, 5).takeViaUnfold(10).toListRecursive == Stream(1, 2, 3, 4, 5).toListRecursive)
  }

  test("returns empty stream when takeWhileViaUnfold is applied to an empty stream") {
    assert(Empty.takeWhileViaUnfold((n: Int) => n > 5) == Empty)
    assert(Stream().takeWhileViaUnfold((n: Int) => n > 5) == Empty)
  }

  test("returns all starting elements of the original stream that match the given predicate using takeWhileViaUnfold") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhileViaUnfold((n: Int) => n > 5) == Empty)
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhileViaUnfold((n: Int) => n < 5).toListRecursive == Stream(1, 2, 3, 4).toListRecursive)
  }

  test("returns Empty when zipWith is applied to an empty stream") {
    assert(Empty.zipWith(Stream(1, 2, 3, 4))((a: Int, b: Int) => a + b) == Empty)
    assert(Stream(1, 2, 3, 4).zipWith(Empty)((a: Int, b: Int) => a + b) == Empty)
  }

  test("returns a stream with the elements of the streams added pairwise using zipWith") {
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).zipWith(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ + _).toListRecursive == Stream(2, 4, 6, 8, 10, 12, 14, 16, 18, 20).toListRecursive)
  }

  test("returns Stream(Some, Some) when zipAll is applied to a non-empty stream with a non-empty argument") {
    assert(Stream(1, 2, 3, 4).zipAll(Stream(1, 2, 3, 4)).toListRecursive == Stream((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)), (Some(4), Some(4))).toListRecursive)
  }

  test("returns Stream(Some, None) when zipAll is applied to a non-empty stream with an empty argument") {
    assert(Stream(1, 2, 3, 4).zipAll(Empty).toListRecursive == Stream((Some(1), None), (Some(2), None), (Some(3), None), (Some(4), None)).toListRecursive)
  }

  test("returns Stream(None, Some) when zipAll is applied to an empty stream with a non-empty argument") {
    assert(Empty.zipAll(Stream(1, 2, 3, 4)).toListRecursive == Stream((None, Some(1)), (None, Some(2)), (None, Some(3)), (None, Some(4))).toListRecursive)
  }

  test("returns Stream(None, None) when zipAll is applied to an empty stream with an empty argument") {
    assert(Empty.zipAll(Empty).toListRecursive == Stream().toListRecursive)
  }
}