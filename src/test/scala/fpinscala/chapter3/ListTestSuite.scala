package fpinscala.chapter3

import org.scalatest.FunSuite

class ListTestSuite extends FunSuite {
  //Exercise 3.2

  test("returns system error when trying to get the tail of an empty list") {
    assertThrows [RuntimeException] {
      List.tail(Nil)
    }
  }

  test("returns tail part of a non-empty list") {
    assert(List.tail(List(1, 2, 3, 4)) == List(2, 3, 4))
  }

  //Exercise 3.3

  test("returns system error when trying to set the head of an empty list") {
    assertThrows [RuntimeException] {
      List.setHead(1, Nil)
    }
  }

  test("returns a list with the head replaced for a non-empty input list") {
    assert(List.setHead(8, List(1, 2, 3, 4)) == List(8, 2, 3, 4))
  }

  //Exercise 3.4

  test("returns the list on the input when the number of elements to drop is < 0") {
    assert(List.drop(List(1, 2, 3, 4), -1) == List(1, 2, 3, 4))
    assert(List.drop(Nil, -1) == Nil)
  }

  test("returns the list on the input when the number of elements to drop is = 0") {
    assert(List.drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
    assert(List.drop(Nil, 0) == Nil)
  }

  test("returns Nil when the input list in drop is Nil") {
    assert(List.drop(Nil, 1) == Nil)
  }

  test("returns Nil when the the number of elements to drop is > than the input list size") {
    assert(List.drop(List(1, 2, 3, 4), 5) == Nil)
  }

  test("returns a list with the n first elements dropped") {
    assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
  }

  //Exercise 3.5

  test("returns Nil when the input list in dropWhile is Nil") {
    assert(List.dropWhile(Nil, (a: Int) => a < 5) == Nil)
  }

  test("returns a list that matches the input predicate") {
    assert(List.dropWhile(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (a: Int) => a < 5) == List(5, 6, 7, 8, 9, 10))
    assert(List.dropWhile(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), (a: Int) => a > 5) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  //Exercise 3.6

  test("returns error when the input list in init is Nil") {
    assertThrows [RuntimeException] {
      List.init(Nil)
    }
  }

  test("returns Nil when the input list contains just one element") {
    assert(List.init(List(1)) == Nil)
  }

  test("returns a list with the last element removed") {
    assert(List.init(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
}
