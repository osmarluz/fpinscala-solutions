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

  test("returns a list containing the input head for an empty input list") {
    assertThrows [RuntimeException] {
      List.setHead(1, Nil)
    }
  }

  test("returns a list with the head replaced for a non-empty input list") {
    assert(List.setHead(8, List(1, 2, 3, 4)) == List(8, 2, 3, 4))
  }
}
