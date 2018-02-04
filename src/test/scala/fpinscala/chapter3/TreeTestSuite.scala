package fpinscala.chapter3

import org.scalatest.FunSuite

class TreeTestSuite extends FunSuite {
  //Exercise 3.25

  test("returns 1 as the size when the input list is a Leaf") {
    assert(Tree.size(Leaf(1)) == 1)
  }

  test("returns the number of elements of a more complex tree structure") {
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))) == 7)
  }
}
