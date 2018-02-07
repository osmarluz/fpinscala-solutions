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

  //Exercise 3.26

  test("returns the value of the Leaf when the input list is a Leaf") {
    assert(Tree.maximum(Leaf(5)) == 5)
  }

  test("returns the maximum value of a more complex tree structure") {
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(4), Leaf(12)))) == 12)
  }

  //Exercise 3.27

  test("returns 0 as the depth when the input tree is a Leaf") {
    assert(Tree.depth(Leaf(5)) == 0)
  }

  test("returns the depth of a more complex tree structure") {
    assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(4), Branch(Leaf(1), Leaf(15))))) == 3)
  }
}
