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

  //Exercise 3.9

  test("returns 0 when the input list is Nil") {
    assert(List.length(Nil) == 0)
  }

  test("returns the length of the input list") {
    assert(List.length(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == 10)
  }

  //Exercise 3.11

  test("returns sum = 0 when the input list is Nil using sum3 function") {
    assert(List.sum3(Nil) == 0)
  }

  test("returns the sum of the elements on the input list using sum3 function") {
    assert(List.sum3(List(1, 2, 3 ,4)) == 10)
  }

  test("returns product = 1 when the input list is Nil using product3 function") {
    assert(List.product3(Nil) == 1)
  }

  test("returns the product of the elements on the input list using product3 function") {
    assert(List.product3(List(1.0, 2.0, 3.0 ,4.0)) == 24.0)
  }

  test("returns 0 when the input list is Nil using length2 function") {
    assert(List.length2(Nil) == 0)
  }

  test("returns the length of the input list using length2 function") {
    assert(List.length2(List(1, 2, 3 ,4)) == 4)
  }

  //Exercise 3.12

  test("returns Nil when reverse is applied on an empty list") {
    assert(List.reverse(Nil) == Nil)
  }

  test("returns a list in reverse order") {
    assert(List.reverse(List(1, 2, 3 ,4)) == List(4, 3, 2 ,1))
  }

  //Exercise 3.14

  test("returns second input list when the first input list is Nil using appendViaFoldRight") {
    assert(List.appendViaFoldRight(Nil, List(5, 6, 7, 8)) == List(5, 6, 7, 8))
  }

  test("returns first input list when the second input list is Nil using appendViaFoldRight") {
    assert(List.appendViaFoldRight(List(1, 2, 3, 4), Nil) == List(1, 2, 3, 4))
  }

  test("returns the two input lists appended using appendViaFoldRight") {
    assert(List.appendViaFoldRight(List(1, 2, 3, 4), List(5, 6, 7, 8)) == List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("returns second input list when the first input list is Nil using appendViaFoldLeft") {
    assert(List.appendViaFoldLeft(Nil, List(5, 6, 7, 8)) == List(5, 6, 7, 8))
  }

  test("returns first input list when the second input list is Nil using appendViaFoldLeft") {
    assert(List.appendViaFoldLeft(List(1, 2, 3, 4), Nil) == List(1, 2, 3, 4))
  }

  test("returns the two input lists appended using appendViaFoldLeft") {
    assert(List.appendViaFoldLeft(List(1, 2, 3, 4), List(5, 6, 7, 8)) == List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  //Exercise 3.15

  test("returns the concatenation of a list of lists (with a Nil second list) as a single list") {
    assert(List.concat(List(List(1, 2, 3, 4), Nil)) == List(1, 2, 3, 4))
  }

  test("returns the concatenation of a list of lists (with a Nil first list) as a single list") {
    assert(List.concat(List(Nil, List(5, 6, 7, 8))) == List(5, 6, 7, 8))
  }

  test("returns the concatenation of a list of lists as a single list") {
    assert(List.concat(List(List(1, 2, 3, 4), Nil, List(5, 6, 7, 8))) == List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  //Exercise 3.16

  test("returns Nil when add1 is applied to an empty list") {
    assert(List.add1(Nil) == Nil)
  }

  test("returns a list with 1 added to each element") {
    assert(List.add1(List(5, 6, 7, 8)) == List(6, 7, 8, 9))
  }

  //Exercise 3.17

  test("returns Nil when doubleToString is applied to an empty list") {
    assert(List.doubleToString(Nil) == Nil)
  }

  test("returns a list with elements converted to String") {
    assert(List.doubleToString(List(1.0, 2.0, 3.0, 4.0)) == List("1.0", "2.0", "3.0", "4.0"))
  }

  //Exercise 3.18

  test("returns Nil when map is applied to an empty list using _ + 1 as the input function") {
    assert(List.map(Nil: List[Int])(_ + 1) == Nil)
  }

  test("returns a list with 1 added to each element using map") {
    assert(List.map(List(5, 6, 7, 8))(_ + 1) == List(6, 7, 8, 9))
  }

  test("returns Nil when map is applied to an empty list using _.toString as the input function") {
    assert(List.map(Nil: List[Double])(_.toString) == Nil)
  }

  test("returns a list with elements converted to String using map") {
    assert(List.map(List(1.0, 2.0, 3.0, 4.0))(_.toString) == List("1.0", "2.0", "3.0", "4.0"))
  }

  //Exercise 3.19

  test("returns Nil when the input list in filter is Nil") {
    assert(List.filter(Nil)((a: Int) => a < 5) == Nil)
  }

  test("returns a filtered list with elements that match the input predicate") {
    assert(List.filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))((a: Int) => a < 5) == List(1, 2, 3, 4))
    assert(List.filter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))((a: Int) => a > 5) == List(6, 7, 8, 9, 10))
  }

  //Exercise 3.20

  test("returns Nil when flatMap is applied to an empty list") {
    assert(List.flatMap(Nil: List[Int])(i => List(i, i)) == Nil)
  }

  test("returns a list with duplicated elements from the original list") {
    assert(List.flatMap(List(5, 6, 7, 8))(i => List(i, i)) == List(5, 5, 6, 6, 7, 7, 8, 8))
  }

  //Exercise 3.21

  test("returns Nil when the input list in filterUsingFlatMap is Nil") {
    assert(List.filterViaFlatMap(Nil)((a: Int) => a < 5) == Nil)
  }

  test("returns a filtered list (using filterUsingFlatMap) with elements that match the input predicate") {
    assert(List.filterViaFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))((a: Int) => a < 5) == List(1, 2, 3, 4))
    assert(List.filterViaFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))((a: Int) => a > 5) == List(6, 7, 8, 9, 10))
  }

  //Exercise 3.22

  test("returns the second input list when the first input list in addPairwise is Nil") {
    assert(List.addPairwise(Nil, List(1, 2, 3, 4)) == List(1, 2, 3, 4))
    assert(List.addPairwise(Nil, Nil) == Nil)
  }

  test("returns the first input list when the second input list in addPairwise is Nil") {
    assert(List.addPairwise(List(1, 2, 3, 4), Nil) == List(1, 2, 3, 4))
    assert(List.addPairwise(Nil, Nil) == Nil)
  }

  test("returns a list with the elements of the input lists added pairwise") {
    assert(List.addPairwise(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
  }

  //Exercise 3.23

  test("returns Nil list when the first input list in zipWith is Nil") {
    assert(List.zipWith(Nil: List[Int], List(1, 2, 3, 4))(_ + _) == Nil)
    assert(List.zipWith(Nil: List[Int], Nil)(_ + _) == Nil)
  }

  test("returns Nil list when the second input list in zipWith is Nil") {
    assert(List.zipWith(List(1, 2, 3, 4), Nil: List[Int])(_ + _) == Nil)
    assert(List.zipWith(Nil: List[Int], Nil)(_ + _) == Nil)
  }

  test("returns a list with the elements of the input lists added pairwise using zipWith") {
    assert(List.zipWith(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ + _) == List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
  }

  //Exercise 3.24

  test("returns true when the sup list is Nil and the sub list is also Nil") {
    assert(List.hasSubsequence(Nil, Nil))
  }

  test("returns false when the sup list is Nil and the sub list is not Nil") {
    assert(!List.hasSubsequence(Nil, List(1, 2, 3, 4)))
  }

  test("returns true for a sub list that is in the start of the sup list") {
    assert(List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(1, 2, 3, 4)))
  }

  test("returns true for a sub list that is in the middle of the sup list") {
    assert(List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(4, 5, 6, 7)))
  }

  test("returns false for a sub list that is not a sublist of sup") {
    assert(!List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(9, 10, 11, 12)))
  }
}