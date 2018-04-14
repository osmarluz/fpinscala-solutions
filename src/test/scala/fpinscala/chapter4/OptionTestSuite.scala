package fpinscala.chapter4

import org.scalatest.FunSuite

class OptionTestSuite extends FunSuite {
  //Exercise 4.1

  test("returns None when map is applied to None") {
    assert(None.map((a: Int) => a + 1) == None)
  }

  test("returns Some mapped according to the input function") {
    assert(Some(5).map(_ + 1) == Some(6))
  }

  test("returns None when flatMap is applied to None") {
    assert(None.flatMap((a: Int) => Some(a + 5)) == None)
  }

  test("returns Some flat mapped according to the input function") {
    assert(Some(5).flatMap(a => Some(a + 5)) == Some(10))
  }

  test("returns the default Option when getOrElse is applied to None") {
    assert(None.getOrElse(None) == None)
    assert(None.getOrElse(Some(5)) == Some(5))
  }

  test("returns the Some value after applying getOrElse") {
    assert(Some(5).getOrElse(None) == 5)
  }

  test("returns the default Option when orElse is applied to None") {
    assert(None.orElse(None) == None)
    assert(None.orElse(Some(5)) == Some(5))
  }

  test("returns Some after applying orElse to Some") {
    assert(Some(5).orElse(None) == Some(5))
    assert(Some(5).orElse(Some(5)) == Some(5))
  }

  test("returns None when filter is applied to None") {
    assert(None.filter((a: Int) => a > 5) == None)
  }

  test("returns None when the input function on filter is not satisfied") {
    assert(Some(5).filter(_ > 5) == None)
  }

  test("returns Some when the input function on filter is satisfied") {
    assert(Some(5).filter(_ <= 5) == Some(5))
  }

  //Exercise 4.2

  test("returns the variance of an input sequence") {
    assert(None.variance(List(1, 2, 3, 4)) == Some(1.25))
    assert(Some(1).variance(List(1, 2, 3, 4)) == Some(1.25))
  }

  //Exercise 4.3

  test("returns None when the first input argument is None") {
    assert(None.map2(None: Option[Int], Some(5))(_ + _) == None)
    assert(Some(1).map2(None: Option[Int], Some(5))(_ + _) == None)
  }

  test("returns None when the second input argument is None") {
    assert(None.map2(Some(5), None: Option[Int])(_ + _) == None)
    assert(Some(1).map2(Some(5), None: Option[Int])(_ + _) == None)
  }

  test("returns the output of the input function lifted to the Option type") {
    assert(None.map2(Some(5), Some(5))(_ + _) == Some(10))
    assert(Some(1).map2(Some(5), Some(5))(_ + _) == Some(10))
  }

  //Exercise 4.4

  test("returns Some(Nil) when the input list is Nil") {
    assert(None.sequence(Nil) == Some(Nil))
    assert(Some(1).sequence(Nil) == Some(Nil))
  }

  test("returns None when the input list contains at least one None type") {
    assert(None.sequence(List(Some(1), Some(2), Some(5), None)) == None)
    assert(Some(1).sequence(List(Some(1), Some(2), Some(5), None)) == None)
  }

  test("returns Some(List()) when the input list is a List of Some types") {
    assert(None.sequence(List(Some(1), Some(2), Some(5))) == Some(List(1, 2, 5)))
    assert(Some(1).sequence(List(Some(1), Some(2), Some(5))) == Some(List(1, 2, 5)))
  }

  test("returns Some(Nil) when the input list is Nil using sequence_1") {
    assert(None.sequence_1(Nil) == Some(Nil))
    assert(Some(1).sequence_1(Nil) == Some(Nil))
  }

  test("returns None when the input list contains at least one None type using sequence_1") {
    assert(None.sequence_1(List(Some(1), Some(2), Some(5), None)) == None)
    assert(Some(1).sequence_1(List(Some(1), Some(2), Some(5), None)) == None)
  }

  test("returns Some(List()) when the input list is a List of Some types using sequence_1") {
    assert(None.sequence_1(List(Some(1), Some(2), Some(5))) == Some(List(1, 2, 5)))
    assert(Some(1).sequence_1(List(Some(1), Some(2), Some(5))) == Some(List(1, 2, 5)))
  }

  test("returns Some(Nil) when the input list is Nil using sequenceViaTraverse") {
    assert(None.sequenceViaTraverse(Nil) == Some(Nil))
    assert(Some(1).sequenceViaTraverse(Nil) == Some(Nil))
  }

  test("returns None when the input list contains at least one None type using sequenceViaTraverse") {
    assert(None.sequenceViaTraverse(List(Some(1), Some(2), Some(5), None)) == None)
    assert(Some(1).sequenceViaTraverse(List(Some(1), Some(2), Some(5), None)) == None)
  }

  test("returns Some(List()) when the input list is a List of Some types using sequenceViaTraverse") {
    assert(None.sequenceViaTraverse(List(Some(1), Some(2), Some(5))) == Some(List(1, 2, 5)))
    assert(Some(1).sequenceViaTraverse(List(Some(1), Some(2), Some(5))) == Some(List(1, 2, 5)))
  }

  //Exercise 4.5

  test("returns Some(Nil) when the input list is Nil for traverse") {
    assert(None.traverse(Nil)(e => Some(e)) == Some(Nil))
    assert(Some(1).traverse(Nil)(e => Some(e)) == Some(Nil))
  }

  test("returns None when the input function maps to at least one None element") {
    assert(None.traverse(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else None) == None)
    assert(Some(1).traverse(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else None) == None)
  }

  test("returns Some(List()) when the input function maps to no None element") {
    assert(None.traverse(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else Some(0)) == Some(List(0, 2, 0, 4, 0)))
    assert(Some(1).traverse(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else Some(0)) == Some(List(0, 2, 0, 4, 0)))
  }

  test("returns Some(Nil) when the input list is Nil for traverse_1") {
    assert(None.traverse_1(Nil)(e => Some(e)) == Some(Nil))
    assert(Some(1).traverse_1(Nil)(e => Some(e)) == Some(Nil))
  }

  test("returns None when the input function maps to at least one None element using traverse_1") {
    assert(None.traverse_1(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else None) == None)
    assert(Some(1).traverse_1(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else None) == None)
  }

  test("returns Some(List()) when the input function maps to no None element using traverse_1") {
    assert(None.traverse_1(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else Some(0)) == Some(List(0, 2, 0, 4, 0)))
    assert(Some(1).traverse_1(List(1, 2, 3, 4, 5))(e => if (e % 2 == 0) Some(e) else Some(0)) == Some(List(0, 2, 0, 4, 0)))
  }
}
