package fpinscala.chapter4

import org.scalatest.FunSuite

class OptionTestSuite extends FunSuite {
  //Exercise 4.1

  test("returns None when map is applied to None") {
    assert(None.map((a: Int) => a + 1) == None)
  }

  test("returns the Option mapped according to the input function") {
    assert(Some(5).map(_ + 1) == Some(6))
  }

  test("returns None when flatMap is applied to None") {
    assert(None.flatMap((a: Int) => Some(a + 5)) == None)
  }

  test("returns the Option flat mapped according to the input function") {
    assert(Some(5).flatMap(a => Some(a + 5)) == Some(10))
  }

  test("returns the default Option when getOrElse is applied to None") {
    assert(None.getOrElse(None) == None)
    assert(None.getOrElse(Some(5)) == Some(5))
  }

  test("returns the Option value after applying getOrElse") {
    assert(Some(5).getOrElse(None) == 5)
  }

  test("returns the default value when orElse is applied to None") {
    assert(None.orElse(None) == None)
    assert(None.orElse(Some(5)) == Some(5))
  }

  test("returns the input Option after applying orElse") {
    assert(Some(5).orElse(None) == Some(5))
    assert(Some(5).orElse(Some(5)) == Some(5))
  }

  test("returns None when filter is applied to None") {
    assert(None.filter((a: Int) => a > 5) == None)
  }

  test("returns None when the input function on filter is not satisfied") {
    assert(Some(5).filter(_ > 5) == None)
  }

  test("returns the input Option when the input function on filter is satisfied") {
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
}
