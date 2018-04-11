package fpinscala.chapter4

import org.scalatest.FunSuite

class EitherTestSuite extends FunSuite {
  //Exercise 4.6

  val e = new Exception("Test")

  test("returns Left(e) when map is applied to Left(e)") {
    assert(Left(e).map((a: Int) => a + 1) == Left(e))
  }

  test("returns Right mapped according to the input function") {
    assert(Right(5).map(_ + 1) == Right(6))
  }

  test("returns Left(e) when flatMap is applied to Left(e)") {
    assert(Left(e).flatMap((a: Int) => Right(a + 5)) == Left(e))
  }

  test("returns Right flat mapped according to the input function") {
    assert(Right(5).flatMap(a => Right(a + 5)) == Right(10))
  }

  test("returns the default value when orElse is applied to Left(e)") {
    assert(Left(e).orElse(Left(e)) == Left(e))
    assert(Left(e).orElse(Right(5)) == Right(5))
  }

  test("returns the input Either after applying orElse") {
    assert(Right(5).orElse(Left(e)) == Right(5))
    assert(Right(5).orElse(Right(10)) == Right(5))
  }

  test("returns Left(e) when the input argument is Left(e)") {
    assert(Left(e).map2(Left(e))((a: Int, b: Int) => a + b) == Left(e))
    assert(Right(1).map2(Left(e))(_ + _) == Left(e))
  }

  test("returns Left(e) when map2 is applied to Left(e)") {
    assert(Left(e).map2(Right(5))((a: Int, b: Int) => a + b) == Left(e))
  }

  test("returns the output of the input function lifted to the Either type") {
    assert(Right(5).map2(Right(5))(_ + _) == Right(10))
  }

  //Exercise 4.7

  test("returns Right(Nil) when the input list is Nil") {
    assert(Left(e).sequence(Nil) == Right(Nil))
    assert(Right(1).sequence(Nil) == Right(Nil))
  }

  test("returns the first Left encountered on the input list") {
    val ee = new Exception("Test2");
    assert(Left(e).sequence(List(Right(1), Right(2), Right(5), Left(e))) == Left(e))
    assert(Left(e).sequence(List(Left(ee), Right(2), Right(5), Left(e))) == Left(ee))
    assert(Right(1).sequence(List(Right(1), Right(2), Right(5), Left(e))) == Left(e))
    assert(Right(1).sequence(List(Left(ee), Right(2), Right(5), Left(e))) == Left(ee))
  }

  test("returns Right(List()) when the input list is a List of Right types") {
    assert(Left(e).sequence(List(Right(1), Right(2), Right(5))) == Right(List(1, 2, 5)))
    assert(Right(1).sequence(List(Right(1), Right(2), Right(5))) == Right(List(1, 2, 5)))
  }

  test("returns Right(Nil) when the input list is Nil for traverse") {
    assert(Left(e).traverse(Nil)(a => Right(a)) == Right(Nil))
    assert(Right(1).traverse(Nil)(a => Right(a)) == Right(Nil))
  }

  test("returns Left(e) when the input function maps to at least one Left element") {
    assert(Left(e).traverse(List(1, 2, 3, 4, 5))(a => if (a % 2 == 0) Right(a) else Left(e)) == Left(e))
    assert(Right(1).traverse(List(1, 2, 3, 4, 5))(a => if (a % 2 == 0) Right(a) else Left(e)) == Left(e))
  }

  test("returns Right(List()) when the input function maps to no Left element") {
    assert(Left(e).traverse(List(2, 4, 6, 8, 10))(a => if (a % 2 == 0) Right(a) else Left(e)) == Right(List(2, 4, 6, 8, 10)))
    assert(Right(1).traverse(List(2, 4, 6, 8, 10))(a => if (a % 2 == 0) Right(a) else Left(e)) == Right(List(2, 4, 6, 8, 10)))
  }
}
