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
}
