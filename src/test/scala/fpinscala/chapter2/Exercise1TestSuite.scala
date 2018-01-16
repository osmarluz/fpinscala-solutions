package fpinscala.chapter2

import org.scalatest.FunSuite
import Exercise1.fib

class Exercise1TestSuite extends FunSuite {
  test("returns 0 for the 1st Fibonacci number") {
    assert(fib(0) == 0)
  }

  test("returns 1 for the 2nd Fibonacci number") {
    assert(fib(1) == 1)
  }

  test("returns 1 for the 3rd Fibonacci number") {
    assert(fib(2) == 1)
  }

  test("returns 2 for the 4th Fibonacci number") {
    assert(fib(3) == 2)
  }

  test("returns 3 for the 5th Fibonacci number") {
    assert(fib(4) == 3)
  }

  test("returns 5 for the 6th Fibonacci number") {
    assert(fib(5) == 5)
  }
}
