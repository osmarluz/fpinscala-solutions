package fpinscala.chapter2

object Exercise1 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibCalc(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else fibCalc(n - 1, curr, prev + curr)
    }

    fibCalc(n, 0, 1)
  }
}
