package fpinscala.chapter2

object Exercise3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => f(a,_)
  }
}
