package fpinscala.chapter2

object Exercise2 {
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def scan(ind: Int): Boolean = {
      if (ind >= as.length - 1) true
      else if (gt(as(ind), as(ind + 1))) false
      else scan(ind + 1)
    }

    scan(0)
  }
}
