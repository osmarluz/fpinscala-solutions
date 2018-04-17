package fpinscala.chapter5

import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(_, t) if n > 0 => t().drop(n - 1)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}