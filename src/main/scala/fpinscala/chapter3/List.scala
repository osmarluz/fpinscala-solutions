package fpinscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of an empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](x: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead on an empty list")
    case Cons(_, xs) => Cons(x, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => l
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def dropLoop(l: List[A]): List[A] = l match {
      case Cons(x, xs) if f(x) => dropLoop(xs)
      case _ => l
    }

    dropLoop(l)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on an empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =  a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)
}