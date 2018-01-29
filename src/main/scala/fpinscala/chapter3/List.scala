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

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = {
    foldLeft(ns, 0)((x, y) => x + y)
  }

  def product3(ns: List[Double]) = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((h, l) => Cons(h, l))
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((l, h) => Cons(h, l))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil: List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = {
    foldLeft(reverse(l), Nil: List[Int])((b, a) => Cons(a + 1, b))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldLeft(reverse(l), Nil: List[String])((b, a) => Cons(a.toString, b))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldLeft(reverse(as), Nil: List[B])((b, a) => Cons(f(a), b))
  }
}