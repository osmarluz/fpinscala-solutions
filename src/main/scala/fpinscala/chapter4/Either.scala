package fpinscala.chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = map(f) match {
    case Left(e) => Left(e)
    case Right(a) => a
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this map (Right(_)) match {
    case Left(_) => b
    case Right(a) => a
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for { a <- this; b1 <- b } yield f(a, b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]