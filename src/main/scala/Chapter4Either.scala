object Chapter4Either {

  /** Exercise 6 */
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Left(e) => Left(e)
      case Right(a) => b.map(f(a, _))
    }

    def map2viaFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for { bb <- b; a <- this } yield f(a, bb)

  }

  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

}
