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

  /** Exercise 7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case eitherH::eitherT => eitherH.flatMap(h => sequence(eitherT).map(h::_))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))(_::_)
  }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)

}
