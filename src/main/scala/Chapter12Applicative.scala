import Chapter11Functor.Functor
import Chapter11Monads.Monad

object Chapter12Applicative {

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

    /** Exercise 1 */
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      fas.foldRight(unit(List.empty[A]))((fa, fla) => map2(fa, fla)(_ :: _))

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

    /** Exercise 2 */
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_)) //(f, a) => f(a)
    def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(mapViaApply(fa)(f.curried))(fb)

    /** Exercise 3 */
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(mapViaApply(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(mapViaApply(fa)(f.curried))(fb))(fc))(fd)

    /** Exercise 8 */
    def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] =
      new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
        def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), G.unit(a))
        def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
          (Applicative.this.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }

  }

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for { a <- fa; b <- fb } yield f(a, b)
  }

  /** Exercise 5 */
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
        case Left(e) => Left(e)
        case Right(value) => f(value)
      }
    }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  /** Exercise 6 */
  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
        f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (f @ Failure(_, _), _) => f
          case (_, f @ Failure(_, _)) => f
          case (Success(a), Success(b)) => Success(f(a, b))
        }
    }

}
