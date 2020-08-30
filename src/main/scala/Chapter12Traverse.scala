import Chapter10Foldable.Foldable
import Chapter11Functor.Functor
import Chapter12Applicative.{Applicative, stateMonad}
import Chapter6State.State
import Chapter6State.State.{get, set}

object Chapter12Traverse {

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

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(stateMonad)

    def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
      traverseS(ta)((a: A) => for { i <- get[Int]; _ <- set(i + 1) } yield (a, i))
        .run(0)
        ._1

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)(
        (a: A) =>
          for {
            s1 <- get[S]
            (b, s2) = f(a, s1)
            _ <- set(s2)
          } yield b
      ).run(s)

    override def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def zipWithIndexViaMapAccum[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

    /** Exercise 16 */
    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
  }

  /** Exercise 13 */
  val listTraverse: Traverse[List] = new Traverse[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

}
