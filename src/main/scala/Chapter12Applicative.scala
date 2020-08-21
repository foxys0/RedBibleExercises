import Chapter11Functor.Functor

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

  }

}
