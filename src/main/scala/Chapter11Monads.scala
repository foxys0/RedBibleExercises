import Chapter11Functor.Functor
import Chapter5Stream.Stream
import Chapter7Parallelism.Par
import Chapter8GenProp.Gen

object Chapter11Monads {

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    /** Exercise 3 */
    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List.empty[A]))((ma, mla) => map2(ma, mla)(_ :: _))

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List.empty[B]))((a, mlb) => map2(f(a), mlb)(_ :: _))

  }

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  /** Exercise 1 */
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

}