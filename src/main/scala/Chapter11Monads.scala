import Chapter11Functor.Functor
import Chapter5Stream.Stream
import Chapter6State.State
import Chapter7Parallelism.Par
import Chapter8GenProp.Gen

object Chapter11Monads {

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    /** Exercise 3 */
    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List.empty[A]))((ma, mla) => map2(ma, mla)(_ :: _))

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List.empty[B]))((a, mlb) => map2(f(a), mlb)(_ :: _))

    /** Exercise 4 */
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    /** Exercise 7 */
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    /** Exercise 12 */
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
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

  /** Exercise 17 */
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonad[S]: Monad[({ type f[x] = State[S, x] })#f] =
    new Monad[({ type f[x] = State[S, x] })#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
    }

  def getState[S]: State[S, S] = State(s => (s, s))
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(stateMonad[Int].unit(List[(Int, A)]()))((acc, a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs)
      .run(0)
      ._1
      .reverse

  case class Reader[R, A](run: R => A)

  def readerMonad[R]: Monad[({ type f[x] = Reader[R, x] })#f] =
    new Monad[({ type f[x] = Reader[R, x] })#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }

}
