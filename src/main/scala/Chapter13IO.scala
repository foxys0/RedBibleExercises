import Chapter13Monad.Monad

object Chapter13IO {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  /** Exercise 1 */
  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f] {
      def unit[A](a: => A): Free[F, A] = Return(a)
      def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
    }

  /** Exercise 2
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) =>
        runTrampoline(y.flatMap(a0 => g(a0).flatMap(f)) )
    }
  }
  */

}
