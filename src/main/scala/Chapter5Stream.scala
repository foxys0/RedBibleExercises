import Chapter5Stream.Stream._

import scala.annotation.tailrec

object Chapter5Stream {

  trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def reverse: Stream[A] = {
      @tailrec
      def loop(stream: Stream[A], acc: Stream[A]): Stream[A] = stream match {
        case Cons(h, t) => loop(t(), cons(h(), acc))
        case _ => acc
      }

      loop(this, empty)
    }

    /** Exercise 1 */
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List.empty
    }

    def toListTailRec: List[A] = {
      @tailrec
      def loop(stream: Stream[A], list: List[A]): List[A] = stream match {
        case Cons(h, t) => loop(t(), h() :: list)
        case _ => list
      }

      loop(this, List()).reverse
    }

    /** Exercise 2 */
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    def takeTailRec(n: Int): Stream[A] = {
      @tailrec
      def loop(stream: Stream[A], acc: Stream[A], i: Int): Stream[A] =
        stream match {
          case Cons(h, t) if i > 1 => loop(t(), cons(h(), acc), i - 1)
          case Cons(h, _) if i == 1 => cons(h(), acc)
          case _ => acc
        }

      loop(this, empty, n).reverse
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    /** Exercise 3: Returns all starting elements that match a given predicate */
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case _ => empty
    }

    /** Exercise 4: Checks that all elements match a given predicate */
    def forAll(f: A => Boolean): Boolean = {
      @tailrec
      def loop(stream: Stream[A]): Boolean = stream match {
        case Cons(h, t) if f(h()) => loop(t())
        case Cons(_, _) => false
        case _ => true
      }

      loop(this)
    }

    /** Exercise 5 */
    def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty)

    /** Exercise 6 */
    def headOption: Option[A] = foldRight(Option.empty[A])((h, _) => Some(h))

    /** Exercise 7 */
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h).append(t))

    /** Exercise 13 */
    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

    def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
      case Cons(h, t) if n > 1 => Some(h(), t())
      case Cons(h, _) if n == 1 => Some(h(), empty)
      case _ => None
    }

    def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    /** Exercise 8 */
    def constant[A](a: A): Stream[A] = cons(a, constant(a))
    def constantLazy[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    /** Exercise 9 */
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    /** Exercise 10 */
    val fibs: Stream[Int] = {
      def loop(prev: Int, act: Int): Stream[Int] =
        cons(prev, loop(act, prev + act))

      loop(0, 1)
    }

    /** Exercise 11 */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

    /** Exercise 12 */
    def constantViaUnfold[A](a: A): Stream[A] = unfold(empty)(Some(a, _))
    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
    val fibsViaUnfold: Stream[Int] = unfold((0, 1)) {
      case (prev, act) => Some(prev, (act, prev + act))
    }

  }

}
