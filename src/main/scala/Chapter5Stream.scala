import Chapter5Stream.Stream._

import scala.annotation.tailrec

object Chapter5Stream {

  trait Stream[+A] {

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
      case Empty => List.empty
    }

    def toListTailRec: List[A] = {
      @tailrec
      def loop(stream: Stream[A], list: List[A]): List[A] = stream match {
        case Cons(h, t) => loop(t(), h() :: list)
        case Empty => list
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
      def loop(stream: Stream[A], acc: Stream[A], i: Int): Stream[A] = stream match {
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
  }

}
