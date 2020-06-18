import annotation.tailrec

object Chapter3 {

  sealed trait MyList[+A]

  object MyList {
    case object Nil extends MyList[Nothing]
    case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

    def apply[A](as: A*): MyList[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
    def sum(ints: MyList[Int]): Int = ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
    def product(ints: MyList[Int]): Int = ints match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(head, tail) => head * product(tail)
    }

    /** Exercise 1: Pattern matching */
    def patternMatching(myList: MyList[Int]): Int = myList match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    /** Exercise 2: Removes first element of list */
    def tail[A](myList: MyList[A]): Option[MyList[A]] = myList match {
      case Nil => None
      case Cons(_, tail) => Some(tail)
    }

    /** Exercise 3: Replaces first element of list */
    def setHead[A](head: A, myList: MyList[A]): Option[MyList[A]] = myList match {
      case Nil => None
      case Cons(_, tail) => Some(Cons(head, tail))
    }

    /** Exercise 4: Drops first n elements of list */
    def drop[A](myList: MyList[A], n: Int): MyList[A] = {
      @tailrec
      def loop(xs: MyList[A], i: Int): MyList[A] = {
        val maybeTail = tail(xs)
        if (maybeTail.isEmpty | i == 0) xs
        else loop(maybeTail.get, i - 1)
      }

      loop(myList, n)
    }
  }


}
