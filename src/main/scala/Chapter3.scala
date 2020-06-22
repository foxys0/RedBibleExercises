import annotation.tailrec

object Chapter3 {

  sealed trait MyList[+A]

  object MyList {
    case object Nil extends MyList[Nothing]
    case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

    def apply[A](as: A*): MyList[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](myList: MyList[A], z: B)(f: (A, B) => B): B = myList match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

    def sum(myList: MyList[Int]): Int = myList match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
    def product(myList: MyList[Int]): Int = myList match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(head, tail) => head * product(tail)
    }

    def sum2(myList: MyList[Int]): Int = foldRight(myList, 0)(_ + _)
    def product2(myList: MyList[Int]): Int = foldRight(myList, 1)(_ * _)

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
        if (maybeTail.isEmpty | i <= 0) xs
        else loop(maybeTail.get, i - 1)
      }

      loop(myList, n)
    }

    /** Exercise 5: Removes first n elements until condition is met */
    @tailrec
    def dropWhile[A](myList: MyList[A])(f: A => Boolean): MyList[A] = myList match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => myList
    }

    /** Exercise 6: Returns list without the last element */
    def init[A](myList: MyList[A]): MyList[A] =
      myList match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(head, tail) => Cons(head, init(tail))
      }

    /** Exercise 9: Computes length of list using foldRight */
    def length[A](myList: MyList[Int]): Int = foldRight(myList, 0)((_, i) => i + 1)

    /** Exercise 10: stack-safe foldRight */
    @tailrec
    def foldLeft[A, B](myList: MyList[A], z: B)(f: (B, A) => B): B = myList match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

    /** Exercise 11: sum, product, length using foldLeft */
    def sum3(myList: MyList[Int]): Int = foldLeft(myList, 0)(_ + _)
    def product3(myList: MyList[Int]): Int = foldLeft(myList, 1)(_ * _)
    def length2[A](myList: MyList[Int]): Int = foldLeft(myList, 0)((i, _) => i + 1)



  }


}
