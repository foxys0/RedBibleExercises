import annotation.tailrec

object Chapter3List {

  sealed trait List[+A]

  object List {
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

    def sum(list: List[Int]): Int = list match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
    def product(list: List[Int]): Int = list match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(head, tail) => head * product(tail)
    }

    def sum2(list: List[Int]): Int = foldRight(list, 0)(_ + _)
    def product2(list: List[Int]): Int = foldRight(list, 1)(_ * _)

    /** Exercise 1: Pattern matching */
    def patternMatching(list: List[Int]): Int = list match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    /** Exercise 2: Removes first element of list */
    def tail[A](list: List[A]): Option[List[A]] = list match {
      case Nil => None
      case Cons(_, tail) => Some(tail)
    }

    /** Exercise 3: Replaces first element of list */
    def setHead[A](head: A, list: List[A]): Option[List[A]] = list match {
      case Nil => None
      case Cons(_, tail) => Some(Cons(head, tail))
    }

    /** Exercise 4: Drops first n elements of list */
    def drop[A](list: List[A], n: Int): List[A] = {
      @tailrec
      def loop(xs: List[A], i: Int): List[A] = {
        val maybeTail = tail(xs)
        if (maybeTail.isEmpty | i <= 0) xs
        else loop(maybeTail.get, i - 1)
      }

      loop(list, n)
    }

    /** Exercise 5: Removes first n elements until condition is met */
    @tailrec
    def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => list
    }

    /** Exercise 6: Returns list without the last element */
    def init[A](list: List[A]): List[A] =
      list match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(head, tail) => Cons(head, init(tail))
      }

    /** Exercise 9: Computes length of list using foldRight */
    def length[A](list: List[Int]): Int = foldRight(list, 0)((_, i) => i + 1)

    /** Exercise 10: Stack-safe foldRight */
    @tailrec
    def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

    /** Exercise 11: sum, product, length using foldLeft */
    def sum3(list: List[Int]): Int = foldLeft(list, 0)(_ + _)
    def product3(list: List[Int]): Int = foldLeft(list, 1)(_ * _)
    def length2[A](list: List[Int]): Int = foldLeft(list, 0)((i, _) => i + 1)

    /** Exercise 12: Returns the reverse of a list */
    def reverse[A](list: List[A]): List[A] =
      foldLeft(list, List[A]())((b, a) => Cons(a, b))

    def foldRightViaFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(list), z)((b, a) => f(a, b))

    /** Exercise 14: Appends element to a list */
    def append[A](list: List[A], element: A): List[A] =
      foldRight(list, List[A](element))(Cons(_, _))

    /** Exercise 15: Concatenates a list of lists into one list */
    def concat[A](list: List[List[A]]): List[A] =
      foldLeft(list, List[A]())((a, b) => foldRight(a, b)(Cons(_,_)))

    /** Exercise 16: Adds 1 to every element of list */
    def increment(list: List[Int]): List[Int] =
      foldRight(list, List[Int]())((a, b) => Cons(a + 1, b))

    def increment2(list: List[Int]): List[Int] =
      reverse(foldLeft(list, List[Int]())((b, a) => Cons(a + 1, b)))

    /** Exercise 17: Converts all elements to String */
    def convertToString[A](list: List[A]): List[String] =
      foldRightViaFoldLeft(list, List[String]())((a, b) => Cons(a.toString, b))

    /** Exercise 18: Map */
    def map[A, B](list: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(list, List[B]())((a, b) => Cons(f(a), b))

    /** Exercise 19: Filter elements unless they satisfy condition */
    def filter[A](list: List[A])(f: A => Boolean): List[A] =
      foldRightViaFoldLeft(list, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

    /** Exercise 20: FlatMap */
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      concat(map(list)(f))

    /** Exercise 21: Filter implemented via flatMap */
    def filter2[A](list: List[A])(f: A => Boolean): List[A] =
      flatMap(list)(a => if(f(a)) List(a) else Nil)

    /** Exercise 22: Sums elements from two lists */
    def sumOfPairs(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh + rh, sumOfPairs(lt, rt))
    }

    /** Exercise 23: Generalized sumOfPairs */
    def zipWith[A](left: List[A], right: List[A])(f: (A, A) => A): List[A] = (left, right) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt)(f))
    }
  }

}
