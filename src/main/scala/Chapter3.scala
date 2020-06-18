object Chapter3 {

  sealed trait MyList[+A]
  case object Nil extends MyList[Nothing]
  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
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
  }


}
