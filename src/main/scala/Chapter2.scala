import annotation.tailrec

object Chapter2 {

  /** Exercise 1: Tail recursive Fibonacci function */
  def fibonacci(nth: Int): Int = {
    @tailrec
    def fib(i: Int, actual: Int, previous: Int): Int =
      if (i == 0) actual
      else fib(i - 1, previous, actual + previous)

    fib(nth, 0, 1)
  }

  /** Exercise 2: Polymorphic function */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  /** Exercise 3: Currying */
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  /** Exercise 4: UnCurrying */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /** Exercise 5: Higher-order function */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}