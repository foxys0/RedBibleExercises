object Chapter10Monoids {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  object Monoid {

    val stringMonoid: Monoid[String] = new Monoid[String] {
      def op(a1: String, a2: String): String = a1 + a2
      val zero = ""
    }

    def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
      val zero: List[A] = Nil
    }

    /** Exercise 1 */
    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2
      val zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 * a2
      val zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
      val zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
      val zero: Boolean = true
    }

    /** Exercise 2 */
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
      val zero: Option[A] = None
    }

    /** Exercise 3 */
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(a1: A => A, a2: A => A): A => A = a1.compose(a2) // a => a1(a2(a))
      val zero: A => A = a => a
    }

  }

}
