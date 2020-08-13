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

    def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
      def op(a1: A, a2: A): A = m.op(a2, a1)
      def zero: A = m.zero
    }

    /** Exercise 5 */
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

    /** Exercise 6 */
    def foldMapViaFoldRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.foldRight(m.zero)((a, b) => m.op(f(a), b))

    def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldMap(as, endoMonoid[B])(f.curried)(z)

    def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

    /** Exercise 7 */
    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.length match {
      case 0 => m.zero
      case 1 => f(as.head)
      case i =>
        val (left, right) = as.splitAt(i / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

    /** Exercise 10 */
    sealed trait WC
    case class Stub(chars: String) extends WC
    case class Part(lStub: String, words: Int, rStub: String) extends WC

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      def op(a1: WC, a2: WC): WC = (a1, a2) match {
        case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
        case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
        case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          val splitWord = if ((r1 + l2).isEmpty) 0 else 1
          Part(l1, w1 + w2 + splitWord, r2)
      }

      def zero: Stub = Stub("")
    }

    /** Exercise 11 */
    def wordCount(input: String): Int = {
      def asWC(c: Char): WC = if (c == ' ') Part("", 0, "") else Stub(c.toString)
      def count(s: String): Int = if (s.isEmpty) 0 else 1

      foldMapV(input.toIndexedSeq, wcMonoid)(asWC) match {
        case Stub(chars) => count(chars)
        case Part(lStub, words, rStub) => count(lStub) + words + count(rStub)
      }
    }

  }

}
