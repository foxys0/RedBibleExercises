import Chapter10Monoids.Monoid
import Chapter3Tree.{Branch, Leaf, Tree}

object Chapter10Foldable {

  trait Foldable[F[_]] {
    import Monoid.{dual, endoMonoid}

    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    /** Exercise 15 */
    def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_ :: _)
  }

  /** Exercise 12 */
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def toList[A](as: List[A]): List[A] = as
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    import Monoid.foldMapV

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
  }

  /** Exercise 13 */
  object TreeFoldable extends Foldable[Tree] {

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }
  }
}
