object Chapter3Tree {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    /** Exercise 25: Counts all elements in tree */
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    /** Exercise 26: Returns maximum value */
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

    /** Exercise 27: Returns maximum depth */
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    /** Exercise 28: Map */
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    /** Exercise 29: Fold */
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
    def maximum2(tree: Tree[Int]): Int = fold(tree)(a => a)(_.max(_))
    def depth2(tree: Tree[Int]): Int = fold(tree)(_ => 0)(1 + _.max(_))
    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  }

}
