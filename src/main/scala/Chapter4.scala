object Chapter4 {

  def mean(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None else Some(seq.sum / seq.length)

  /** Exercise 2 */
  def variance(seq: Seq[Double]): Option[Double] =
    mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))

  /** Exercise 3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for { aa <- a; bb <- b } yield f(aa, bb)

  /** Exercise 4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val list = for { maybeElement <- a; element <- maybeElement } yield element

    if (list.size == a.size) Some(list)
    else None
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  /** Exercise 5 */
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)

}
