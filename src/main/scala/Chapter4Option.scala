object Chapter4Option {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  /** Exercise 1 */
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(get) => f(get)
      case None => None
    }

    def flatMap2[B](f: A => Option[B]): Option[B] = map(f(_)).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(_) => this
      case None => ob
    }

    def orElse2[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(get) if f(get) => this
      case _ => None
    }

    def filter2(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) this else None)

  }

  def mean(seq: Seq[Double]): Option[Double] =
    if (seq.isEmpty) None else Some(seq.sum / seq.length)

  /** Exercise 2 */
  def variance(seq: Seq[Double]): Option[Double] =
    mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))

  /** Exercise 3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for { aa <- a; bb <- b } yield f(aa, bb)

  /** Exercise 4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case maybeH::maybeT => maybeH.flatMap(h => sequence(maybeT).map(h::_))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  /** Exercise 5 */
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a => a)

}
