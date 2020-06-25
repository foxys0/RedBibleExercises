object Chapter4Option {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

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



}
