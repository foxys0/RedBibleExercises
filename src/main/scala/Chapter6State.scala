object Chapter6State {

  def unit[S, A](a: A): State[S, A] = State((a, _))

  case class State[S, +A](run: S => (A, S)) {

    /** Exercise 10 */
    def map[B](f: A => B): State[S, B] = State { s =>
      val (a, s2) = run(s)
      (f(a), s2)
    }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(f(a, _)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }

  }

}
