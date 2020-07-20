object Chapter6State {

  case class State[S, +A](run: S => (A, S)) {

    def unit[S, A](a: A): State[S, A] = ???

    def map[B](f: A => B): State[S, B] = ???

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

    def flatMap[B](f: A => State[S, B]): State[S, B] = ???

  }

}
