object Chapter7Parallelism {

  type Par[A]

  def unit[A](a: A): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](pa: Par[A]): A = ???

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    unit(f(run(pa), run(pb)))


}
