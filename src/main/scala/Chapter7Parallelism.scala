import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Chapter7Parallelism {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get) //new Callable[A] { def call: A = a(es).get }

  /** Exercise 4 */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  /** Exercise 5 */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case pa :: pt => map2(pa, fork(sequence(pt)))(_ :: _)
    case Nil => unit(Nil)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

  /** Exercise 6 */
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
    map(sequence(l.map(asyncF((a: A) => if (f(a)) List(a) else List()))))(_.flatten)

  /** Exercise 11 */
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val i = run(es)(n).get
    run(es)(choices(i))
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(a => if(a) 0 else 1))(List(t, f))

  /** Exercise 12 */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val value = run(es)(key).get()
    run(es)(choices(value))
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val value = run(es)(pa).get()
    run(es)(choices(value))
  }

}
