import scala.annotation.tailrec
import scala.math.pow

object Chapter6Random {

  type Rand[+A] = RNG => (A, RNG)

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  /** Exercise 1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, rng2) = rng.nextInt

    ((num + 1).abs, rng2)
  }

  /** Exercise 2: Double between 0 and 1 (excluded) */
  def double(rng: RNG): (Double, RNG) = {
    val (num, rng2) = nonNegativeInt(rng)
    val dbl = num / pow(10, num.toString.length - 1)

    (dbl - dbl.floor, rng2)
  }

  /** Exercise 3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (num, rng2) = rng.nextInt
    val (dbl, rng3) = double(rng2)

    ((num, dbl), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (dbl, rng2) = double(rng)
    val (num, rng3) = rng2.nextInt

    ((dbl, num), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dbl, rng2) = double(rng)
    val (dbl2, rng3) = double(rng2)
    val (dbl3, rng4) = double(rng3)

    ((dbl, dbl2, dbl3), rng4)
  }

  /** Exercise 4: Generates list of random integers */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(i: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (i > 0) {
        val (num, rng2) = rng.nextInt
        loop(i - 1, num :: acc, rng2)
      } else (acc, rng)

    loop(count, List.empty, rng)
  }

  /** Exercise 5 */
  val doubleViaMap: Rand[Double] = map(nonNegativeInt) { num =>
    val dbl = num / pow(10, num.toString.length - 1)
    dbl - dbl.floor
  }

  /** Exercise 6 */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  /** Exercise 7 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def loop(list: List[Rand[A]], acc: Rand[List[A]]): Rand[List[A]] =
      list match {
        case head :: tail => loop(tail, map2(head, acc)(_ :: _))
        case Nil => acc
      }

    loop(fs, unit(List.empty[A]))
  }

  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  /** Exercise 8 */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)

    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { num =>
    val mod = num % n

    if (num + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  /** Exercise 9 */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}
