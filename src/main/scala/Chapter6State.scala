import scala.annotation.tailrec
import scala.math.pow

object Chapter6State {

  trait RNG { def nextInt: (Int, RNG) }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
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
      }
      else (acc, rng)

    loop(count, List.empty, rng)
  }

}
