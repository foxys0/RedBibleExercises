import Chapter6Random.RNG
import Chapter6State.State
import Chapter8GenProp.Prop.{FailedCase, SuccessCount}

object Chapter8GenProp {

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int

    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  }

  case class Gen[A](sample: State[RNG, A]) {

    def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

    /** Exercise 6 */
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  }

  object Gen {

    /** Exercise 5 */
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
    def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    /** Exercise 7 */
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

  }

}
