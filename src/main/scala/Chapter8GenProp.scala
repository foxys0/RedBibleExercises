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

    /** Exercise 5 */
    def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  }

  object Gen {
    def unit[A](a: => A): Gen[A] = Gen(Chapter6State.unit(a))
  }

}
