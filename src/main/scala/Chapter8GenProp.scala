import Chapter5Stream.Stream
import Chapter6Random.RNG
import Chapter6State.State
import Chapter8GenProp.Prop.{Falsified, Passed, Proved, Result, TestCases}

object Chapter8GenProp {

  case class Prop(run: (TestCases, RNG) => Result) {

    /** Exercise 9 */
    def &&(p: Prop): Prop = Prop { (ts, rng) =>
      run(ts, rng) match {
        case Passed | Proved => p.run(ts, rng)
        case f => f
      }
    }

    def ||(p: Prop): Prop = Prop { (ts, rng) =>
      run(ts, rng) match {
        case Falsified(_, _) => p.run(ts, rng)
        case p => p
      }
    }
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int

    sealed trait Result { def isFalsified: Boolean }
    case object Passed extends Result { def isFalsified = false }
    case class Falsified(failedCase: FailedCase, successCount: SuccessCount) extends Result {
      def isFalsified = true
    }
    case object Proved extends Result { def isFalsified = false }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Chapter5Stream.Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }
  }

  case class Gen[A](sample: State[RNG, A]) {

    def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

    /** Exercise 6 */
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

    /** Exercise 10 */
    def unsized: SGen[A] = SGen(_ => this)
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

  case class SGen[A](forSize: Int => Gen[A]) {

    /** Exercise 10 */
    def apply(n: Int): Gen[A] = forSize(n)
    def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      val forSize2: Int => Gen[B] = n => forSize(n).flatMap(f(_).forSize(n))
      SGen(forSize2)
    }
  }



}
