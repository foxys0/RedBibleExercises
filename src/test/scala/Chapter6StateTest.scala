import Chapter6Random.SimpleRNG
import Chapter6State.State
import Chapter6State.State.unit
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter6StateTest extends Properties("Chapter6State") {

  private val increment: Int => Int = _ + 1

  property("unit, map") = forAll { i: Int =>
    val rng = SimpleRNG(i)
    val num = rng.nextInt._1

    increment(num) == unit(num).map(increment).run(rng)._1
  }

  property("map2") = forAll { i: Int =>
    val rng = SimpleRNG(i)
    val num = rng.nextInt._1
    val state: State[SimpleRNG, Int] = unit(num)

    increment(num) == unit(num).map2(state)((a, _) => increment(a)).run(rng)._1 &&
    increment(num) == unit(num).map2(state)((_, b) => increment(b)).run(rng)._1
  }

  property("flatMap") = forAll { i: Int =>
    val rng = SimpleRNG(i)
    val num = rng.nextInt._1
    val incrementS: Int => State[SimpleRNG, Int] = a => State((increment(num), _))

    increment(num) == unit(num).flatMap(incrementS).run(rng)._1
  }

}
