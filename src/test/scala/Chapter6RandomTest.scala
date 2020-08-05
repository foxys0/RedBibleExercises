import Chapter6Random.RNG._
import Chapter6Random.SimpleRNG
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter6RandomTest extends Properties("Chapter6Random") {

  property("nonNegativeInt") = forAll { i: Int =>
    val num = nonNegativeInt(SimpleRNG(i))._1

    num >= 0 && num <= Int.MaxValue
  }

  property("double") = forAll { i: Int =>
    val num = double(SimpleRNG(i))._1

    num >= 0 && num < 1
  }

  property("doubleViaMap") = forAll { i: Int =>
    val num = doubleViaMap(SimpleRNG(i))._1

    num >= 0 && num < 1
  }

  property("intDouble") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    val ((num, dbl), _) = intDouble(rng)
    val ((num2, dbl2), _) = intDouble(rng)

    num == num2 && dbl == dbl2
  }

  property("doubleInt") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    val ((dbl, num), _) = doubleInt(rng)
    val ((dbl2, num2), _) = doubleInt(rng)

    dbl == dbl2 && num == num2
  }

  property("double3") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    val ((dbl1, dbl2, dbl3), _) = double3(rng)
    val ((dbl4, dbl5, dbl6), _) = double3(rng)

    dbl1 == dbl4 && dbl2 == dbl5 && dbl3 == dbl6
  }

  property("ints") = forAll { i: Int =>
    val rng = SimpleRNG(i)
    val list = ints(5)(rng)._1

    ints(0)(rng)._1.isEmpty &&
    ints(1)(rng)._1.length == 1 &&
    list.length == 5 && list.distinct.length == list.length
  }

  property("map2") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    intDouble(rng) == map2(int, doubleViaMap)((_, _))(rng)
  }

  property("sequence") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    val ((dbl1, dbl2, dbl3), _) = double3(rng)
    val list = sequence(List(double(_), double(_), double(_)))(rng)

    dbl1 == list._1.head && dbl2 == list._1(1) && dbl3 == list._1(2)
  }

  property("sequenceViaFoldRight") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    val ((dbl1, dbl2, dbl3), _) = double3(rng)
    val list = sequenceViaFoldRight(List(double(_), double(_), double(_)))(rng)

    dbl1 == list._1.head && dbl2 == list._1(1) && dbl3 == list._1(2)
  }

  property("flatMap") = forAll { i: Int =>
    val rng = SimpleRNG(i)

    int(rng) == flatMap(int)(unit)(rng)
  }

  property("nonNegativeLessThan") = forAll { i: Int =>
    val constraint = 10
    val num = nonNegativeLessThan(constraint)(SimpleRNG(i))._1

    num >= 0 && num <= constraint
  }

  property("mapViaFlatMap") = forAll { i: Int =>
    val rng = SimpleRNG(i)
    val num = mapViaFlatMap(int)(_ + 1)(rng)._1

    int(rng)._1 + 1 == num
  }

}
