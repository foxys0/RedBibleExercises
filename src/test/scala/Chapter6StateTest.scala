import Chapter6State._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter6StateTest extends Properties("Chapter6") {

  property("nonNegativeInt") = forAll { i: Int =>
    val num = nonNegativeInt(SimpleRNG(i))._1
    num >= 0 && num <= Int.MaxValue
  }

  property("double") = forAll { i: Int =>
    val num = double(SimpleRNG(i))._1
    num >= 0 && num < 1
  }

  property("intDouble") = forAll { i: Int =>
    val ((num, dbl), _) = intDouble(SimpleRNG(i))
    val ((num2, dbl2), _) = intDouble(SimpleRNG(i))

    num == num2 && dbl == dbl2
  }

  property("doubleInt") = forAll { i: Int =>
    val ((dbl, num), _) = doubleInt(SimpleRNG(i))
    val ((dbl2, num2), _) = doubleInt(SimpleRNG(i))

    dbl == dbl2 && num == num2
  }

  property("double3") = forAll { i: Int =>
    val ((dbl1, dbl2, dbl3), _) = double3(SimpleRNG(i))
    val ((dbl4, dbl5, dbl6), _) = double3(SimpleRNG(i))

    dbl1 == dbl4 && dbl2 == dbl5 && dbl3 == dbl6
  }

  property("ints") = forAll { i: Int =>
    val list = ints(5)(SimpleRNG(i))._1

    ints(0)(SimpleRNG(i))._1.isEmpty &&
      ints(1)(SimpleRNG(i))._1.length == 1 &&
      list.length == 5 && list.distinct.length == list.length
  }

}