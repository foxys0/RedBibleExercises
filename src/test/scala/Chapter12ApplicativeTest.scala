import Chapter12Applicative.{eitherMonad, optionApplicative}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter12ApplicativeTest extends Properties("Chapter12Applicative") {

  property("map, map2, map3, map4") = forAll { (i: Int, j: Int, k: Int, l: Int) =>
    import optionApplicative.unit
    val (fa, fb, fc, fd) = (unit(i), unit(j), unit(k), unit(l))

    optionApplicative.map(fa)(_ + 2).contains(i + 2)
    optionApplicative.map2ViaApply(fa, fb)(_ + _).contains(i + j)
    optionApplicative.map3(fa, fb, fc)(_ + _ + _).contains(i + j + k)
    optionApplicative.map4(fa, fb, fc, fd)(_ + _ + _ + _).contains(i + j + k + l)
  }

  property("either monad") = forAll { i: Int =>
    eitherMonad.flatMap(eitherMonad.unit(i))(a => Right(a - 2)).contains(i - 2)
  }

}
