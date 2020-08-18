import Chapter11Monads.{listMonad, optionMonad}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter11MonadsTest extends Properties("Chapter11Monads") {

  private val oneTwoTree = List(1, 2, 3)
  private val optionOneTwoThree = List(Some(1), Some(2), Some(3))
  private val listOneTwoThree = List(List(1), List(2), List(3))

  property("sequence") =
    optionMonad.sequence(optionOneTwoThree).contains(oneTwoTree) &&
      optionMonad.sequence(List(Some(1), None, Some(3))).isEmpty &&
      listMonad.sequence(listOneTwoThree).contains(oneTwoTree)

  property("traverse") =
    optionMonad.traverse(oneTwoTree)(Some(_)).contains(oneTwoTree) &&
      optionMonad.traverse(oneTwoTree)(_ => None).isEmpty

  property("replicateM") = listMonad.replicateM(1, oneTwoTree) == listOneTwoThree

  /** Exercise 9 */
  property("associative law") = forAll { i: Int =>
    import optionMonad.{compose, flatMap}
    val f: Int => Option[Int] = a => Some(a + 1)
    val g: Int => Option[Int] = a => Some(a + 4)
    val h: Int => Option[Int] = a => Some(a - 6)
    val law: Option[Int] => Boolean = x => flatMap(x)(f).flatMap(g) == flatMap(x)(a => f(a).flatMap(g))

    law(None) && law(Some(1)) &&
      compose(compose(f, g), h)(1) == compose(f, compose(g, h))(1)
  }

}
