import Chapter11Monads.{listMonad, optionMonad}
import org.scalacheck.Properties

object Chapter11MonadsTest extends Properties("Chapter11Monads") {

  private val optionOneTwoThree = List(Some(1), Some(2), Some(3))
  private val oneTwoTree = List(1, 2, 3)

  property("sequence") =
    optionMonad.sequence(optionOneTwoThree).contains(oneTwoTree) &&
      optionMonad.sequence(List(Some(1), None, Some(3))).isEmpty &&
      listMonad.sequence(List(List(1), List(2), List(3))).contains(oneTwoTree)

  property("traverse") =
    optionMonad.traverse(oneTwoTree)(Some(_)).contains(oneTwoTree) &&
      optionMonad.traverse(oneTwoTree)(_ => None).isEmpty

}
