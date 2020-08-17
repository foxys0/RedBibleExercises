import Chapter11Functor.listFunctor
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter11FunctorTest extends Properties("Chapter11Functor") {

  property("composition law") = forAll { i: Int =>
    val f: Int => Int = _ + 1
    val g: Int => Int = _ + 4
    val xs = List(i)

    listFunctor.map(listFunctor.map(xs)(f))(g) == listFunctor.map(xs)(f andThen g) &&
      listFunctor.map(xs)(x => x) == xs
  }

}
