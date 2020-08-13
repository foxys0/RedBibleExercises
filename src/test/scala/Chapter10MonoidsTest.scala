import Chapter10Monoids.Monoid
import Chapter10Monoids.Monoid._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object Chapter10MonoidsTest extends Properties("Chapter10Monoids") {

  /** Exercise 4 */
  def monoidLaws[A](m: Monoid[A], a1: A, a2: A, a3: A): Boolean =
    m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3) &&
      m.op(a1, m.zero) == a1 &&
      m.op(m.zero, a1) == a1

  property("intAddition") = forAll { (i: Int, j: Int, k: Int) =>
    monoidLaws(stringMonoid, i.toString, j.toString, k.toString) &&
    monoidLaws(listMonoid[Int], List(i), List(j), List(k)) &&
    monoidLaws(intAddition, i, j, k) &&
    monoidLaws(intMultiplication, i, j, k) &&
    monoidLaws(booleanAnd, i % 2 == 0, j % 2 == 0, k % 2 == 0)
    monoidLaws(booleanOr, i % 2 == 0, j % 2 == 0, k % 2 == 0) &&
    monoidLaws(optionMonoid[Int], Some(i), Some(j), Some(k))
  }

}
