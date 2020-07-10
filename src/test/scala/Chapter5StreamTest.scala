import Chapter5Stream.Stream._
import Chapter5Stream._
import org.scalatest.funsuite.AnyFunSuite

class Chapter5StreamTest extends AnyFunSuite {

  case class TestStream(s: Stream[Int], l: List[Int])

  private val empty = TestStream(Stream(), List())
  private val one = TestStream(Stream(1), List(1))
  private val oneTwoThree = TestStream(Stream(1, 2, 3), List(1, 2, 3))

  test("toList, toListTailRec") {
    assert(empty.s.toList == empty.l)
    assert(one.s.toList == one.l)
    assert(oneTwoThree.s.toList == oneTwoThree.l)
    assert(empty.s.toListTailRec == empty.l)
    assert(one.s.toListTailRec == one.l)
    assert(oneTwoThree.s.toListTailRec == oneTwoThree.l)
  }

  test("take, drop") {
    assert(empty.s.take(1) == empty.s)
    assert(one.s.take(1).toList == one.l)
    assert(oneTwoThree.s.take(1).toList == one.l)
    assert(empty.s.takeTailRec(1) == empty.s)
    assert(one.s.takeTailRec(1).toList == one.l)
    assert(oneTwoThree.s.takeTailRec(1).toList == one.l)
    assert(empty.s.drop(1) == empty.s)
    assert(one.s.drop(1) == empty.s)
    assert(oneTwoThree.s.drop(1).toList == List(2, 3))
  }

  test("takeWhile, takeWhileViaFoldRight") {
    assert(empty.s.takeWhile(_ < 2) == empty.s)
    assert(one.s.takeWhile(_ < 2).toList == one.l)
    assert(oneTwoThree.s.takeWhile(_ < 2).toList == one.l)
    assert(empty.s.takeWhileViaFoldRight(_ < 2) == empty.s)
    assert(one.s.takeWhileViaFoldRight(_ < 2).toList == one.l)
    assert(oneTwoThree.s.takeWhileViaFoldRight(_ < 2).toList == one.l)
  }

  test("forAll") {
    assert(empty.s.forAll(_ < 2))
    assert(one.s.forAll(_ < 2))
    assert(!oneTwoThree.s.forAll(_ < 2))
  }

  test("headOption") {
    assert(empty.s.headOption.isEmpty)
    assert(one.s.headOption.contains(one.l.head))
    assert(oneTwoThree.s.headOption.contains(oneTwoThree.l.head))
  }

  test("map") {
    assert(empty.s.map(_ + 1) == empty.s)
    assert(one.s.map(_ + 1).toList == List(2))
    assert(oneTwoThree.s.map(_ + 1).toList == List(2, 3, 4))
  }

  test("filter") {
    assert(empty.s.filter(_ < 3) == empty.s)
    assert(one.s.filter(_ < 3).toList == List(1))
    assert(oneTwoThree.s.filter(_ < 3).toList == List(1, 2))
  }

  test("append") {
    assert(empty.s.append(one.s) == one.s)
    assert(one.s.append(one.s).toList == List(1, 1))
    assert(oneTwoThree.s.append(one.s).toList == List(1, 2, 3, 1))
  }

  test("flatMap") {
    assert(empty.s.flatMap(Stream(_, 10)) == empty.s)
    assert(one.s.flatMap(Stream(_, 10)).toList == List(1, 10))
    assert(oneTwoThree.s.flatMap(Stream(_, 10)).toList == List(1, 10, 2, 10, 3, 10))
  }

  test("constant") {
    assert(constant(1).take(5).toList == List(1, 1, 1, 1, 1))
    assert(constantLazy(1).take(5).toList == List(1, 1, 1, 1, 1))
    assert(constantUnfold(1).take(5).toList == List(1, 1, 1, 1, 1))
  }

  test("from") {
    assert(from(1).take(3).toList == oneTwoThree.l)
    assert(fromUnfold(1).take(3).toList == oneTwoThree.l)
  }

  test("fibs") {
    assert(fibs.take(1).toList == List(0))
    assert(fibsUnfold.take(1).toList == List(0))
    assert(fibs.take(2).toList == List(0, 1))
    assert(fibsUnfold.take(2).toList == List(0, 1))
    assert(fibs.take(6).toList == List(0, 1, 1, 2, 3, 5))
    assert(fibsUnfold.take(6).toList == List(0, 1, 1, 2, 3, 5))
  }

  test("unfold") {
    assert(unfold(empty.s)(s => Some(2, s)).take(3).toList == List(2, 2, 2))
    assert(unfold(empty.s)(_ => None).take(3).toList == List())
  }


}
