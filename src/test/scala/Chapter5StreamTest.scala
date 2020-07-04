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


}
