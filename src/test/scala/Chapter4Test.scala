import Chapter4._
import org.scalatest.funsuite.AnyFunSuite

class Chapter4Test extends AnyFunSuite {

  private val oneTwoTree = List(1, 2, 3)

  test("variance") {
    assert(variance(Seq(1, 1, 5, 5)).contains(4.0))
    assert(variance(Seq(1, 2, 3, 4)).contains(1.25))
  }

  test("map2") {
    assert(map2(Some(1), Some(2))(_ + _).contains(3))
    assert(map2[Int, Int, Int](Some(1), None)(_ + _).isEmpty)
  }

  test("sequence") {
    assert(sequence(List(Some(1), Some(2), Some(3))).contains(oneTwoTree))
    assert(sequence(List(Some(1), None)).isEmpty)
    assert(sequence2(List(Some(1), Some(2), Some(3))).contains(oneTwoTree))
    assert(sequence2(List(Some(1), None)).isEmpty)
  }

  test("traverse") {
    assert(traverse(oneTwoTree)(Some(_)).contains(oneTwoTree))
  }

}
