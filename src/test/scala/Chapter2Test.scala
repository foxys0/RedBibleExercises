import Chapter2._
import org.scalatest.funsuite.AnyFunSuite

class Chapter2Test extends AnyFunSuite {

  test("fibonacci") {
    Seq((0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (7, 13), (16, 987), (20, 6765))
      .map(t => assert(fibonacci(t._1) == t._2))
  }

  test("isSorted") {
    assert(!isSorted[Int](Array(1, 3, 2, 7, 8, 10, 2), _ >= _))
    assert(isSorted[Int](Array(10, 8, 7, 3, 2, 2, 1), _ >= _))
    assert(isSorted[String](Array("a", "b", "c"), _ < _))
    assert(!isSorted[String](Array("a", "c", "b"), _ < _))
  }

  test("curry, uncurry, compose") {
    val c = curry((a: Int, b: Int) => a + b)
    val uc = uncurry((a: Int) => (b: Int) => a + b)
    val hof = compose((b: Int) => b + 2, (a: Int) => a + 1)

    assert(c(1)(2) == 3)
    assert(uc(1, 2) == 3)
    assert(hof(0) == 3)
  }


}
