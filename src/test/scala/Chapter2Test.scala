import Chapter2.{fibonacci, isSorted}
import org.scalatest.funsuite.AnyFunSuite

class Chapter2Test extends AnyFunSuite {

  test("fibonacci") {
    Seq((0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (7, 13), (16, 987), (20, 6765))
      .map(t => assert(fibonacci(t._1) == t._2))
  }
  
  test("isSorted") {
    assert(isSorted(Array(1, 3, 2, 7, 8, 10, 2), _ >= _) == false)
    assert(isSorted(Array(10, 8, 7, 3, 2, 2, 1), _ >= _) == true)
    assert(isSorted(Array("a", "b", "c"), _ < _) == true)
    assert(isSorted(Array("a", "c", "b"), _ < _) == false)
  }
}
