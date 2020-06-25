import Chapter3Tree._
import Chapter3Tree.Tree._
import org.scalatest.funsuite.AnyFunSuite

class Chapter3TreeTest extends AnyFunSuite {

  private val one = Leaf(1)
  private val oneTwo = Branch(Leaf(1), Leaf(2))
  private val oneTwoThreeFour = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  test("size") {
    assert(size(one) == 1)
    assert(size(oneTwo) == 3)
    assert(size(oneTwoThreeFour) == 7)
    assert(size2(one) == 1)
    assert(size2(oneTwo) == 3)
    assert(size2(oneTwoThreeFour) == 7)
  }

  test("maximum") {
    assert(maximum(one) == 1)
    assert(maximum(oneTwo) == 2)
    assert(maximum(oneTwoThreeFour) == 4)
    val tree = Branch(Branch(Leaf(3), Leaf(2)), Branch(Leaf(4), Leaf(1)))
    assert(maximum(tree) == 4)
    assert(maximum2(one) == 1)
    assert(maximum2(oneTwo) == 2)
    assert(maximum2(oneTwoThreeFour) == 4)
    assert(maximum2(tree) == 4)
  }

  test("depth") {
    assert(depth(one) == 0)
    assert(depth(oneTwo) == 1)
    assert(depth(oneTwoThreeFour) == 2)
    val tree = Branch(Leaf(1), Branch(Leaf(2), Branch(Branch(Leaf(4), Leaf(4)), Leaf(3))))
    assert(depth(tree) == 4)
    assert(depth2(one) == 0)
    assert(depth2(oneTwo) == 1)
    assert(depth2(oneTwoThreeFour) == 2)
    assert(depth2(tree) == 4)
  }

  test("map") {
    assert(map(one)(_ + 1) == Leaf(2))
    assert(map(oneTwo)(_ + 1) == Branch(Leaf(2), Leaf(3)))
    assert(map2(one)(_ + 1) == Leaf(2))
    assert(map2(oneTwo)(_ + 1) == Branch(Leaf(2), Leaf(3)))
  }

}
