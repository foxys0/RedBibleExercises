import Chapter3List.List
import Chapter3List.List._
import org.scalatest.funsuite.AnyFunSuite

class Chapter3ListTest extends AnyFunSuite {

  case class TestList(l: List[Int], sum: Int, product: Int, length: Int)
  
  private val empty = TestList(List(), 0, 1, 0)
  private val one = TestList(List(1), 1, 1, 1)
  private val oneTwoThreeFour = TestList(List(1, 2, 3, 4), 10, 24, 4)
  
  test("sum, product") {
    assert(sum(empty.l) == empty.sum)
    assert(sum(one.l) == one.sum)
    assert(sum(oneTwoThreeFour.l) == oneTwoThreeFour.sum)
    assert(sum2(empty.l) == empty.sum)
    assert(sum2(one.l) == one.sum)
    assert(sum2(oneTwoThreeFour.l) == oneTwoThreeFour.sum)
    assert(sum3(empty.l) == empty.sum)
    assert(sum3(one.l) == one.sum)
    assert(sum3(oneTwoThreeFour.l) == oneTwoThreeFour.sum)
    assert(product(empty.l) == empty.product)
    assert(product(one.l) == one.product)
    assert(product(List(1, 2, 0, 4)) == 0)
    assert(product(oneTwoThreeFour.l) == oneTwoThreeFour.product)
    assert(product2(empty.l) == empty.product)
    assert(product2(one.l) == one.product)
    assert(product2(oneTwoThreeFour.l) == oneTwoThreeFour.product)
    assert(product3(empty.l) == empty.product)
    assert(product3(one.l) == one.product)
    assert(product3(oneTwoThreeFour.l) == oneTwoThreeFour.product)
  }

  test("patternMatching") {
    assert(patternMatching(empty.l) == 42)
    assert(patternMatching(one.l) == one.sum)
    assert(patternMatching(oneTwoThreeFour.l) == 3)
    assert(patternMatching(List(1, 2)) == 3)
  }

  test("tail") {
    assert(tail(empty.l).isEmpty)
    assert(tail(oneTwoThreeFour.l).contains(List(2, 3, 4)))
    assert(tail(List("a", "b")).contains(List("b")))
    assert(tail(List("a")).contains(Nil))
  }

  test("setHead") {
    assert(setHead(2, List(1, 3, 4)).contains(List(2, 3, 4)))
    assert(setHead("a", List("a", "b")).contains(List("a", "b")))
    assert(setHead("b", List("a", "c")).contains(List("b", "c")))
    assert(setHead("b", List("a")).contains(List("b")))
    assert(setHead("b", empty.l).isEmpty)
  }

  test("drop") {
    assert(drop(empty.l, 1) == empty.l)
    assert(drop(one.l, 1) == empty.l)
    assert(drop(oneTwoThreeFour.l, 2) == List(3, 4))
    assert(drop(List(1, 2), 1) == List(2))
    assert(drop(List(1, 2), 3) == empty.l)
  }

  test("dropWhile") {
    assert(dropWhile[Int](empty.l)(_ < 3) == empty.l)
    assert(dropWhile(one.l)(_ < 3) == empty.l)
    assert(dropWhile(oneTwoThreeFour.l)(_ < 3) == List(3, 4))
    assert(dropWhile(List(1, 2, 3, 4, 1))(_ < 3) == List(3, 4, 1))
    assert(dropWhile(List(5))(_ < 3) == List(5))
  }

  test("init") {
    assert(init(empty.l) == empty.l)
    assert(init(one.l) == empty.l)
    assert(init(oneTwoThreeFour.l) == List(1, 2, 3))
  }

  test("length") {
    assert(length(empty.l) == empty.length)
    assert(length(one.l) == one.length)
    assert(length(oneTwoThreeFour.l) == oneTwoThreeFour.length)
    assert(length2(empty.l) == empty.length)
    assert(length2(one.l) == one.length)
    assert(length2(oneTwoThreeFour.l) == oneTwoThreeFour.length)
  }

  test("foldLeft, foldRightViaFoldLeft") {
    assert(foldLeft(empty.l, 0)(_ + _) == empty.sum)
    assert(foldLeft(one.l, 0)(_ + _) == one.sum)
    assert(foldLeft(oneTwoThreeFour.l, 0)(_ + _) == oneTwoThreeFour.sum)
    assert(foldRightViaFoldLeft(empty.l, 0)(_ + _) == empty.sum)
    assert(foldRightViaFoldLeft(one.l, 0)(_ + _) == one.sum)
    assert(foldRightViaFoldLeft(oneTwoThreeFour.l, 0)(_ + _) == oneTwoThreeFour.sum)
  }

  test("reverse") {
    assert(reverse(empty.l) == empty.l)
    assert(reverse(one.l) == List(1))
    assert(reverse(oneTwoThreeFour.l) == List(4, 3, 2, 1))
  }

  test("append") {
    assert(append(empty.l, 10) == List(10))
    assert(append(one.l, 10) == List(1, 10))
    assert(append(oneTwoThreeFour.l, 10) == List(1, 2, 3, 4, 10))
  }

  test("concat") {
    assert(concat(List(empty.l, empty.l)) == empty.l)
    assert(concat(List(empty.l, one.l)) == one.l)
    assert(concat(List(one.l, empty.l)) == one.l)
    assert(concat(List(one.l, oneTwoThreeFour.l)) == List(1, 1, 2, 3, 4))
  }

  test("increment") {
    assert(increment(empty.l) == empty.l)
    assert(increment(one.l) == List(2))
    assert(increment(oneTwoThreeFour.l) == List(2, 3, 4, 5))
    assert(increment2(empty.l) == empty.l)
    assert(increment2(one.l) == List(2))
    assert(increment2(oneTwoThreeFour.l) == List(2, 3, 4, 5))
  }

  test("convertToString") {
    assert(convertToString(empty.l) == List())
    assert(convertToString(one.l) == List("1"))
    assert(convertToString(oneTwoThreeFour.l) == List("1", "2", "3", "4"))
  }

  test("map, flatMap") {
    assert(map(empty.l)(_ + 1) == empty.l)
    assert(map(one.l)(_ + 1) == List(2))
    assert(map(oneTwoThreeFour.l)(_ + 1) == List(2, 3, 4, 5))
    assert(flatMap(empty.l)(List(_, 10)) == empty.l)
    assert(flatMap(one.l)(List(_, 10)) == List(1, 10))
    assert(flatMap(oneTwoThreeFour.l)(List(_, 10)) == List(1, 10, 2, 10, 3, 10, 4, 10))
  }

  test("filter, filter2") {
    assert(filter(empty.l)(_ < 3) == empty.l)
    assert(filter(one.l)(_ < 3) == one.l)
    assert(filter(oneTwoThreeFour.l)(_ < 3) == List(1, 2))
    assert(filter2(empty.l)(_ < 3) == empty.l)
    assert(filter2(one.l)(_ < 3) == one.l)
    assert(filter2(oneTwoThreeFour.l)(_ < 3) == List(1, 2))
  }

  test("sumOfPairs, zipWith") {
    assert(sumOfPairs(empty.l, empty.l)  == empty.l)
    assert(sumOfPairs(one.l, one.l)  == List(2))
    assert(sumOfPairs(oneTwoThreeFour.l, oneTwoThreeFour.l)  == List(2, 4, 6, 8))
    assert(zipWith(empty.l, empty.l)(_ + _)  == empty.l)
    assert(zipWith(one.l, one.l)(_ + _)  == List(2))
    assert(zipWith(oneTwoThreeFour.l, oneTwoThreeFour.l)(_ + _)  == List(2, 4, 6, 8))
  }


}
