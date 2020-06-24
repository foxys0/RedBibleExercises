import Chapter3List.MyList
import Chapter3List.MyList._
import org.scalatest.funsuite.AnyFunSuite

class Chapter3ListTest extends AnyFunSuite {

  case class TestList(l: MyList[Int], sum: Int, product: Int, length: Int)
  
  private val empty = TestList(MyList(), 0, 1, 0)
  private val one = TestList(MyList(1), 1, 1, 1)
  private val oneTwoThreeFour = TestList(MyList(1, 2, 3, 4), 10, 24, 4)
  
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
    assert(product(MyList(1, 2, 0, 4)) == 0)
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
    assert(patternMatching(MyList(1, 2)) == 3)
  }

  test("tail") {
    assert(tail(empty.l).isEmpty)
    assert(tail(oneTwoThreeFour.l).contains(MyList(2, 3, 4)))
    assert(tail(MyList("a", "b")).contains(MyList("b")))
    assert(tail(MyList("a")).contains(Nil))
  }

  test("setHead") {
    assert(setHead(2, MyList(1, 3, 4)).contains(MyList(2, 3, 4)))
    assert(setHead("a", MyList("a", "b")).contains(MyList("a", "b")))
    assert(setHead("b", MyList("a", "c")).contains(MyList("b", "c")))
    assert(setHead("b", MyList("a")).contains(MyList("b")))
    assert(setHead("b", empty.l).isEmpty)
  }

  test("drop") {
    assert(drop(empty.l, 1) == empty.l)
    assert(drop(one.l, 1) == empty.l)
    assert(drop(oneTwoThreeFour.l, 2) == MyList(3, 4))
    assert(drop(MyList(1, 2), 1) == MyList(2))
    assert(drop(MyList(1, 2), 3) == empty.l)
  }

  test("dropWhile") {
    assert(dropWhile[Int](empty.l)(_ < 3) == empty.l)
    assert(dropWhile(one.l)(_ < 3) == empty.l)
    assert(dropWhile(oneTwoThreeFour.l)(_ < 3) == MyList(3, 4))
    assert(dropWhile(MyList(1, 2, 3, 4, 1))(_ < 3) == MyList(3, 4, 1))
    assert(dropWhile(MyList(5))(_ < 3) == MyList(5))
  }

  test("init") {
    assert(init(empty.l) == empty.l)
    assert(init(one.l) == empty.l)
    assert(init(oneTwoThreeFour.l) == MyList(1, 2, 3))
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
    assert(reverse(one.l) == MyList(1))
    assert(reverse(oneTwoThreeFour.l) == MyList(4, 3, 2, 1))
  }

  test("append") {
    assert(append(empty.l, 10) == MyList(10))
    assert(append(one.l, 10) == MyList(1, 10))
    assert(append(oneTwoThreeFour.l, 10) == MyList(1, 2, 3, 4, 10))
  }

  test("concat") {
    assert(concat(MyList(empty.l, empty.l)) == empty.l)
    assert(concat(MyList(empty.l, one.l)) == one.l)
    assert(concat(MyList(one.l, empty.l)) == one.l)
    assert(concat(MyList(one.l, oneTwoThreeFour.l)) == MyList(1, 1, 2, 3, 4))
  }

  test("increment") {
    assert(increment(empty.l) == empty.l)
    assert(increment(one.l) == MyList(2))
    assert(increment(oneTwoThreeFour.l) == MyList(2, 3, 4, 5))
    assert(increment2(empty.l) == empty.l)
    assert(increment2(one.l) == MyList(2))
    assert(increment2(oneTwoThreeFour.l) == MyList(2, 3, 4, 5))
  }

  test("convertToString") {
    assert(convertToString(empty.l) == MyList())
    assert(convertToString(one.l) == MyList("1"))
    assert(convertToString(oneTwoThreeFour.l) == MyList("1", "2", "3", "4"))
  }

  test("map, flatMap") {
    assert(map(empty.l)(_ + 1) == empty.l)
    assert(map(one.l)(_ + 1) == MyList(2))
    assert(map(oneTwoThreeFour.l)(_ + 1) == MyList(2, 3, 4, 5))
    assert(flatMap(empty.l)(MyList(_, 10)) == empty.l)
    assert(flatMap(one.l)(MyList(_, 10)) == MyList(1, 10))
    assert(flatMap(oneTwoThreeFour.l)(MyList(_, 10)) == MyList(1, 10, 2, 10, 3, 10, 4, 10))
  }

  test("filter, filter2") {
    assert(filter(empty.l)(_ < 3) == empty.l)
    assert(filter(one.l)(_ < 3) == one.l)
    assert(filter(oneTwoThreeFour.l)(_ < 3) == MyList(1, 2))
    assert(filter2(empty.l)(_ < 3) == empty.l)
    assert(filter2(one.l)(_ < 3) == one.l)
    assert(filter2(oneTwoThreeFour.l)(_ < 3) == MyList(1, 2))
  }

  test("sumOfPairs, zipWith") {
    assert(sumOfPairs(empty.l, empty.l)  == empty.l)
    assert(sumOfPairs(one.l, one.l)  == MyList(2))
    assert(sumOfPairs(oneTwoThreeFour.l, oneTwoThreeFour.l)  == MyList(2, 4, 6, 8))
    assert(zipWith(empty.l, empty.l)(_ + _)  == empty.l)
    assert(zipWith(one.l, one.l)(_ + _)  == MyList(2))
    assert(zipWith(oneTwoThreeFour.l, oneTwoThreeFour.l)(_ + _)  == MyList(2, 4, 6, 8))
  }


}
