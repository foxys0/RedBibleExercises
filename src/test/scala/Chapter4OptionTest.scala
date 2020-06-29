import Chapter4Option._
import org.scalatest.funsuite.AnyFunSuite

class Chapter4OptionTest extends AnyFunSuite {

  private val someInt: Option[Int] = Some(10)
  private val noneInt: Option[Int] = None
  private val default: Int = 0
  private val oneTwoTree = List(1, 2, 3)

  test("map") {
    assert(someInt.map(_ + 2) ==  Some(12))
    assert(noneInt.map(_ + 2) ==  None)
  }

  test("flatMap") {
    assert(someInt.flatMap(x => Some(x + 2)) ==  Some(12))
    assert(noneInt.flatMap(x => Some(x + 2)) ==  None)
    assert(someInt.flatMap2(x => Some(x + 2)) ==  Some(12))
    assert(noneInt.flatMap2(x => Some(x + 2)) ==  None)
  }

  test("getOrElse") {
    assert(someInt.getOrElse(default) ==  10)
    assert(noneInt.getOrElse(default) ==  default)
  }

  test("orElse") {
    assert(someInt.orElse(someInt) ==  someInt)
    assert(noneInt.orElse(Some(default)) ==  Some(default))
    assert(someInt.orElse2(someInt) ==  someInt)
    assert(noneInt.orElse2(Some(default)) ==  Some(default))
  }

  test("filter") {
    assert(someInt.filter(_ == 10) ==  someInt)
    assert(someInt.filter(_ == 12) ==  noneInt)
    assert(noneInt.filter(_ == 10) ==  noneInt)
    assert(someInt.filter2(_ == 10) ==  someInt)
    assert(someInt.filter2(_ == 12) ==  noneInt)
    assert(noneInt.filter2(_ == 10) ==  noneInt)
  }

  test("variance") {
    assert(variance(Seq(1, 1, 5, 5)) == Some(4.0))
    assert(variance(Seq(1, 2, 3, 4)) == Some(1.25))
  }

  test("map2") {
    assert(map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(map2[Int, Int, Int](Some(1), None)(_ + _) == None)
  }

  test("sequence") {
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(oneTwoTree))
    assert(sequence(List(Some(1), None)) == None)
    assert(sequence2(List(Some(1), Some(2), Some(3))) == Some(oneTwoTree))
    assert(sequence2(List(Some(1), None)) == None)
  }

  test("traverse") {
    assert(traverse(oneTwoTree)(Some(_)) == Some(oneTwoTree))
  }

}
