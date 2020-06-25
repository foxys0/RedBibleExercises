import Chapter4Option.{Option, Some, None}
import org.scalatest.funsuite.AnyFunSuite

class Chapter4OptionTest extends AnyFunSuite {

  private val someInt: Option[Int] = Some(10)
  private val noneInt: Option[Int] = None
  private val default: Int = 0

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

}
