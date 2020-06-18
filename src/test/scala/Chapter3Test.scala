import Chapter3.MyList
import Chapter3.MyList._
import org.scalatest.funsuite.AnyFunSuite

class Chapter3Test extends AnyFunSuite {

  test("patternMatching") {
    assert(patternMatching(MyList(1, 2, 3, 4)) == 3)
    assert(patternMatching(MyList()) == 42)
    assert(patternMatching(MyList(1, 2)) == 3)
  }

  test("tail") {
    assert(tail(MyList(1, 2, 3)).contains(MyList(2, 3)))
    assert(tail(MyList("a", "b")).contains(MyList("b")))
    assert(tail(MyList("a")).contains(Nil))
    assert(tail(MyList()).isEmpty)
  }

  test("setHead") {
    assert(setHead(2, MyList(1, 3, 4)).contains(MyList(2, 3, 4)))
    assert(setHead("b", MyList("a", "c")).contains(MyList("b", "c")))
    assert(setHead("b", MyList("a")).contains(MyList("b")))
    assert(setHead("b", MyList()).isEmpty)
  }

  test("drop") {
    assert(drop(MyList(1, 2, 3, 4, 5), 3) == MyList(4, 5))
    assert(drop(MyList(1, 2), 1) == MyList(2))
    assert(drop(MyList(1, 2), 3) == MyList())
    assert(drop(MyList(1), 1) == MyList())
    assert(drop(MyList(), 1) == MyList())
  }

}
