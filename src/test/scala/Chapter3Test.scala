import Chapter3.MyList
import Chapter3.MyList.patternMatching
import org.scalatest.funsuite.AnyFunSuite

class Chapter3Test extends AnyFunSuite {

  test("patternMatching") {
    assert(patternMatching(MyList(1, 2, 3, 4)) == 3)
    assert(patternMatching(MyList()) == 42)
    assert(patternMatching(MyList(1, 2)) == 3)
  }

}
