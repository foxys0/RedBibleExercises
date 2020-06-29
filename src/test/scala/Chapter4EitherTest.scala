import Chapter4Either._
import org.scalatest.funsuite.AnyFunSuite

class Chapter4EitherTest extends AnyFunSuite {

  private val oneTwoTree = List(1, 2, 3)

  test("map") {
    assert(Right(1).map(_ + 1) == Right(2))
    assert(Left(0).map(a => a) == Left(0))
  }

  test("flatMap") {
    assert(Right(1).flatMap(a => Right(a + 1)) == Right(2))
    assert(Left(0).flatMap(a => a) == Left(0))
  }

  test("orElse") {
    assert(Right(1).orElse(Right(2)) == Right(1))
    assert(Left(0).orElse(Left(2)) == Left(2))
  }

  test("map2") {
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
    assert(Left(0).map2(Right(1))((_, b) => b) == Left(0))
    assert(Right(1).map2viaFor(Right(2))(_ + _) == Right(3))
    assert(Left(0).map2viaFor(Right(1))((_, b) => b) == Left(0))
  }

  test("sequence") {
    assert(sequence(List(Right(1), Right(2), Right(3))) == Right(oneTwoTree))
    assert(sequence(List(Right(1), Left(0))) == Left(0))
    assert(sequence2(List(Right(1), Right(2), Right(3))) == Right(oneTwoTree))
    assert(sequence2(List(Right(1), Left(0))) == Left(0))
  }

  test("traverse") {
    assert(traverse(oneTwoTree)(Right(_)) == Right(oneTwoTree))
  }

}
