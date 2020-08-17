import java.util.concurrent.Executors.newFixedThreadPool

import Chapter7Parallelism.Par.{run => runPar, _}
import org.scalatest.funsuite.AnyFunSuite

class Chapter7ParallelismTest extends AnyFunSuite {

  private val es = newFixedThreadPool(10)

  test("Exercise 7") {
    val f: Int => Int = _ + 1
    val g: Int => Int = _ + 2
    val y = unit(10)
    val expr1 = map(map(y)(g))(f)
    val expr2 = map(y)(f compose g)

    assert(runPar(es)(expr1) == runPar(es)(expr2))
  }

  test("Exercise 8") {
    val list = List.range(1, 10)
    val result = List.range(2, 10, 2)
    val a = lazyUnit(42 + 1)

    assert(runPar(es)(parFilter(list)(_ % 2 == 0)).get() == result)
    assert(equal(es)(a, fork(a)))

    // Deadlocks
    //assert(runPar(newFixedThreadPool(1))(parFilter(list)(_ % 2 == 0)).get() == result)
    //assert(equal(newFixedThreadPool(1))(a, fork(a)))
  }

  test("choice, choiceN") {
    val a = 0
    val b = 10
    val pa = unit(a)
    val pb = unit(b)

    assert(runPar(es)(choice(unit(true))(pa, pb)).get() == a)
    assert(runPar(es)(choiceViaChooser(unit(false))(pa, pb)).get() == b)
    assert(runPar(es)(choiceN(pa)(List(pa))).get() == a)
    assert(runPar(es)(choiceNViaChooser(pa)(List(pa))).get() == a)
  }

  test("join, flatMap") {
    val a = 0
    val pa = unit(a)

    assert(runPar(es)(join(unit(pa))).get() == a)
    assert(runPar(es)(joinViaFlatMap(unit(pa))).get() == a)
    assert(runPar(es)(flatMapViaJoin(pa)(unit)).get() == a)
  }

}
