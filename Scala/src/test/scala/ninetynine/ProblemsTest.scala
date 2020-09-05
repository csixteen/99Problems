import org.scalatest.funsuite.AnyFunSuite
import ninetynine.Problems


class ProblemsTest extends AnyFunSuite {
  test("Problem 1 - myLast") {
    assert(Problems.myLast(List(5, 4, 3, 2, 1)) === Some(1))
    assert(Problems.myLast(List()) === None)
    assert(Problems.myLast2(List(5, 4, 3, 2, 1)) === Some(1))
    assert(Problems.myLast2(List()) === None)
    assert(Problems.myLast3(List(5, 4, 3, 2, 1)) === Some(1))
    assert(Problems.myLast3(List()) === None)
  }
}
