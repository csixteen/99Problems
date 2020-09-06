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

  test("Problem 2 - butLast") {
    assert(Problems.butLast(List()) === None)
    assert(Problems.butLast(List(5, 4, 3, 2, 1)) === Some(2))
    assert(Problems.butLast(List(1, 2)) === Some(1))
    assert(Problems.butLast2(List()) === None)
    assert(Problems.butLast2(List(5, 4, 3, 2, 1)) === Some(2))
    assert(Problems.butLast2(List(1, 2)) === Some(1))
    assert(Problems.butLast3(List()) === None)
    assert(Problems.butLast3(List(5, 4, 3, 2, 1)) === Some(2))
    assert(Problems.butLast3(List(1, 2)) === Some(1))
  }

  test("Problem 3 - elementAt") {
    assert(Problems.elementAt(List(5, 4, 3, 2, 1), 4) == Some(2))
    assert(Problems.elementAt(List(), 2) == None)
    assert(Problems.elementAt2(List(5, 4, 3, 2, 1), 4) == Some(2))
    assert(Problems.elementAt2(List(), 2) == None)
  }
}
