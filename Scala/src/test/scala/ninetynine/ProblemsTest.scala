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
    assert(Problems.elementAt(List(5, 4, 3, 2, 1), 4) === Some(2))
    assert(Problems.elementAt(List(), 2) === None)
    assert(Problems.elementAt2(List(5, 4, 3, 2, 1), 4) === Some(2))
    assert(Problems.elementAt2(List(), 2) === None)
  }

  test("Problem 4 - myLength") {
    assert(Problems.myLength(List()) === 0)
    assert(Problems.myLength(List(1, 2, 3, 4, 5)) === 5)
    assert(Problems.myLength2(List()) === 0)
    assert(Problems.myLength2(List(1, 2, 3, 4, 5)) === 5)
  }

  test("Problem 5 - myReverse") {
    assert(Problems.myReverse(List()) === List())
    assert(Problems.myReverse(List(1, 2, 3, 4, 5)) === List(5, 4, 3, 2, 1))
    assert(Problems.myReverse2(List()) === List())
    assert(Problems.myReverse2(List(1, 2, 3, 4, 5)) === List(5, 4, 3, 2, 1))
  }

  test("Problem 6 - isPalindrome") {
    assert(Problems.isPalindrome(List()))
    assert(Problems.isPalindrome(List(1, 2, 3, 2, 1)))
    assert(!Problems.isPalindrome(List(1, 2, 3, 4, 5)))
  }

  test("Problem 7 - flatten") {
    assert(Problems.flatten(List()) === List())
    assert(Problems.flatten(List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4, 5))
    assert(Problems.flatten(List(1, List(2, List(3, 4)), 5)) === List(1, 2, 3, 4, 5))
    assert(Problems.flatten2(List()) === List())
    assert(Problems.flatten2(List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4, 5))
    assert(Problems.flatten2(List(1, List(2, List(3, 4)), 5)) === List(1, 2, 3, 4, 5))
  }

  test("Problem 8 - compress") {
    assert(Problems.compress(List()) === List())
    assert(Problems.compress(List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4, 5))
    assert(Problems.compress(List(1, 1, 1, 2, 3, 3, 4, 4, 4)) === List(1, 2, 3, 4))
    assert(Problems.compress2(List()) === List())
    assert(Problems.compress2(List(1, 2, 3, 4, 5)) === List(1, 2, 3, 4, 5))
    assert(Problems.compress2(List(1, 1, 1, 2, 3, 3, 4, 4, 4)) === List(1, 2, 3, 4))
  }
}
