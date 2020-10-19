import org.scalatest.funsuite.AnyFunSuite
import ninetynine.arithmetic._


class ArithmeticTest extends AnyFunSuite {
  test("Problem 31 - isPrime") {
    assert(Problems.isPrime(47))
    assert(!Problems.isPrime(1))
    assert(!Problems.isPrime(49))
  }

  test("Problem 32 - gcd") {
    assert(Problems.gcd(36, 63) == 9)
    assert(Problems.gcd(-3, -6) == 3)
    assert(Problems.gcd(-3, 6) == 3)
  }

  test("Problem 33 - coprime") {
    assert(Problems.coprime(35, 64))
    assert(!Problems.coprime(21, 49))
  }

  test("Problem 34 - totientPhi") {
    assert(Problems.totientPhi(1) == 1)
    assert(Problems.totientPhi(10) == 4)
  }

  test("Problem 35 - primeFactors") {
    assert(Problems.primeFactors(94) == List(2, 47))
    assert(Problems.primeFactors(315) == List(3, 3, 5, 7))
  }

  test("Problem 36 - primeFactorsMult") {
    assert(Problems.primeFactorsMult(315) == List((3, 2), (5, 1), (7, 1)))
  }

  test("Problem 37 - totientPhiImproved") {
    assert(Problems.totientPhiImproved(1) == 1)
    assert(Problems.totientPhiImproved(10) == 4)
  }

  test("Problem 39 - primesRange") {
    assert(Problems.primesRange(10, 20) == List(11, 13, 17, 19))
  }

  test("Problem 40 - goldbach") {
    assert(Problems.goldbach(28) == Right((5, 23)))
    assert(Problems.goldbach(10) == Right((3, 7)))
    assert(Problems.goldbach(12) == Right((5, 7)))
    assert(Problems.goldbach(14) == Right((3, 11)))
    assert(Problems.goldbach(16) == Right((3, 13)))
    assert(Problems.goldbach(18) == Right((5, 13)))
    assert(Problems.goldbach(20) == Right((3, 17)))
  }

  test("Problem 41 - goldbachList") {
    assert(Problems.goldbachList(9, 20) == 
      List(
        Right((3, 7)),
        Right((5, 7)),
        Right((3, 11)),
        Right((3, 13)),
        Right((5, 13)),
        Right((3, 17)),
      ))

    assert(Problems.goldbachList(4, 2000, 50) ==
      List(
        Right((73, 919)),
        Right((61, 1321)),
        Right((67, 1789)),
        Right((61, 1867)),
      ))
  }
}
