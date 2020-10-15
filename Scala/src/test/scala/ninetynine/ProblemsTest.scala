import org.scalatest.funsuite.AnyFunSuite
import ninetynine._


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

  test("Problem 9 - pack") {
    assert(Problems.pack(List()) === List())
    assert(Problems.pack(List(1, 2, 3)) === List(List(1), List(2), List(3)))
    assert(Problems.pack(List(1, 1, 1, 2, 3, 3)) === List(List(1, 1, 1), List(2), List(3, 3)))
  }

  test("Problem 10 - encode") {
    assert(Problems.encode(List()) === List())
    assert(Problems.encode(List(1, 2, 3)) === List((1, 1), (1, 2), (1, 3)))
    assert(Problems.encode(List(1, 1, 1, 2, 3, 3)) === List((3, 1), (1, 2), (2, 3)))
  }

  test("Problem 11 - encodeModified") {
    assert(Problems.encodeModified(List()) === List())
    assert(Problems.encodeModified(List(1, 2, 3)) === List(Single(1), Single(2), Single(3)))
    assert(Problems.encodeModified(List(1, 1, 1, 2, 3, 3)) 
      === List(Multiple(3, 1), Single(2), Multiple(2, 3)))
  }

  test("Problem 12 - decodeModified") {
    assert(Problems.decodeModified(List()) === List())
    assert(Problems.decodeModified(List(Single(1), Single(2), Single(3))) === List(1, 2, 3))
    assert(Problems.decodeModified(List(Multiple(3, 1), Single(2), Multiple(2, 3)))
      === List(1, 1, 1, 2, 3, 3))
  }

  test("Problem 14 - duplicate") {
    assert(Problems.duplicate(List()) === List())
    assert(Problems.duplicate(List(1, 2, 3)) === List(1, 1, 2, 2, 3, 3))
  }

  test("Problem 15 - replicate") {
    assert(Problems.replicate(List(), 2) === List())
    assert(Problems.replicate(List(1, 2, 3), 1) === List(1, 2, 3))
    assert(Problems.replicate(List(1, 2, 3), 3) === List(1, 1, 1, 2, 2, 2, 3, 3, 3))
  }

  test("Problem 16 - dropEvery") {
    assert(Problems.dropEvery(List(), 3) === List())
    assert(Problems.dropEvery(List(1, 2), 3) === List(1, 2))
    assert(Problems.dropEvery(List(1, 2, 3, 4 ,5, 6, 7), 3) === List(1, 2, 4, 5, 7))
  }

  test("Problem 17 - splitAt") {
    assert(Problems.splitAt(List(), 2) === (List(), List()))
    assert(Problems.splitAt(List(1), 3) === (List(1), List()))
    assert(Problems.splitAt(List(1, 2, 3, 4, 5, 6), 3) === (List(1, 2, 3), List(4, 5, 6)))
  }

  test("Problem 18 - slice") {
    assert(Problems.slice(List(1, 2, 3), 0, 3).isLeft)
    assert(Problems.slice(List(1, 2, 3), 3, 1).isLeft)
    assert(Problems.slice(List(1, 2, 3, 4, 5, 6), 3, 5) === Right(List(3, 4, 5)))
  }

  test("Problem 19 - rotate") {
    assert(Problems.rotate(List(), 3) === List())
    assert(Problems.rotate(List(1), 3) === List(1))
    assert(Problems.rotate(List(1, 2, 3, 4, 5), 3) === List(4, 5, 1, 2, 3))
  }

  test("Problem 20 - removeAt") {
    assert(Problems.removeAt(List(), 3) === None)
    assert(Problems.removeAt(List(1, 2, 3, 4), 2) === Some((2, List(1, 3, 4))))
  }

  test("Problem 21 - insertAt") {
    assert(Problems.insertAt(List(), 1, 1) === List(1))
    assert(Problems.insertAt(List(1, 2, 3, 4), 5, 2) === List(1, 5, 2, 3, 4))
  }

  test("Problem 22 - range") {
    assert(Problems.range(5, 4) === List())
    assert(Problems.range(4, 4) === List(4))
    assert(Problems.range(4, 9) === List(4, 5, 6, 7, 8, 9))
  }

  test("Problem 23 - rndSelect") {
    val lst = List('a', 'b', 'c', 'd', 'e', 'f')
    val as1 = Problems.rndSelect(lst, 3)
    assert(as1.length == 3)
    assert(as1.forall(lst.contains(_)))
  }

  test("Problem 24 - diffSelect") {
    val res = Problems.diffSelect(6, 49)
    assert(res.length == 6)
    assert(res.max < 50)
    assert(res.distinct.length == 6)
  }

  test("Problem 25 - rndPermutation") {
    val lst = List('a', 'b', 'c', 'd', 'e', 'f')
    val res = Problems.rndPermutation(lst)
    assert(lst != res)
    assert(res.sorted == lst)
  }

  test("Problem 26 - combinations") {
    val lst = List('a', 'b', 'c', 'd')
    val res = Problems.combinations(2, lst)
    assert(res == List(
      List('a', 'b'),
      List('a', 'c'),
      List('a', 'd'),
      List('b', 'c'),
      List('b', 'd'),
      List('c', 'd')))
  }

  test("Problem 27 - group") {
    assert(Problems.group(
      List(2, 3, 4),
      List("aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida")).length 
      == 1260)
  }

  test("Problem 28 - lengthSort") {
    assert(Problems.lengthSort(
      List("abc", "de", "fgh", "de", "ijkl", "mn", "o")) ==
        List("o", "de", "de", "mn", "abc", "fgh", "ijkl"))
  }

  test("Problem 28 - lengthFreqSort") {
    assert(Problems.lengthFreqSort(
      List("abc", "de", "fgh", "de", "ijkl", "mn", "o")) ==
        List("ijkl", "o", "abc", "fgh", "de", "de", "mn"))
  }

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

  test("Problem 46 - and/2") {
    val andTable = 
      List(
        (false, false, false),
        (false, true, false),
        (true, false, false),
        (true, true, true))

    andTable.foreach {
      case (a, b, expected) => assert(Problems.and(a, b) == expected)
    }
  }

  test("Problem 46 - (and A (or A B))") {
    val truthTable =
      List(
        (true, true, true),
        (true, false, true),
        (false, true, false),
        (false, false, false))

    truthTable.foreach {
      case (a, b, expected) => assert(Problems.and(a, Problems.or(a, b)) == expected)
    }
  }

  test("Problem 49 - gray") {
    assert(Problems.gray(1) == List("0", "1"))
    assert(Problems.gray(2) == List("00", "01", "11", "10"))
    assert(Problems.gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }
}
