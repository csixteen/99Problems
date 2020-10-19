import org.scalatest.funsuite.AnyFunSuite
import ninetynine.logicandcodes._


class LogicAndCodesTest extends AnyFunSuite {
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

  test("Problem 50 - huffman") {
    assert(Problems.huffman(
      List(('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5))) ==
        List(
          ('a', "0"),
          ('b', "101"),
          ('c', "100"),
          ('d', "111"),
          ('e', "1101"),
          ('f', "1100")))
  }
}
