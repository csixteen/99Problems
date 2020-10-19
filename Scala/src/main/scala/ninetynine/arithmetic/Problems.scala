package ninetynine.arithmetic

import scala.math.pow
import ninetynine.lists.{Problems => PLists}


object Problems {
  private def sieve(as: LazyList[Int]): LazyList[Int] =
    as.head #:: sieve(as.tail).filter(x => x % as.head != 0)

  val primes: LazyList[Int] = sieve(LazyList.from(2))

  /** Problem 31 - Determine whether a given integer number is prime. */
  def isPrime(a: Int): Boolean =
    a > 1 && !primes.takeWhile(_ <= math.sqrt(a)).exists(a % _ == 0)


  //---------------------------------------------------------------

  /**
   * Problem 32 - Determine the greatest common divisor of two positive 
   * integer numbers.
   */
  def gcd(a: Int, b: Int): Int =
    if (b == 0) math.abs(a)
    else gcd(b, a % b)


  //----------------------------------------------------------------

  /**
   * Problem 33 - Determine whether two positive integer numbers are coprime.
   * Two numbers are coprime if their greatest common divisor equals 1.
   */
  def coprime(a: Int, b: Int): Boolean = 1 == gcd(a, b)


  //----------------------------------------------------------------

  /** Problem 34 - Calculate Euler's totient function phi(m) */
  def totientPhi(m: Int): Int =
    if (m == 1) 1
    else (1 to m).filter(coprime(_, m)).length


  //----------------------------------------------------------------

  @annotation.tailrec
  private def factors(n: Int, p: List[Int], acc: List[Int]): List[Int] = {
    if (n == 1) acc
    else {
      val xs = p.dropWhile(n % _ != 0)
      factors(n / xs.head, xs, xs.head :: acc)
    }
  }

  /**
   * Problem 35 - Determine the prime factors of a given positive integer.
   * Construct a flat list containing the prime factors in ascending order.
   */
  def primeFactors(n: Int): List[Int] =
    factors(n, primes.takeWhile(_ <= n/2).reverse.toList, List())


  //----------------------------------------------------------------

  /**
   * Problem 36 - Determine the prime factors of a given positive integer
   * with their multiplicity.
   */
  def primeFactorsMult(n: Int): List[(Int,Int)] =
    PLists.encode(primeFactors(n)).map { case (a,b) => (b,a) }


  //----------------------------------------------------------------

  /** Problem 37 - Calculate Euler's totient function phi (improved) */
  def totientPhiImproved(m: Int): Double =
    primeFactorsMult(m).map { case (p,m) => (p-1) * pow(p, m-1) }.product


  //----------------------------------------------------------------

  /** Problem 39 - A list of prime numbers */
  def primesRange(a: Int, b: Int): List[Int] =
    primes.takeWhile(_ < b).dropWhile(_ < a).toList


  //----------------------------------------------------------------

  /** Problem 40 - Goldbach's conjecture */
  def goldbach(n: Int): Either[String, (Int, Int)] = {
    @annotation.tailrec
    def go(
      candidates: List[Int], left: Int, right: Int, target: Int
    ): Either[String, (Int, Int)] = {
      if (left >= right) Left("left >= right")
      else
        target.compare(candidates(left) + candidates(right)) match {
          case 0 => Right((candidates(left), candidates(right)))
          case 1 => go(candidates, left+1, right, target)
          case -1 => go(candidates, left, right-1, target)
        }
    }

    val p = primes.takeWhile(_ <= n).toList
    go(p, 0, p.length-1, n)
  }


  //----------------------------------------------------------------

  /** 
   * Problem 41 - Given a range of integers by its lower and upper limit, print
   * a list of all even numbers and their Goldbach composiion.
   */
  def goldbachList(a: Int, b: Int, p: Int = 1): List[Either[String, (Int, Int)]] =
    (a to b)
      .filter(_ % 2 == 0)
      .map(goldbach(_)).toList
      .filter{
        case Right((x, y)) => (x > p) && (y > p)
        case _ => false
      }
}
