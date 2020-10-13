package ninetynine

import scala.collection.immutable.AbstractSeq
import scala.math.pow
import scala.util.{Random,Try}


object Problems {
  ////****************************************************
  ////
  ////                   Lists


  /** Problem 1 - find the last element of a list */
  def myLast[A](as: List[A]): Option[A] =
    as match {
      case List(x) => Some(x)
      case _::t => myLast(t)
      case _ => None
    }

  def myLast2[A](as: List[A]): Option[A] =
    as.foldLeft(None: Option[A])((_, a) => Some(a))

  def myLast3[A](as: List[A]): Option[A] =
    as.reverse.headOption


  //------------------------------------------------------

  /** Problem 2 - find the but last element in a list */
  def butLast[A](as: List[A]): Option[A] =
    as match {
      case List(x, _) => Some(x)
      case _ :: t => butLast(t)
      case _ => None
    }

  def butLast2[A](as: List[A]): Option[A] =
    Try(as.reverse.tail.headOption).getOrElse(None: Option[A])

  def butLast3[A](as: List[A]): Option[A] =
    Try(as.init.lastOption).getOrElse(None: Option[A])


  //------------------------------------------------------

  /** Problem 3 - find the kth element of a list (starting from 1) */
  def elementAt[A](as: List[A], n: Int): Option[A] =
    as match {
      case List(x, _) if n == 1 => Some(x)
      case h :: t if n > 1 => elementAt(t, n-1)
      case _ => None
    }

  def elementAt2[A](as: List[A], n: Int): Option[A] =
    as.drop(n-1).headOption


  //------------------------------------------------------
  
  /** Problem 4 - find the number of elements in a list */
  def myLength[A](as: List[A]): Int = {
    @annotation.tailrec
    def go[A](xs: List[A], acc: Int): Int =
      xs match {
        case List() => acc
        case _ :: t => go(t, acc + 1)
      }

    go(as, 0)
  }

  def myLength2[A](as: List[A]): Int =
    as.foldLeft(0)((acc, _) => acc + 1)


  //-------------------------------------------------------

  /** Problem 5 - reverse a list */
  def myReverse[A](as: List[A]): List[A] = {
    @annotation.tailrec
    def go(xs: List[A], acc: List[A]): List[A] =
      xs match {
        case List() => acc
        case h :: t => go(t, h :: acc)
      }

    go(as, List())
  }

  def myReverse2[A](as: List[A]): List[A] =
    as.foldLeft(List[A]())((b, a) => a :: b)


  //-------------------------------------------------------

  /** Problem 6 - isPalindrome */
  def isPalindrome[A](as: List[A]): Boolean =
    as == as.reverse


  //------------------------------------------------------

  /** Problem 7 - flatten */
  def flatten(as: List[Any]): List[Any] =
    as flatMap {
      case xs: List[_] => flatten(xs)
      case x => List(x)
    }

  def flatten2(as: List[Any]): List[Any] = {
    def go(xs: List[Any], acc: List[Any]): List[Any] =
      xs match {
        case List() => acc
        case (x: List[_]) :: y => go(y, go(x, acc))
        case h :: t => go(t, h :: acc)
      }

    go(as, List()).reverse
  }


  //------------------------------------------------------

  /** Problem 8 - compress */
  def compress[A](as: List[A]): List[A] =
    as match {
      case Nil => List()
      case h::t => h :: compress(t.dropWhile(_ == h))
    }

  def compress2[A](as: List[A]): List[A] =
    as match {
      case x :: (ys @ List(y, _*)) => if (x == y) compress(ys) else x :: compress(ys)
      case _ => List()
    }


  //------------------------------------------------------
  
  /** Problem 9 - pack consecutive duplicates into sublists */
  def pack[A](as: List[A]): List[List[A]] =
    as match {
      case Nil => List()
      case h::t => {
        val (left, right) = t.span(_ == h)
        (h :: left) :: pack(right)
      }
    }


  //-------------------------------------------------------

  /** Problem 10 - Run-length encoding of a list */
  def encode[A](as: List[A]): List[(Int, A)] =
    pack(as).map(g => (g.length, g.head))


  //-------------------------------------------------------

  /** Problem 11 - Run-length encoding modified */
  def encodeModified[A](as: List[A]): List[ListItem[A]] =
    pack(as).map(g => {
      if (g.length == 1) Single(g.head)
      else Multiple(g.length, g.head)
    })


  //-------------------------------------------------------

  /** Problem 12 - Decode modified run-length */
  def decodeModified[A](as: List[ListItem[A]]): List[A] =
    as.flatMap(a => a match {
        case Single(s) => List(s)
        case Multiple(n, s) => List.fill(n)(s)
    })


  //-------------------------------------------------------

  /** Problem 14 - Duplicate the elements in a list */
  def duplicate[A](as: List[A]): List[A] =
    as.flatMap(x => List(x, x))

  
  //-------------------------------------------------------

  /**
   * Problem 15 - Replicate the elements of a list a given
   * number of times.
   */
  def replicate[A](as: List[A], n: Int): List[A] =
    as.flatMap(List.fill(n)(_))


  //--------------------------------------------------------

  /** Problem 16 - drop every N elements of a list */
  def dropEvery[A](as: List[A], n: Int): List[A] =
    as match {
      case Nil => Nil
      case _ => as.take(n-1) ::: dropEvery(as.drop(n), n)
    }


  //--------------------------------------------------------

  /** Problem 17 - Split a list into two parts */
  def splitAt[A](as: List[A], n: Int): (List[A], List[A]) =
    (as.take(n), as.drop(n))


  //--------------------------------------------------------

  /** Problem 18 - Extract a slice from a list */
  def slice[A](as: List[A], start: Int, end: Int): Either[String,List[A]] =
    if (start < 1 || start > end) Left("Invalid values for start and end")
    else Right(as drop(start-1) take(end-start+1))


  //--------------------------------------------------------

  /** Problem 19 - Rotate a list N places to the left */
  def rotate[A](as: List[A], n: Int): List[A] = {
    if (as.isEmpty) Nil
    else {
      val (head, tail) = splitAt(as, java.lang.Math.floorMod(n, as.length))
      tail ::: head
    }
  }


  //---------------------------------------------------------

  /** Problem 20 - Remove the kth element of a list */
  def removeAt[A](as: List[A], n: Int): Option[(A, List[A])] = 
    if (n < 1 || n > as.length) None
    else {
      val elem = as(n-1)
      Some((elem, as.take(n-1) ::: as.drop(n)))
    }


  //----------------------------------------------------------

  /** Problem 21 - Insert an element at a given position in a list */
  def insertAt[A](as: List[A], a: A, n: Int): List[A] =
    as.take(n-1) ::: List(a) ::: as.drop(n-1)


  //----------------------------------------------------------

  /** Problem 22 - Create a list containing all integers within a given range */
  def range(start: Int, end: Int): List[Int] = {
    @annotation.tailrec
    def go(a: Int, b: Int, acc: List[Int]): List[Int] =
      if (b < a) acc
      else go(a, b-1, b :: acc)

    go(start, end, List())
  }


  //-----------------------------------------------------------

  /** Problem 23 - Extract a given number of randomly selected elements from a list */
  def rndSelect[A](as: List[A], n: Int): List[A] = {
    if (n == 0 || as.isEmpty) List()
    else {
      val pos = (new Random()).nextInt(as.length) + 1
      removeAt(as, pos) match {
        case Some((elem, rest)) => elem :: rndSelect(rest, n-1)
        case _ => List()  // Shouldn't be reached
      }
    }
  }


  //------------------------------------------------------------

  /** Problem 24 - Draw N different random numbers from the set 1..M */
  def diffSelect(n: Int, m: Int): List[Int] =
    rndSelect(range(1, m), n)


  //-----------------------------------------------------------

  /** Problem 25 - generate a random permutation of the elements of a list */
  def rndPermutation[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case x::xs => {
        val rest = rndPermutation(xs)
        val pos = (new Random()).nextInt(Math.max(1, rest.length))
        val (left, right) = splitAt(rest, pos)
        left ::: (x::right)
      }
    }


  //------------------------------------------------------------

  /**
   * Problem 26 - Generate the combinations of K distinct objects chosen
   * from the N elements of a list.
   */
  def combinations[A](k: Int, as: List[A]): List[List[A]] = {
    if (k == 0) List(List())
    else
      as match {
        case Nil => Nil
        case h::t => {
          val withHead = combinations(k-1, t).map(h :: _)
          val withoutHead = combinations(k, t)
          withHead ::: withoutHead
        }
      }
  }


  //-------------------------------------------------------------

  // This version of `combinations` yields not only the elements we selected,
  // but also the elements that we haven't selected.
  private def combModified[A](k: Int, as: List[A]): List[(List[A], List[A])] = {
    if (k == 0) List((List(), as))
    else
      as match {
        case Nil => Nil
        case x::xs => {
          val withHead = combModified(k-1, xs).map { case (ys, zs) => (x::ys, zs) }
          val withoutHead = combModified(k, xs).map { case (ys, zs) => (ys, x::zs) }
          withHead ::: withoutHead
        }
      }
  }

  /** Problem 27 - Group the elements of a set into disjoint subsets */
  def group[A](k: List[Int], as: List[A]): List[List[List[A]]] = {
    (k, as) match {
      case (Nil, _) => List(List())
      case (n::ns, xs) => {
        val pairs = combModified(n, xs)
        pairs.flatMap { case (g, rs) => group(ns, rs).map(g::_) }
      }
    }
  }


  //---------------------------------------------------------------

  /** Problem 28 - sorting a list of lists according to length of sublists */

  // a-) Sorting a list of lists according to the length of the sublists
  def lengthSort[A](as: List[String]): List[String] =
    as.sortBy(_.length)

  // b-) Sorting a list of lists according to the frequency of the length
  // of the sublists
  def lengthFreqSort[A](as: List[String]): List[String] = {
    val lengths = as.groupBy(_.length)
    val freqs = lengths.values.groupBy(_.length)
    freqs.values.flatMap(_.flatten.toList.sorted).toList
  }


  ////****************************************************
  ////
  ////                Arithmetic


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
    encode(primeFactors(n)).map { case (a,b) => (b,a) }


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

  def goldbach(n: Int): (Int, Int) = {
    @annotation.tailrec
    def gold(candidates: List[Int], left: Int, right: Int, target: Int): (Int, Int) = {
      target.compare(candidates(left) + candidates(right)) match {
        case 0 => (candidates(left), candidates(right))
        case 1 => gold(candidates, left+1, right, target)
        case -1 => gold(candidates, left, right-1, target)
      }
    }

    val p = primes.takeWhile(_ <= n).toList
    gold(p, 0, p.length-1, n)
  }
}
