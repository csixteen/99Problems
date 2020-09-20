package ninetynine

import scala.util.Try


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
}
