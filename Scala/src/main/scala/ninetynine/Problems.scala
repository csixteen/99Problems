package ninetynine

import scala.util.Try


object Problems {
  //---------------------------------------------
  // Lists

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

  /** Problem 3 - find the kth element of a list (starting from 1) */
  def elementAt[A](as: List[A], n: Int): Option[A] =
    as match {
      case List(x, _) if n == 1 => Some(x)
      case h :: t if n > 1 => elementAt(t, n-1)
      case _ => None
    }

  def elementAt2[A](as: List[A], n: Int): Option[A] =
    as.drop(n-1).headOption

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
}
