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
}
