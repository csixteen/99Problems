package ninetynine


object Problems {
  //---------------------------------------------
  // Lists

  // Problem 1 - find the last element of a list

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
}
