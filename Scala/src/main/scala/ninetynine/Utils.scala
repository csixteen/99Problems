package ninetynine


//---------------------------------------------------------
// Needed for Problem 11 - Run-length encoding modified

sealed trait ListItem[+A]
case class Single[A](value: A) extends ListItem[A]
case class Multiple[A](n: Int, value: A) extends ListItem[A]
