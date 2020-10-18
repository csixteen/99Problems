package ninetynine


//---------------------------------------------------------
// Needed for Problem 11 - Run-length encoding modified

sealed trait ListItem[+A]
case class Single[A](value: A) extends ListItem[A]
case class Multiple[A](n: Int, value: A) extends ListItem[A]


//---------------------------------------------------------
// Needed for Problem 50 - Huffman coding

sealed trait HTree[+A]
case class Leaf[A](value: A) extends HTree[A]
case class Branch[A](left: HTree[A], right: HTree[A]) extends HTree[A]
