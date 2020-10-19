package ninetynine.logicandcodes


//---------------------------------------------------------
// Needed for Problem 50 - Huffman coding

sealed trait HTree[+A]
case class Leaf[A](value: A) extends HTree[A]
case class Branch[A](left: HTree[A], right: HTree[A]) extends HTree[A]
