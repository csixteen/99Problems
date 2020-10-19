/**
 * MIT License
 *
 * Copyright (c) 2020 Pedro Rodrigues
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package ninetynine.logicandcodes


object Problems {
  /**
   * Problem 46 - Define the predicates and/2, or/2, nand/2, nor/2, xor/2,
   * impl/2 and equ/2 (for logical equivalence) which succeed or fail
   * according to the result of their respective operations.
   */
  def not(a: Boolean): Boolean =
    a match {
      case true => false
      case false => true
    }

  def and(a: Boolean, b: Boolean): Boolean =
    (a, b) match {
      case (true, true) => true
      case _ => false
    }

  def or(a: Boolean, b: Boolean): Boolean =
    (a, b) match {
      case (false, false) => false
      case _ => true
    }

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def xor(a: Boolean, b: Boolean): Boolean = and(or(a, b), nand(a, b))


  //----------------------------------------------------------------

  /** Problem 49 - Gray codes */
  def gray(n: Int): List[String] = {
    @annotation.tailrec
    def go(i: Int, acc: List[String]): List[String] =
      if (i == 1) acc
      else go(i-1, acc.map(_.prepended('0')) ::: acc.reverse.map(_.prepended('1')))

    go(n, List("0", "1"))
  }


  //----------------------------------------------------------------

  /** Problem 50 - Huffman codes */
  def huffman(freqs: List[(Char, Int)]): List[(Char, String)] = {
    val leafs = freqs.sortBy(_._2).map{ case (c, f) => (f, Leaf(c)) }
    val tree = buildHTree(leafs)
    serialize(tree).sortBy(_._1)
  }

  private def buildHTree(leafs: List[(Int, HTree[Char])]): HTree[Char] =
    leafs match {
      case List((_, t)) => t
      case (f1, t1) :: (f2, t2) :: ls => {
        val xs = insertOrdered(ls, (f1+f2, Branch(t1, t2)))((a, b) => a._1 < b._1)
        buildHTree(xs)
      }
    }

  private def insertOrdered[A](as: List[A], elem: A)(fn: (A, A) => Boolean): List[A] =
    as match {
      case Nil => List(elem)
      case x :: xs if (fn(elem, x)) => elem :: as
      case x :: xs if (!fn(elem, x)) => x :: insertOrdered(xs, elem)(fn)
    }

  private def serialize(tree: HTree[Char]): List[(Char, String)] =
    tree match {
      case Leaf(x) => List((x, ""))
      case Branch(l, r) =>
        val left = serialize(l).map{ case (c, code) => (c, "0" + code)}
        val right = serialize(r).map{ case (c, code) => (c, "1" + code)}
        left ::: right
    }
}
