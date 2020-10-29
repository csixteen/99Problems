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

package ninetynine.trees


object Problems {
  /** Problem 55 - Construct completely balanced binary trees */
  def cbalTree[A](n: Int, elem: A): List[BTree[A]] = {
    if (n == 0) List(EmptyTree)
    else {
      val (q, r) = ((n-1) / 2, (n-1) % 2)
      for {
        i <- (q to q+r).toList
        left <- cbalTree(i, elem)
        right <- cbalTree(n-i-1, elem)
      } yield Branch(elem, left, right)
    }
  }


  //-------------------------------------------------------------

  /** Problem 56 - Symmetric binary trees */
  def mirror[A](a: BTree[A], b: BTree[A]): Boolean =
    (a, b) match {
      case (EmptyTree, EmptyTree) => true
      case (Branch(_, l1, r1), Branch(_, l2, r2)) => mirror(l1, r2) && mirror(r1, l2)
      case _ => false
    }

  def symmetric[A](tree: BTree[A]): Boolean =
    tree match {
      case EmptyTree => true
      case Branch(_, l, r) => mirror(l, r)
      case _ => false
    }


  //-------------------------------------------------------------

  /** Problem 57 - Binary Search Trees */
  def construct(as: List[Int]): BTree[Int] =
    as.foldLeft(EmptyTree: BTree[Int])((acc, e) => add(e, acc))

  private def add[A](elem: A, tree: BTree[A])(implicit ev: A => Ordered[A]): BTree[A] =
    tree match {
      case EmptyTree => Branch(elem, EmptyTree, EmptyTree)
      case y@Branch(x, l, r) =>
        elem.compareTo(x) match {
          case 0 => y
          case 1 => Branch(x, l, add(elem, r))
          case -1 => Branch(x, add(elem, l), r)
        }
    }


  //-------------------------------------------------------------

  /**
   * Problem 58 - Generate-and-test paradigm. Apply the generate-and-test
   * paradigm to construct all symmetric, completely balanced binary trees
   * with a given number of nodes.
   */
  def symCbalTrees[A](n: Int, elem: A): List[BTree[A]] =
    if (n % 2 == 0) Nil
    else cbalTree(n/2, elem).map(t => Branch(elem, t, reverseTree(t)))

  private def reverseTree[A](t: BTree[A]): BTree[A] =
    t match {
      case EmptyTree => EmptyTree
      case Branch(v, l, r) => Branch(v, reverseTree(r), reverseTree(l))
    }


  //-------------------------------------------------------------

  /** Problem 59 - Construct height-balanced binary trees */
  def hbalTree[A](n: Int, elem: A): List[BTree[A]] =
    n match {
      case 0 => List(EmptyTree)
      case 1 => List(Branch(elem, EmptyTree, EmptyTree))
      case n =>
        for {
          (l, r) <- List((n-2, n-1), (n-1, n-1), (n-1, n-2))
          left <- hbalTree(l, elem)
          right <- hbalTree(r, elem)
        } yield Branch(elem, left, right)
    }
}
