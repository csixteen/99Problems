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
}
