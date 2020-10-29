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

import org.scalatest.funsuite.AnyFunSuite
import ninetynine.trees._


class TreesTest extends AnyFunSuite {
  test("Problem 55 - cbalTree") {
    assert(Problems.cbalTree(0, 'x') == List(EmptyTree))
    assert(Problems.cbalTree(1, 'x') == List(Branch('x', EmptyTree, EmptyTree)))
    assert(Problems.cbalTree(2, 'x') ==
      List(
        Branch('x', EmptyTree, Branch('x', EmptyTree, EmptyTree)),
        Branch('x', Branch('x', EmptyTree, EmptyTree), EmptyTree)))
  }

  test("Problem 56 - symmetric") {
    assert(!Problems.symmetric(
      Branch('x',
        Branch('x', EmptyTree, EmptyTree),
        EmptyTree
      )))

    assert(Problems.symmetric(
      Branch('x',
        Branch('x', EmptyTree, EmptyTree),
        Branch('x', EmptyTree, EmptyTree)
      )))
  }

  test("Problem 57 - construct") {
    assert(Problems.construct(List(3, 2, 5, 7, 1)) ==
      Branch(3,
        Branch(2,
          Branch(1, EmptyTree, EmptyTree),
          EmptyTree,
        ),
        Branch(5,
          EmptyTree,
          Branch(7, EmptyTree, EmptyTree))))
  }

  test("Problem 58 - symCbalTrees") {
    assert(Problems.symCbalTrees(5, 'x') ==
      List(
        Branch(
          'x',
          Branch('x', EmptyTree, Branch('x', EmptyTree, EmptyTree)),
          Branch('x', Branch('x', EmptyTree, EmptyTree), EmptyTree),
        ),
        Branch(
          'x',
          Branch('x', Branch('x', EmptyTree, EmptyTree), EmptyTree),
          Branch('x', EmptyTree, Branch('x', EmptyTree, EmptyTree)),
        ),
      )
    )
  }

  test("Problem 59 - hbalTree") {
    assert(Problems.hbalTree(3, 'x').take(3) ==
      List(
        Branch('x',
          Branch('x', EmptyTree, EmptyTree),
          Branch('x', EmptyTree, Branch('x', EmptyTree, EmptyTree))),
        Branch('x',
          Branch('x', EmptyTree, EmptyTree),
          Branch('x', Branch('x', EmptyTree, EmptyTree), Branch('x', EmptyTree, EmptyTree))),
        Branch('x',
          Branch('x', EmptyTree, EmptyTree),
          Branch('x', Branch('x', EmptyTree, EmptyTree), EmptyTree)),
      )
    )
  }
}
