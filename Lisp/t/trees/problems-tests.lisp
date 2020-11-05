; Copyright 2020 Pedro Rodrigues
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#-xlisp-test
(in-package :ninety-nine.tests.trees)

(define-test test-is-tree
  (assert-true (nn.trees:is-tree '(a (b nil nil) nil)))
  (assert-false (nn.trees:is-tree '(a (b nil nil)))))

(define-test test-cbal-tree
  (assert-equal '(nil) (nn.trees:cbal-tree 0))
  (assert-equal '((1 nil nil)) (nn.trees:cbal-tree 1))
  (assert-equal '((1 nil (1 nil nil))
                  (1 (1 nil nil) nil))
                (nn.trees:cbal-tree 2)))

(define-test test-mirror
  (assert-true (nn.trees:mirror nil nil))
  (assert-false (nn.trees:mirror nil '(1 nil nil))))

(define-test test-symmetric
  (assert-false (nn.trees:symmetric '(1 (1 nil nil) nil)))
  (assert-true (nn.trees:symmetric nil))
  (assert-true (nn.trees:symmetric '(1 (1 nil nil) (1 nil nil)))))
