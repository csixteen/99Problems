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
(in-package :ninety-nine.tests)


(define-test test-my-last
  (assert-equal 1 (nn:my-last '(5 4 3 2 1)))
  (assert-equal 1 (nn:my-last2 '(5 4 3 2 1)))
  (assert-equal 1 (nn:my-last3 '(5 4 3 2 1)))
  (assert-equal 1 (nn:my-last4 '(5 4 3 2 1))))

(define-test test-but-last
  (assert-equal 2 (nn:but-last '(5 4 3 2 1))))

(define-test test-element-at
  (assert-error 'error (nn:element-at '() 3))
  (assert-equal 2 (nn:element-at '(5 4 3 2 1) 4)))

(define-test test-my-length
  (assert-equal 0 (nn:my-length '()))
  (assert-equal 5 (nn:my-length '(1 2 3 4 5))))

(define-test test-my-reverse
  (assert-equal nil (nn:my-reverse nil))
  (assert-equal '(5 4 3 2 1) (nn:my-reverse '(1 2 3 4 5)))
  (assert-equal nil (nn:my-reverse2 nil))
  (assert-equal '(5 4 3 2 1) (nn:my-reverse2 '(1 2 3 4 5))))