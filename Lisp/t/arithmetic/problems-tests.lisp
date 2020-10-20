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
(in-package :ninety-nine.tests.arithmetic)


(define-test test-is-prime
  (assert-true (nn.arithmetic:is-prime 47))
  (assert-true (nn.arithmetic:is-prime 2))
  (assert-false (nn.arithmetic:is-prime 1))
  (assert-false (nn.arithmetic:is-prime 49)))

(define-test test-gcd2
  (assert-equal 9 (nn.arithmetic:gcd2 36 63))
  (assert-equal 3 (nn.arithmetic:gcd2 -3 -6))
  (assert-equal 3 (nn.arithmetic:gcd2 -3 6)))

(define-test test-coprime
  (assert-true (nn.arithmetic:coprime 35 64))
  (assert-false (nn.arithmetic:coprime 21 49)))

(define-test test-totient-phi
  (assert-equal 1 (nn.arithmetic:totient-phi 1))
  (assert-equal 4 (nn.arithmetic:totient-phi 10)))

(define-test test-prime-factors
  (assert-equal '(3 3 5 7) (nn.arithmetic:prime-factors 315))
  (assert-equal '(2 47) (nn.arithmetic:prime-factors 94)))

(define-test test-prime-factors-mult
  (assert-equal '((3 2) (5 1) (7 1)) (nn.arithmetic:prime-factors-mult 315)))

(define-test test-totient-phi-improved
  (assert-equal 1 (nn.arithmetic:totient-phi-improved 1))
  (assert-equal 4 (nn.arithmetic:totient-phi-improved 10)))

(define-test test-primes-range
  (assert-equal '(11 13 17 19) (nn.arithmetic:primes-range 10 20)))

(define-test test-goldbach
  (let ((pair (nn.arithmetic:goldbach 28)))
    (assert-equal 5 (first pair))
    (assert-equal 23 (second pair))))

(define-test test-goldbach-list
  (assert-equal '((3 7) (5 7) (3 11) (3 13) (5 13) (3 17))
                (nn.arithmetic:goldbach-list 9 20))
  (assert-equal '((73 919) (61 1321) (67 1789) (61 1867))
                (nn.arithmetic:goldbach-list 4 2000 50)))
