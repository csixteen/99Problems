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

(define-test test-palindrome?
  (assert-true (nn:palindrome? nil))
  (assert-true (nn:palindrome? '(1 2 3 2 1)))
  (assert-false (nn:palindrome? '(1 2 3 4 5))))

(define-test test-flatten
  (assert-equal nil (nn:flatten nil))
  (assert-equal '(1 2 3 4 5) (nn:flatten '(1 2 3 4 5)))
  (assert-equal '(1 2 3 4 5) (nn:flatten '((1 (2 3)) 4 (5)))))

(define-test test-compress
  (assert-equal nil (nn:compress nil))
  (assert-equal '(1 2 3) (nn:compress '(1 2 3)))
  (assert-equal '(1 2 3) (nn:compress '(1 1 1 2 2 3 3 3 3))))

(define-test test-pack
  (assert-equal nil (nn:pack nil))
  (assert-equal '((1) (2) (3)) (nn:pack '(1 2 3)))
  (assert-equal '((1 1 1 1) (2) (3 3)) (nn:pack '(1 1 1 1 2 3 3))))

(define-test test-encode
  (assert-equal nil (nn:encode nil))
  (assert-equal '((1 1) (1 2) (1 3)) (nn:encode '(1 2 3)))
  (assert-equal '((4 1) (1 2) (2 3)) (nn:encode '(1 1 1 1 2 3 3))))

(define-test test-encode-modified
  (assert-equal nil (nn:encode-modified nil))
  (assert-equal '(1 2 3) (nn:encode-modified '(1 2 3)))
  (assert-equal '((4 1) 2 (2 3)) (nn:encode-modified '(1 1 1 1 2 3 3))))

(define-test test-decode-modified
  (assert-equal nil (nn:decode-modified nil))
  (assert-equal '(1 2 3) (nn:decode-modified '(1 2 3)))
  (assert-equal '(1 1 1 1 2 3 3) (nn:decode-modified '((4 1) 2 (2 3)))))

(define-test test-duplicate
  (assert-equal nil (nn:duplicate nil))
  (assert-equal '(1 1 2 2 3 3) (nn:duplicate '(1 2 3))))

(define-test test-replicate
  (assert-equal nil (nn:replicate nil 2))
  (assert-equal '(1 2 3) (nn:replicate '(1 2 3) 1))
  (assert-equal '(1 1 1 2 2 2 3 3 3) (nn:replicate '(1 2 3) 3)))

(define-test test-drop-every
  (assert-equal nil (nn:drop-every 3 nil))
  (assert-equal '(1 2) (nn:drop-every 3 '(1 2)))
  (assert-equal '(1 2 4 5 7) (nn:drop-every 3 '(1 2 3 4 5 6 7))))
