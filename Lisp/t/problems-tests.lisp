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
  (assert-equal 1 (nn:my-last '(5 4 3 2 1))))

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
  (assert-equal '(5 4 3 2 1) (nn:my-reverse '(1 2 3 4 5))))

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
  (assert-equal nil (nn:drop-every nil 3))
  (assert-equal '(1 2) (nn:drop-every '(1 2) 3))
  (assert-equal '(1 2 4 5 7) (nn:drop-every '(1 2 3 4 5 6 7) 3)))

(define-test test-split-at
  (multiple-value-bind (left right) (nn:split-at nil 3)
    (assert-nil left)
    (assert-nil right))
  (multiple-value-bind (left right) (nn:split-at '(1) 3)
    (assert-equal left '(1))
    (assert-nil right))
  (multiple-value-bind (left right) (nn:split-at '(1 2 3 4 5 6) 3)
    (assert-equal left '(1 2 3))
    (assert-equal right '(4 5 6))))

(define-test test-slice
  (assert-error 'error (nn:slice '(1 2 3 4 5 6) 0 3))
  (assert-error 'error (nn:slice '(1 2 3 4 5 6) 3 1))
  (assert-error 'error (nn:slice '(1 2 3) 1 4))
  (assert-equal '(1 2 3) (nn:slice '(1 2 3) 1 3))
  (assert-equal '(3 4 5) (nn:slice '(1 2 3 4 5 6 7) 3 5)))

(define-test test-rotate
  (assert-nil (nn:rotate nil 3))
  (assert-equal '(1 2 3) (nn:rotate '(1 2 3) 0))
  (assert-equal '(4 5 1 2 3) (nn:rotate '(1 2 3 4 5) 3)))

(define-test test-remove-at
  (assert-error 'error (nn:remove-at '() 2))
  (assert-error 'error (nn:remove-at '(1 2 3) 4))
  (multiple-value-bind (elem residue) (nn:remove-at '(1 2 3 4) 2)
    (assert-equal '(1 3 4) residue)
    (assert-equal 2 elem)))

(define-test test-insert-at
  (assert-equal '(1) (nn:insert-at nil 1 1))
  (assert-equal '(1 5 2 3 4) (nn:insert-at '(1 2 3 4) 5 2)))

(define-test test-range
  (assert-equal nil (nn:range 5 3))
  (assert-equal '(4) (nn:range 4 4))
  (assert-equal '(4 5 6 7 8 9) (nn:range 4 9)))

(define-test test-rnd-select
  (let* ((lst '(a b c d e f))
         (res (rnd-select lst 3)))
    (assert-equal 3 (length res))
    (assert-true (every #'(lambda (x) (member x lst))
                        res))))

(define-test test-diff-select
  (let ((res (diff-select 6 49)))
    (assert-equal 6 (length res))
    (assert-equal 6 (length (remove-duplicates res)))
    (assert-true (< (apply #'max res) 50))))

(define-test test-rnd-permutation
  (let* ((lst '(#\a #\b #\c #\d #\e #\f))
         (res (nn:rnd-permutation lst)))
    (assert-false (equal lst res))
    (assert-equal (sort res #'char<) lst)))

(define-test test-combinations
  (let* ((lst '(#\a #\b #\c #\d))
         (res (nn:combinations 2 lst)))
    (assert-equal res
                  '((#\a #\b)
                    (#\a #\c)
                    (#\a #\d)
                    (#\b #\c)
                    (#\b #\d)
                    (#\c #\d)))))

(define-test test-comb-modified
  (let* ((lst '(#\a #\b #\c #\d))
         (res (nn:comb-modified 2 lst)))
    (assert-equal 6 (length res))))

(define-test test-group
  (let ((res (group '(2 3 4)
                    '("aldo" "beat" "carla" "david" "evi" "flip" "gary" "hugo" "ida"))))
    (assert-equal 1260 (length res))))

(define-test test-length-sort
  (assert-equal 
    '((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))
    (nn:length-sort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))))

(define-test test-length-freq-sort
  (assert-equal
    '("ijkl" "o" "abc" "fgh" "de" "de" "mn")
    (nn:length-freq-sort '("abc" "de" "fgh" "de" "ijkl" "mn" "o"))))

(define-test test-is-prime
  (assert-true (nn:is-prime 47))
  (assert-true (nn:is-prime 2))
  (assert-false (nn:is-prime 1))
  (assert-false (nn:is-prime 49)))

(define-test test-gcd2
  (assert-equal 9 (nn:gcd2 36 63))
  (assert-equal 3 (nn:gcd2 -3 -6))
  (assert-equal 3 (nn:gcd2 -3 6)))

(define-test test-coprime
  (assert-true (nn:coprime 35 64))
  (assert-false (nn:coprime 21 49)))

(define-test test-totient-phi
  (assert-equal 1 (nn:totient-phi 1))
  (assert-equal 4 (nn:totient-phi 10)))

(define-test test-prime-factors
  (assert-equal '(3 3 5 7) (nn:prime-factors 315))
  (assert-equal '(2 47) (nn:prime-factors 94)))

(define-test test-prime-factors-mult
  (assert-equal '((3 2) (5 1) (7 1)) (nn:prime-factors-mult 315)))
