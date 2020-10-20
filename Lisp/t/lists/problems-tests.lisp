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
(in-package :ninety-nine.tests.lists)


(define-test test-my-last
  (assert-equal 1 (nn.lists:my-last '(5 4 3 2 1))))

(define-test test-but-last
  (assert-equal 2 (nn.lists:but-last '(5 4 3 2 1))))

(define-test test-element-at
  (assert-error 'error (nn.lists:element-at '() 3))
  (assert-equal 2 (nn.lists:element-at '(5 4 3 2 1) 4)))

(define-test test-my-length
  (assert-equal 0 (nn.lists:my-length '()))
  (assert-equal 5 (nn.lists:my-length '(1 2 3 4 5))))

(define-test test-my-reverse
  (assert-equal nil (nn.lists:my-reverse nil))
  (assert-equal '(5 4 3 2 1) (nn.lists:my-reverse '(1 2 3 4 5))))

(define-test test-palindrome?
  (assert-true (nn.lists:palindrome? nil))
  (assert-true (nn.lists:palindrome? '(1 2 3 2 1)))
  (assert-false (nn.lists:palindrome? '(1 2 3 4 5))))

(define-test test-flatten
  (assert-equal nil (nn.lists:flatten nil))
  (assert-equal '(1 2 3 4 5) (nn.lists:flatten '(1 2 3 4 5)))
  (assert-equal '(1 2 3 4 5) (nn.lists:flatten '((1 (2 3)) 4 (5)))))

(define-test test-compress
  (assert-equal nil (nn.lists:compress nil))
  (assert-equal '(1 2 3) (nn.lists:compress '(1 2 3)))
  (assert-equal '(1 2 3) (nn.lists:compress '(1 1 1 2 2 3 3 3 3))))

(define-test test-pack
  (assert-equal nil (nn.lists:pack nil))
  (assert-equal '((1) (2) (3)) (nn.lists:pack '(1 2 3)))
  (assert-equal '((1 1 1 1) (2) (3 3)) (nn.lists:pack '(1 1 1 1 2 3 3))))

(define-test test-encode
  (assert-equal nil (nn.lists:encode nil))
  (assert-equal '((1 1) (1 2) (1 3)) (nn.lists:encode '(1 2 3)))
  (assert-equal '((4 1) (1 2) (2 3)) (nn.lists:encode '(1 1 1 1 2 3 3))))

(define-test test-encode-modified
  (assert-equal nil (nn.lists:encode-modified nil))
  (assert-equal '(1 2 3) (nn.lists:encode-modified '(1 2 3)))
  (assert-equal '((4 1) 2 (2 3)) (nn.lists:encode-modified '(1 1 1 1 2 3 3))))

(define-test test-decode-modified
  (assert-equal nil (nn.lists:decode-modified nil))
  (assert-equal '(1 2 3) (nn.lists:decode-modified '(1 2 3)))
  (assert-equal '(1 1 1 1 2 3 3) (nn.lists:decode-modified '((4 1) 2 (2 3)))))

(define-test test-duplicate
  (assert-equal nil (nn.lists:duplicate nil))
  (assert-equal '(1 1 2 2 3 3) (nn.lists:duplicate '(1 2 3))))

(define-test test-replicate
  (assert-equal nil (nn.lists:replicate nil 2))
  (assert-equal '(1 2 3) (nn.lists:replicate '(1 2 3) 1))
  (assert-equal '(1 1 1 2 2 2 3 3 3) (nn.lists:replicate '(1 2 3) 3)))

(define-test test-drop-every
  (assert-equal nil (nn.lists:drop-every nil 3))
  (assert-equal '(1 2) (nn.lists:drop-every '(1 2) 3))
  (assert-equal '(1 2 4 5 7) (nn.lists:drop-every '(1 2 3 4 5 6 7) 3)))

(define-test test-split-at
  (multiple-value-bind (left right) (nn.lists:split-at nil 3)
    (assert-nil left)
    (assert-nil right))
  (multiple-value-bind (left right) (nn.lists:split-at '(1) 3)
    (assert-equal left '(1))
    (assert-nil right))
  (multiple-value-bind (left right) (nn.lists:split-at '(1 2 3 4 5 6) 3)
    (assert-equal left '(1 2 3))
    (assert-equal right '(4 5 6))))

(define-test test-slice
  (assert-error 'error (nn.lists:slice '(1 2 3 4 5 6) 0 3))
  (assert-error 'error (nn.lists:slice '(1 2 3 4 5 6) 3 1))
  (assert-error 'error (nn.lists:slice '(1 2 3) 1 4))
  (assert-equal '(1 2 3) (nn.lists:slice '(1 2 3) 1 3))
  (assert-equal '(3 4 5) (nn.lists:slice '(1 2 3 4 5 6 7) 3 5)))

(define-test test-rotate
  (assert-nil (nn.lists:rotate nil 3))
  (assert-equal '(1 2 3) (nn.lists:rotate '(1 2 3) 0))
  (assert-equal '(4 5 1 2 3) (nn.lists:rotate '(1 2 3 4 5) 3)))

(define-test test-remove-at
  (assert-error 'error (nn.lists:remove-at '() 2))
  (assert-error 'error (nn.lists:remove-at '(1 2 3) 4))
  (multiple-value-bind (elem residue) (nn.lists:remove-at '(1 2 3 4) 2)
    (assert-equal '(1 3 4) residue)
    (assert-equal 2 elem)))

(define-test test-insert-at
  (assert-equal '(1) (nn.lists:insert-at nil 1 1))
  (assert-equal '(1 5 2 3 4) (nn.lists:insert-at '(1 2 3 4) 5 2)))

(define-test test-range
  (assert-equal nil (nn.lists:range 5 3))
  (assert-equal '(4) (nn.lists:range 4 4))
  (assert-equal '(4 5 6 7 8 9) (nn.lists:range 4 9)))

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
         (res (nn.lists:rnd-permutation lst)))
    (assert-false (equal lst res))
    (assert-equal (sort res #'char<) lst)))

(define-test test-combinations
  (let* ((lst '(#\a #\b #\c #\d))
         (res (nn.lists:combinations 2 lst)))
    (assert-equal res
                  '((#\a #\b)
                    (#\a #\c)
                    (#\a #\d)
                    (#\b #\c)
                    (#\b #\d)
                    (#\c #\d)))))

(define-test test-comb-modified
  (let* ((lst '(#\a #\b #\c #\d))
         (res (nn.lists:comb-modified 2 lst)))
    (assert-equal 6 (length res))))

(define-test test-group
  (let ((res (group '(2 3 4)
                    '("aldo" "beat" "carla" "david" "evi" "flip" "gary" "hugo" "ida"))))
    (assert-equal 1260 (length res))))

(define-test test-length-sort
  (assert-equal 
    '((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))
    (nn.lists:length-sort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))))

(define-test test-length-freq-sort
  (assert-equal
    '("ijkl" "o" "abc" "fgh" "de" "de" "mn")
    (nn.lists:length-freq-sort '("abc" "de" "fgh" "de" "ijkl" "mn" "o"))))
