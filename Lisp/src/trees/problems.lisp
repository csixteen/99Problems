; MIT License
; 
; Copyright (c) 2020 Pedro Rodrigues
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

(in-package :ninety-nine.trees)


;; Problem 54A - Check whether a given term represents a binary tree

(defun is-tree (tree)
  (cond ((null tree) t)
        ((or (not (listp tree)) (not (= 3 (length tree)))) nil)
        (t (and (is-tree (second tree))
                (is-tree (third tree))))))


;; Problem 55 - Construct completely balanced binary trees

(defun cbal-tree (n)
  (if (zerop n)
    '(nil)
    (let ((q (floor (/ (1- n) 2)))
          (r (mod (1- n) 2)))
      (mapcan #'(lambda (i)
                  (mapcan #'(lambda (left)
                              (mapcar #'(lambda (right) (list 1 left right))
                                      (cbal-tree (1- (- n i)))))
                          (cbal-tree i)))
              (range q (+ q r))))))


;; Problem 56 - Symmetric binary trees

(defun mirror (a b)
  (cond ((and (null a) (null b)) t)
        ((or (null a) (null b)) nil)
        (t (and (mirror (second a) (third b))
                (mirror (third a) (second b))))))

(defun symmetric (tree)
  (or (null tree)
      (mirror (second tree) (third tree))))
