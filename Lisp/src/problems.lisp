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

(in-package :ninety-nine)


;;;--------------------------------------------
;;; Lists


;; Problem 1 - find the last element of a list

(defun my-last (lst)
  (when lst
    (cond ((null (cdr lst)) (car lst))
          (t (my-last (cdr lst))))))


(defun my-last2 (lst)
  (when lst
    (reduce #'(lambda (x y) y) lst :initial-value nil)))


(defun my-last3 (lst)
  (when lst (car (reverse lst))))


(defun my-last4 (lst)
  (let ((len (length lst)))
    (nth (1- len) lst)))


;; Problem 2 - find the but last element in a list

(defun but-last (lst)
  (if (< (length lst) 2)
    (error "List must have at least 2 elements")
    (cadr (reverse lst))))


;; Problem 3 - find the kth element of a list

(defun element-at (lst n)
  (cond ((null lst) (error "Not enough elements"))
        ((= n 1) (car lst))
        (t (element-at (cdr lst) (1- n)))))


;; Problem 4 - find the number of elements in a list

(defun my-length (lst)
  (reduce #'(lambda (x y) (1+ x)) lst :initial-value 0))


;; Problem 5 - reverse a list

(defun my-reverse (lst)
  (labels ((rev (xs acc)
                (cond ((null xs) acc)
                      (t (rev (cdr xs) (cons (car xs) acc))))))
    (rev lst nil)))

(defun flip (f)
  (lambda (x y) (funcall f y x)))

(defun my-reverse2 (lst)
  (reduce (flip #'cons) lst :initial-value nil))


;; Problem 6 - find out whether a list is a palindrome

(defun palindrome? (lst)
  (equal lst (reverse lst)))


;; Problem 7 - flatten a nested list structure

(defun flatten (lst)
  (cond ((null lst) nil)
        ((listp (car lst))
         (append (flatten (car lst)) (flatten (cdr lst))))
        (t (cons (car lst) (flatten (cdr lst))))))


;; Problem 8 - remove consecutive duplicates from a list

(defun compress (lst)
  (cond ((< (length lst) 2) lst)
        ((eql (first lst) (second lst))
         (compress (cdr lst)))
        (t (cons (first lst) (compress (cdr lst))))))


;; Problem 9 - pack consecutive duplicates into sublists

(defun pack (lst)
  (reduce #'(lambda (x acc)
              (cond ((null acc) (list (list x)))
                    (t (destructuring-bind (y . ys) acc
                         (cond ((eql x (car y)) (cons (cons x y) ys))
                               (t (cons (list x) (cons y ys))))))))
          lst
          :initial-value nil
          :from-end t))


;; Problem 10 - Run-length encoding of a list

(defun encode (lst)
  (mapcar #'(lambda (g) (list (length g) (car g))) 
          (pack lst)))
