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

(in-package :ninety-nine.arithmetic)


;; Problem 31 - Determine whether a given number is prime

; https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Common_Lisp
(defun sieve-of-eratosthenes (maximum)
  (loop
    with sieve = (make-array (1+ maximum)
                             :element-type 'bit
                             :initial-element 0)
    for candidate from 2 to maximum
    when (zerop (bit sieve candidate))
    collect candidate
    and do (loop for composite from (expt candidate 2) 
                 to maximum by candidate
                 do (setf (bit sieve composite) 1))))

(defun is-prime (n)
  (and (> n 1)
       (not (find-if #'(lambda (x) (zerop (mod n x)))
                     (sieve-of-eratosthenes (floor (sqrt n)))))))


;; Problem 32 - Determine the greatest common divisor of two positive integer numbers

(defun gcd2 (a b)
  "gcd is already an existing symbol"
  (if (zerop b) (abs a) (gcd2 b (mod a b))))


;; Problem 33 - Determine whether two positive integer numbers are coprime. Two numbers
;; are coprime if their greatest common divisor equals 1.

(defun coprime (a b)
  (= 1 (gcd2 a b)))


;; Problem 34 - Calculate Euler's Totient function phi(m)

(defun totient-phi (m)
  (if (= 1 m) 1
    (length (loop for r from 1 to m when (coprime r m) collect r))))


;; Problem 35 - Determine the prime factors of a given positive integer. Construct a flat
;; list containing the prime factors in ascending order.

(defun prime-factors (n)
  (labels ((factors (i primes acc)
                    (cond ((= 1 i) acc)
                          ((zerop (mod i (car primes)))
                           (factors (/ i (car primes)) primes (cons (car primes) acc)))
                          (t (factors i (cdr primes) acc)))))
    (factors n
             (reverse (sieve-of-eratosthenes (floor (/ n 2))))
             nil)))


;; Problem 36 - Determine the prime factors of a given positive integer with their
;; multiplicity.

(defun prime-factors-mult (n)
  (mapcar #'reverse (nn.lists:encode (prime-factors n))))


;; Problem 37 - Calculate Euler's Totient function phi (improved

(defun totient-phi-improved (m)
  (apply #'* (mapcar #'(lambda (factor)
                         (let ((p (first factor))
                               (m (second factor)))
                           (* (1- p) (expt p (1- m)))))
                     (prime-factors-mult m))))


;; Problem 39 - A list of prime numbers

(defun primes-range (a b)
  (nn.lists:drop-while #'(lambda (x) (< x a)) (sieve-of-eratosthenes b)))


;; Problem 40 - Goldbach's conjecture

(defun goldbach (n)
  (labels ((gold (candidates left right target)
                 (unless (>= left right)
                   (let* ((lvalue (aref candidates left))
                          (rvalue (aref candidates right))
                          (res (+ lvalue rvalue)))
                     (cond ((= res target) (list lvalue rvalue))
                           ((> target res) (gold candidates (1+ left) right target))
                           (t (gold candidates left (1- right) target)))))))
    (let* ((primes (sieve-of-eratosthenes n))
           (candidates (make-array (length primes) :initial-contents primes)))
      (gold candidates 0 (1- (length primes)) n))))


;; Problem 41 - Given a range of integers by its lower and upper limit, print a list of
;; all even numbers and their Goldbach composition.

(defun goldbach-list (a b &optional (p 1))
  (let* ((numbers (remove-if-not #'evenp (range a b)))
         (gb-list (mapcar #'goldbach numbers)))
    (remove-if-not #'(lambda (pair)
                       (and (not (null pair))
                            (and (> (first pair) p) (> (second pair) p))))
                   gb-list)))
