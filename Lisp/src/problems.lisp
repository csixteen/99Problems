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
;;;
;;;                  Lists


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


;; Problem 11 - Run-length encoding modified

(defun encode-modified (lst)
  (mapcar #'(lambda (g)
              (let ((len (length g)))
                (if (= len 1) (car g) (list len (car g)))))
          (pack lst)))


;; Problem 12 - Decode modified run-length

(defun decode-modified (lst)
  (mapcan #'(lambda (e)
              (if (listp e)
                (repeat (first e) (second e))
                (list e)))
          lst))


;; Problem 13 - Pretty much the same as problem 11


;; Problem 14 - Duplicate the elements of a list

(defun duplicate (lst)
  (mapcan #'(lambda (x) (list x x)) lst))


;; Problem 15 - Replicate the elements of a list a given
;; number of times

(defun replicate (lst n)
  (mapcan #'(lambda (x) (repeat n x)) lst))


;; Problem 16 - Drop every N elements of a list

(defun drop-every (lst n)
  (when lst
    (append (take lst (1- n))
            (drop-every (drop lst n) n))))


;; Problem 17 - Split a list into two parts

(defun split-at (lst n)
  (values (take lst n) (drop lst n)))


;; Problem 18 - Extract a slice from a list

(defun slice (lst start end)
  (if (or (< start 1) (< end start) (> end (length lst)))
    (error "Invalid values for `start` and `end`")
    (take (drop lst (1- start)) (1+ (- end start)))))


;; Problem 19 - Rotate a list N places to the left

(defun rotate (lst n)
  (unless (null lst)
    (multiple-value-bind (head tail) (split-at lst (mod n (length lst)))
      (append tail head))))


;; Problem 20 - Remove the kth element from a list

(defun remove-at (lst n)
  (if (or (< n 1) (> n (length lst)))
    (error "N is out of bounds")
    (let ((elem (nth (1- n) lst)))
      (values elem
              (append (take lst (1- n))
                      (drop lst n))))))


;; Problem 21 - insert an element at a given position in a list

(defun insert-at (lst e n)
  (append (take lst (1- n))
          (list e)
          (drop lst (1- n))))


;; Problem 22 - Create a list containing all integers within a given range

(defun range (start end)
  (unless (> start end)
    (cons start (range (1+ start) end))))


;; Problem 23 - Extract a given number of randomly selected elements from a list

(defun rnd-select (lst n)
  (unless (or (zerop n) (null lst))
    (let ((pos (1+ (random (length lst)))))
      (multiple-value-bind (elem res) (remove-at lst pos)
        (cons elem (rnd-select res (1- n)))))))


;; Problem 24 - Draw N different random numbers from the set 1..M

(defun diff-select (n m)
  (rnd-select (range 1 m) n))


;; Problem 25 - Generate a random permutation of the elements of a list

(defun rnd-permutation (lst)
  (rnd-select lst (length lst)))


;; Problem 26 - Generate the combinations of K distinct objects chosen from
;; the N elements of a list

(defun combinations (k lst)
  (cond ((zerop k) (list nil))
        ((null lst) nil)
        (t (destructuring-bind (head . tail) lst
             (let ((with-head (mapcar #'(lambda (l) (cons head l))
                                      (combinations (1- k) tail)))
                   (without-head (combinations k tail)))
               (append with-head without-head))))))


;; Problem 27 - Group the elements of a set into disjoint subsets

(defun comb-modified (k lst)
  (cond ((zerop k) (list (list nil lst)))
        ((null lst) nil)
        (t (let* ((x (first lst))
                  (xs (cdr lst))
                  (with-head (mapcar #'(lambda (pair)
                                         (list (cons x (first pair)) (second pair)))
                                     (comb-modified (1- k) xs)))
                  (without-head (mapcar #'(lambda (pair)
                                            (list (first pair) (cons x (second pair))))
                                        (comb-modified k xs))))
             (append with-head without-head)))))

(defun group (ks lst)
  (cond ((null ks) (list nil))
        (t (destructuring-bind (n . ns) ks
             (let ((pairs (comb-modified n lst)))
               (mapcan #'(lambda (pair)
                           (mapcar #'(lambda (gs) (cons (first pair) gs))
                                   (group ns (second pair))))
                       pairs))))))


;; Problem 28

;;; a-) Sort a list of lists according to the length of the sublists

(defun length-sort (lst)
  (sort lst #'(lambda (a b) (< (length a) (length b)))))

;;; b-) Sort a list of lists according to the frequency of the length of the sublists

(defun length-freq-sort (lst)
  (let* ((lengths (group-by lst #'length))
         (freqs (group-by (get-hash-table-values lengths) #'length))
         (freq-values (get-hash-table-values freqs)))
    (flatten (mapcar #'(lambda (e) (sort (mapcan #'identity e) #'string<))
            (sort freq-values #'list-length>)))))


;;;--------------------------------------------
;;;
;;;                Arithmetic


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
  (mapcar #'reverse (encode (prime-factors n))))


;; Problem 37 - Calculate Euler's Totient function phi (improved

(defun totient-phi-improved (m)
  (apply #'* (mapcar #'(lambda (factor)
                         (let ((p (first factor))
                               (m (second factor)))
                           (* (1- p) (expt p (1- m)))))
                     (prime-factors-mult m))))


;; Problem 39 - A list of prime numbers

(defun primes-range (a b)
  (drop-while #'(lambda (x) (< x a)) (sieve-of-eratosthenes b)))


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
