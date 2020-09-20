(in-package :ninety-nine)

(defun flip (f)
  "Flips the parameters of a 2 argument function"
  (lambda (x y) (funcall f y x)))


(defun repeat (n elem)
  "Returns a list with `elem` repeated `n` times"
  (loop for i from 1 to n collect elem))


(defun take (lst n)
  "Collects the first `n` elements of a list"
  (cond ((or (null lst) (zerop n)) nil)
        (t (cons (car lst) (take (cdr lst) (1- n))))))


(defun take-while (p lst)
  "Collects elements from `lst` while the predicate `p`
  is true"
  (cond ((or (null lst) (not (funcall p (car lst)))) nil)
        (t (cons (car lst) (take-while p (cdr lst))))))


(defun drop (lst n)
  "Drops the first `n` elements of a list"
  (nthcdr n lst))


(defun drop-while (p lst)
  "Drops elements from `lst` while the predicate `p`
  is true"
  (cond ((or (null lst) (not (funcall p (car lst)))) lst)
        (t (drop-while p (cdr lst)))))
