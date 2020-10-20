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

(in-package :ninety-nine.logicandcodes)


;; Problem 49 - Gray codes

(defun gray (n)
  (labels ((rec (i acc)
                (cond ((= i 1) acc)
                      (t (rec
                           (1- i)
                           (append
                             (mapcar #'(lambda (x) (cons 0 x)) acc)
                             (mapcar #'(lambda (x) (cons 1 x)) (reverse acc))))))))
    (rec n '((0) (1)))))


;; Problem 50 - Huffman codes

(defun huffman (freqs)
  "Takes a list of pairs (Char, Frequency) and returns a list
  of pairs (Char, Huffman Code)"
  (let* ((leafs (mapcar #'(lambda (p) (list (second p) (make-htree :elem (first p))))
                        (sort freqs #'< :key #'second)))
         (tree (build-htree leafs)))
    (sort (serialize tree) #'char< :key #'first)))

(defun build-htree (leafs)
  "Takes a list of pairs (Frequency, Huffman Tree Leaf) and returns
  a Huffman Tree"
  (cond ((= 1 (length leafs))
         (cadar leafs))
        (t (destructuring-bind ((f1 t1) (f2 t2) . xs) leafs
             (build-htree
               (insert-by xs
                          (list (+ f1 f2) (make-htree :left t1 :right t2))
                          #'<
                          :key #'car))))))

(defun serialize (tree)
  "Takes a Huffman Tree and returns a list of pairs (Char, Huffman Code)"
  (cond ((not (null (htree-elem tree))) ;; It's a leaf
         (list (list (htree-elem tree) "")))
        (t (append
             (mapcar #'(lambda (pair) (list (first pair)
                                            (concatenate 'string "0" (second pair))))
                     (serialize (htree-left tree)))
             (mapcar #'(lambda (pair) (list (first pair)
                                            (concatenate 'string "1" (second pair))))
                     (serialize (htree-right tree)))))))
