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
(in-package :ninety-nine.tests.logicandcodes)

(define-test test-gray
  (assert-equal '((0) (1)) (nn.lac:gray 1))
  (assert-equal '((0 0) (0 1) (1 1) (1 0)) (nn.lac:gray 2))
  (assert-equal '((0 0 0)
                  (0 0 1)
                  (0 1 1)
                  (0 1 0)
                  (1 1 0)
                  (1 1 1)
                  (1 0 1)
                  (1 0 0))
                (nn.lac:gray 3)))

(define-test test-insert-by
  (assert-equal
    '((a 5) (b 6) (c 7))
    (nn.lac:insert-by '((a 5) (c 7)) '(b 6) #'< :key #'second))
  (assert-equal
    '((a 5) (b 6) (c 7))
    (nn.lac:insert-by '((b 6) (c 7)) '(a 5) #'< :key #'second))
  (assert-equal
    '((a 5))
    (nn.lac:insert-by nil '(a 5) #'< :key #'second))
  (assert-equal
    '((a 5) (b 6) (c 7))
    (nn.lac:insert-by '((a 5) (b 6)) '(c 7) #'< :key #'second)))

(define-test test-huffman
  (assert-equal
    '((#\a "0")
      (#\b "101")
      (#\c "100")
      (#\d "111")
      (#\e "1101")
      (#\f "1100"))
    (nn.lac:huffman '((#\a 45) (#\b 13) (#\c 12) (#\d 16) (#\e 9) (#\f 5)))))
