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

(asdf:defsystem :ninety-nine/arithmetic
  :description "99 Lisp Problems (Arithmetic)"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop :ninety-nine/lists)
  :pathname "src/arithmetic/"
  :serial t
  :components ((:file "package")
               (:file "problems")))

(asdf:defsystem :ninety-nine/lists
  :description "99 Lisp Problems (Lists)"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop)
  :pathname "src/lists/"
  :serial t
  :components ((:file "package")
               (:file "problems")
               (:file "utils")))

(asdf:defsystem :ninety-nine/logicandcodes
  :description "99 Lisp Problems (Logic and Codes)"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop)
  :pathname "src/logicandcodes/"
  :serial t
  :components ((:file "package")
               (:file "problems")
               (:file "utils")))

(asdf:defsystem :ninety-nine/tests/arithmetic
  :description "99 Unit Tests (Arithmetic)"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:lisp-unit :ninety-nine/arithmetic)
  :pathname "t/arithmetic/"
  :serial t
  :components ((:file "package")
               (:file "problems-tests")))

(asdf:defsystem :ninety-nine/tests/lists
  :description "99 Unit Tests (Lists)"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:lisp-unit :ninety-nine/lists)
  :pathname "t/lists/"
  :serial t
  :components ((:file "package")
               (:file "problems-tests")))

(asdf:defsystem :ninety-nine/tests/logicandcodes
  :description "99 Unit Tests (Logic and Codes)"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:lisp-unit :ninety-nine/logicandcodes)
  :pathname "t/logicandcodes/"
  :serial t
  :components ((:file "package")
               (:file "problems-tests")))
