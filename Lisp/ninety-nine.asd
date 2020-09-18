(asdf:defsystem :ninety-nine
  :description "99 Lisp Problems"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:uiop)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "problems")
               (:file "utils")))

(asdf:defsystem :ninety-nine/tests
  :description "99 Unit Tests"
  :author "Pedro Rodrigues <csixteen@protonmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:lisp-unit :ninety-nine)
  :pathname "t/"
  :serial t
  :components ((:file "package")
               (:file "problems-tests")))
