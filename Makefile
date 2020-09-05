.PHONY: all

all: scala lisp

.PHONY: scala
scala:
	@cd Scala && sbt test

.PHONY: lisp
lisp:
	@cd Lisp && $(MAKE) test
