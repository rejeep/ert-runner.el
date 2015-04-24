EMACS ?= emacs
CASK ?= cask
PKG_DIR := $(shell cd features/project ; ${CASK} package-directory)

all: test

test: clean-elc
	${MAKE} ecukes
	${MAKE} compile
	${MAKE} ecukes
	${MAKE} clean-elc

ecukes: elpa
	${CASK} exec ecukes --script features

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ert-runner.el

clean-elc:
	rm -f ert-runner.elc

elpa: ${PKG_DIR}
${PKG_DIR}:
	$(shell cd features/project ; ${CASK} install)
	touch $@

.PHONY:	all test compile clean-elc ecukes
