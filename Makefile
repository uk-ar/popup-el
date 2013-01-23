EMACS=emacs
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

travis-ci:
	${EMACS} --version
	${EMACS} -batch -Q -l tests/run-test.el

ecukes:
	carton exec ${ECUKES} features
