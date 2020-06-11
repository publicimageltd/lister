.POSIX:
EMACS = emacs

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean

all: compile

compile: ${SRCS} ${OBJS}

clean:
	rm -f ${OBJS}

test:
	$(EMACS) -batch -f package-initialize -L . -f buttercup-run-discover

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
