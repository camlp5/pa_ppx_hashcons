WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

all:

test:: mdx-test

mdx-test:: toplevel-error-handling.asciidoc.TEST shell-error-handling.asciidoc.TEST

%.asciidoc.TEST: %.asciidoc.corrected %.asciidoc
	cp $< $<.UNMODIFIED
	./clean-ocaml-mdx-output $< > $<.NEW && mv $<.NEW $<
	diff -Bwiu $^

shell-error-handling.asciidoc.TEST: | *.ml

clean::
	rm -f *.UNMODIFIED
