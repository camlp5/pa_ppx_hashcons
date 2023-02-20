# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),oUnit,fmt,pcre,rresult,compiler-libs.common,sexplib0,uint,pa_ppx.utils,pa_ppx.import
PACKAGES := $(PACKAGES),pa_ppx_hashcons,hashcons,pa_ppx_migrate
export IMPORT_OCAMLCFLAGS += -ppopt -pa_passthru-debug 
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-package -ppopt camlp5
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .

ALLSRC=$(shell ls *.ml)
ALLPACKAGES=$(PACKAGES)

TESTS= \
	test_hashcons.byte \
	test_hashcons2.byte \
	lamh.byte \
	test_inject.byte \

ML=\
	test_hashcons.ml test_hashcons2.ml lamh.ml lam.ml lam_hashcons.ml lam_migrate.ml test_inject.ml

all: $(TESTS) $(TESTS:.byte=.opt)

test: $(TESTS:.byte=)

% : %.opt
	mkdir -p _build && ./$<

lamh.byte: lamh.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

lamh.opt: lamh.cmx
	$(LAUNCH) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons.byte: test_hashcons.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons.opt: test_hashcons.cmx
	$(LAUNCH) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons2.byte: test_hashcons2.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons2.opt: test_hashcons2.cmx
	$(LAUNCH) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_inject.byte: lam.cmo lam_hashcons.cmo lam_migrate.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_inject.opt: lam.cmx lam_hashcons.cmx lam_migrate.cmx
	$(LAUNCH) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*
