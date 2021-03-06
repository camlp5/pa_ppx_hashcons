# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),oUnit,fmt,pcre,rresult,compiler-libs.common,sexplib0,uint,pa_ppx.utils,pa_ppx.import
PACKAGES := $(PACKAGES),pa_ppx_hashcons,hashcons,pa_ppx_migrate
SYNTAX := camlp5o
OCAMLCFLAGS += -ppopt -pa_passthru-debug 
OCAMLCFLAGS += -ppopt -pa_import-package -ppopt camlp5
OCAMLCFLAGS += -ppopt -pa_import-I -ppopt .

TESTS= \
	test_hashcons.byte test_hashcons2.byte lamh.byte lamh.opt test_inject.byte \

ML=\
	test_hashcons.ml test_hashcons2.ml lamh.ml lam.ml lam_hashcons.ml lam_migrate.ml test_inject.ml


CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(TESTS)

test: $(TESTS:.byte=)

% : %.opt
	mkdir -p _build && ./$<

lamh.byte: lamh.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

lamh.opt: lamh.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons.byte: test_hashcons.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons.opt: test_hashcons.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons2.byte: test_hashcons2.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_hashcons2.opt: test_hashcons2.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_inject.byte: lam.cmo lam_hashcons.cmo lam_migrate.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_inject.opt: lam.cmx lam_hashcons.cmx lam_migrate.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o \
	*.ml \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend

.ml.cmo:
#	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -ppopt -sep -ppopt ";;\n" -package $(PACKAGES),camlp5.pr_o -syntax $(SYNTAX) $< > $@.ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<
