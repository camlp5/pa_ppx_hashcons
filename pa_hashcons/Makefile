# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx.deriving,pa_ppx.deriving_plugins.show,pa_ppx.base,pa_ppx.unmatched_vala,pa_ppx.deriving_plugins.params
TARGET_PACKAGE=pa_ppx_hashcons

ML=\
	pa_deriving_hashcons.ml \

TOOLSML = mk_meta.ml
allsrc=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX) local-install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META mk_meta mk_meta.ml
