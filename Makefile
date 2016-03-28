#
# root/Makefile
#
# This file is part of fuzz2000
# Copyright (c) 1982--2006 J. M. Spivey
# All rights reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# $Id: Makefile,v 1.3 2007-02-16 12:45:13 mike Exp $
#

# Edit these directories to suit your setup
BINDIR = /usr/local/bin
LIBDIR = /usr/local/lib
TEXDIR = /usr/lib/tex/texmf/tex
MFDIR = /usr/lib/tex/texmf/fonts/source/local

INSTALL = install

all: src

test: force
	$(MAKE) -C test test

install: src
	$(INSTALL) -m 755 src/fuzz $(BINDIR)
	$(INSTALL) -m 644 src/fuzzlib $(LIBDIR)
	$(INSTALL) -m 644 tex/fuzz.sty $(TEXDIR)
	$(INSTALL) -m 644 tex/*.mf $(MFDIR)

SUBDIRS = src doc

$(SUBDIRS): force
	$(MAKE) -C $@ all

clean distclean realclean: force
	for d in $(SUBDIRS); do make -C $$d $@; done

force:
