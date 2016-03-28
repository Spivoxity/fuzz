#
# doc/Makefile
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
# $Id: Makefile,v 1.5 2007-02-16 12:45:13 mike Exp $
#

all: fuzzman.pdf fuzzman-2up.pdf refcard.pdf refcard-2up.pdf refcard-3up.pdf

clean:
	rm -f *.aux *.dvi *.ps *.log *.toc *.idx index.tex
	rm -f *.o mkindex

distclean: clean

realclean: distclean
	rm -f *.pdf

### TeX -> DVI

fuzzman.dvi: fuzzman.tex mkindex
	if [ ! -r index.tex ]; then echo '\relax' >index.tex; fi
	latex \\nonstopmode \\input $<
	mkindex fuzzman.idx index.tex
	latex \\nonstopmode \\input $<
	while grep -s Rerun $*.log; do latex \\nonstopmode \\input $<; done

%.dvi: %.tex a5man.sty
	latex \\nonstopmode \\input $<
	while grep -s Rerun $*.log; do latex \\nonstopmode \\input $<; done

export TEXINPUTS = ../tex:

example.dvi: example.tex
	latex2e \\nonstopmode \\input $<
	while grep -s Rerun $*.log; do latex2e \\nonstopmode \\input $<; done

tut.dvi: tut.tex
	latex2e \\nonstopmode \\input $<
	while grep -s Rerun $*.log; do latex2e \\nonstopmode \\input $<; done


### DVI -> DVI-2up

fuzzman-2up.dvi: blank.dvi fuzzman.dvi
	# Adjust so odd-numbered pages fall on a recto
	dviconcat blank.dvi fuzzman.dvi >tmp.dvi
	dvitodvi '2:0,1(149mm,0)' tmp.dvi >$@
	@echo
	@rm tmp.dvi

refcard-2up.dvi: refcard.dvi
	dvitodvi '2:0,1(149mm,0)' $< >$@
	@echo

refcard-3up.dvi: refcard3.dvi
	dvitodvi '6:4+5(99mm,0)+0(198mm,0),1+2(99mm,0)+3(198mm,0)' $< >$@

### DVI -> PS

PSFLAGS = -Psizes -Ppdf -O -5mm,-10mm -D 600

%-2up.ps: %-2up.dvi
	dvips $(PSFLAGS) -t a4quer $< -o $@

%-3up.ps: %-3up.dvi
	dvips $(PSFLAGS) -t a4quer -O -20mm,-15mm $< -o $@

%.ps: %.dvi
	dvips $(PSFLAGS) -t a5 $< -o $@

### PS -> PDF

%.pdf: %.ps
	ps2pdf $<

###

mkindex: index.o common.o
	$(CC) $^ -o $@

index.o common.o: common.h

CC = gcc
