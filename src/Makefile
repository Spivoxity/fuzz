#
# src/Makefile
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
# $Id: Makefile,v 1.2 2007-02-16 12:45:13 mike Exp $
#

all: fuzz fuzzlib

FUZZ = zparse.o zscan.o param.o main.o spec.o type.o frame.o dict.o \
	sched.o pretty.o error.o expr.o alloc.o unify.o schema.o dump.o
GEN = y.output zparse.c symbol.h absyn.h zscan.c

LEX = flex
LFLAGS = -s
CC=gcc
CPP=/lib/cpp
DEFINES = -DDEBUG -DANSI -DASSUME

fuzz: $(FUZZ)
	$(CC) $(CFLAGS) -o fuzz $(FUZZ)

clean:
	rm -f *.o $(GEN)

distclean: clean
	rm -f fuzz fuzzlib minilib

realclean: distclean

zparse.c : zparse.y
	bison -dv -y zparse.y
	mv y.tab.h symbol.h
	mv y.tab.c zparse.c

symbol.h : zparse.c

absyn.h: absyn.x absyn.k
	gawk -f absyn.k absyn.x >absyn.h

fuzzlib: symdef.x fuzzlib.k fuzzlib.x minilib.x opdef.x symbol.h
	$(CPP) $(DEFINES) symdef.x >symdef.i
	gawk -f fuzzlib.k output=fuzzlib fuzzlib.x >fuzzlib
	gawk -f fuzzlib.k output=minilib minilib.x >minilib
	rm symdef.i

$(FUZZ): fuzz.h proto.h symbol.h absyn.h

%.o : %.c
	$(CC) $(CFLAGS) -Wall -c $(DEFINES) $<

.DELETE_ON_ERROR:

force:

