/*
 * symdef.x
 *
 * This file is part of fuzz2000
 * Copyright (c) 1982--2006 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * $Id: symdef.x,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "symbol.h"

"\bind" BIND
"\bsup" BSUP
"\cross" CROSS
"\defs" DEFEQ
"\ELSE" ELSE
"\esup" ESUP
"\exists" EXISTS
"\exists_1" EXISTS1
"false" FALSITY
"\forall" FORALL
"\hide" HIDE
"\iff" EQUIV
"\IF" IF
"\implies" IMPLIES
"\in" IN
"\inrel" YINREL
"\lambda" LAMBDA
"\land" AND
"\langle" LANGLE
"\lbag" LBAG
"\ldata" LDATA
"\LET" LET
"\limg" LIMG
"\lnot" NOT
"\lor" OR
"\mid" '|'
"\mu" MU
"\pipe" PIPE
"\power" POWER
"\pre" PRE
"\project" PROJECT
"\rangle" RANGLE
"\rbag" RBAG
"\rdata" RDATA
"\rimg" RIMG
"\semi" FATSEMI
"\spot" '@'
"\THEN" THEN
"\theta" THETA
"true" TRUTH
"\vdash" VDASH
"\where" WHERE
"==" EQEQ
"%%ingen" XINGEN
"%%inop" XINOP
"%%inrel" XINREL
"%%pregen" XPREGEN
"%%prerel" XPREREL
"%%postop" XPOSTOP
"%%type" XABBREV
"%%tame" XTAME
"%%pname" XPNAME
"%%ignore" XIGNORE
"%%debug" XDEBUG
"%%line" XLINE
"\begin{axdef}" BAXDEF
"\begin{gendef}" BGENDEF
"\begin{schema}" BSCHEMA
"\begin{zed}" BZED
"\begin{syntax}" BSYNTAX
"\end{axdef}" EAXDEF
"\end{gendef}" EGENDEF
"\end{schema}" ESCHEMA
"\end{zed}" EZED
"\end{syntax}" ESYNTAX
