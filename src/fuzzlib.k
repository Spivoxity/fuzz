#
# fuzzlib.k
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

#
# Copy first arg file (fuzzlib.x), substituting symdef.i (cpp output
# for symdef.x) for SYMDEF and opdef.x for OPDEF.
#
# Call by: gawk -f fuzzlib.awk output=fuzzlib fuzzlib.x
#

/SYMDEF/ {
    while (getline <"symdef.i") {
        if ($0 !~ /^"/) continue;
        val = $2;
	if (val ~ /'.'/) val = char_val(val);
	printf("%%%%token $%s %d\n", $1, val);
    }
    next;
}

/OPDEF/ {
    while (getline <"opdef.x")
	if (NF > 0 && $1 != "%") print;
    next;
}

/^% File:/ { printf("%% File: %s\n", output); next; }

{ print; }

func char_val(c) {
    for (cc = 0; cc < 128; cc++)
	if (c == sprintf("'%c'", cc))
	    return cc;
    return 999;
}
