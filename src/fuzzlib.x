%
% fuzzlib.x
%
% This file is part of fuzz2000
% Copyright (c) 1982--2006 J. M. Spivey
% All rights reserved
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% 1. Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% 3. The name of the author may not be used to endorse or promote products
%    derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
% OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% $Id: fuzzlib.x,v 1.2 2007-02-16 12:45:13 mike Exp $
%

SYMDEF -- Token definitions for the lexer are put here

OPDEF -- Operator definitions are put here

%%type \pfun \nat \seq \bag
\begin{gendef}[X]
	\finset \_: \power (\power X)
\end{gendef}
\begin{gendef}[X,Y]
	\_ \rel \_, \_ \ffun \_: \power (\power (X \cross Y)) \\
	\_ \fun \_: \power (X \pfun Y)
\end{gendef}
%%type \rel \ffun \finset

\begin{axdef}
	-: \num \fun \num \\
        \_ + \_ , \_ - \_ , \_ * \_: \num \cross \num \fun \num \\
        \_ \div \_ , \_ \mod \_: 
		\num \cross \num \pfun \num \\
        \_ < \_ , \_ \leq \_ , \_ \geq \_ , \_ > \_: 
		\num \rel \num \\
	\nat_1: \power \nat \\
        succ: \nat \fun \nat \\
	\_ \upto \_: \num \cross \num \fun \power \num \\
        min: \power \num \pfun \num \\
        max: \power \num \pfun \num
\end{axdef}

\begin{gendef}[X]
        \_ \neq \_: X \rel X \\
        \_ \notin \_: X \rel \power X \\
	\empty, \emptyset: \power X \\
	\_ \subseteq \_ , \_ \subset \_: \power X \rel \power X \\
	\power_1 \_: \power (\power X) \\
        \_  \cup \_ , \_ \cap \_ , \_ \setminus \_:
                \power X \cross \power X \fun \power X \\
        \bigcup, \bigcap: \power (\power X) \fun \power X \\
	\id \_: X \rel X \\
        \_\plus, \_\star: (X \rel X) \fun (X \rel X) \\
        iter: \num \fun (X \rel X) \fun (X \rel X) \\
	\finset_1 \_: \power (\finset X) \\
        \#: \finset X \fun \nat \\
	\seq_1 \_, \iseq \_: \power (\seq X) \\
        \_ \cat \_ :  \seq X \cross \seq X \fun \seq X \\
        head, last: \seq X \pfun X \\
        tail, front: \seq X \pfun \seq X \\
        rev: \seq X \fun \seq X \\
        \_ \filter \_: \seq X \cross \power X \fun \seq X \\
        \_ \extract \_: \power \nat \cross \seq X \pfun \seq X \\
	squash: (\nat \ffun X) \pfun \seq X \\
	\_\prefix\_, \_\suffix\_, \_\inseq\_: \seq X \rel \seq X \\
        \dcat: \seq (\seq X) \fun \seq X \\
        count: \bag X \fun (X \fun \nat) \\
	\_\bcount\_: \bag X \cross X \fun \nat \\
        \_ \inbag \_: X \rel \bag X \\
	\_ \subbageq \_: \bag X \rel \bag X \\
        \_ \uplus \_, \_ \uminus \_: \bag X \cross \bag X \fun \bag X \\
	\_ \otimes \_: \nat \cross \bag X \fun \bag X \\
        items: \seq X \fun \bag X
\end{gendef}

\begin{gendef}[X,Y]
        first: X \cross Y \fun X \\
        second: X \cross Y \fun Y \\
        \_ \mapsto \_: X \cross Y \fun X \cross Y \\
        \dom: (X \rel Y) \fun \power X \\
        \ran: (X \rel Y) \fun \power Y \\
        \_ \dres \_: \power X \cross (X \rel Y) \fun (X \rel Y) \\
        \_ \rres \_: (X \rel Y) \cross \power Y \fun (X \rel Y) \\
        \_ \ndres \_: \power X \cross (X \rel Y) \fun (X \rel Y) \\
        \_ \nrres \_: (X \rel Y) \cross \power Y \fun (X \rel Y) \\
        \_\inv: (X \rel Y) \fun (Y \rel X) \\
        \_\limg\_\rimg: (X \rel Y) \cross \power X \fun \power Y \\
        \_ \oplus \_: (X \rel Y) \cross (X \rel Y) \fun (X \rel Y) \\
	\_ \pinj \_, \_ \inj \_, \_ \psurj \_, 
	\_ \surj \_, \_ \bij \_, \_ \finj \_: \power(X \pfun Y) \\
        \disjoint \_: \power (X \pfun \power Y) \\
        \_ \partition \_: (X \pfun \power Y) \rel \power Y
\end{gendef}

\begin{gendef}[X,Y,Z]
        \_ \comp \_: (X \rel Y) \cross (Y \rel Z) \fun (X \rel Z) \\
	\_ \circ \_: (Y \rel Z) \cross (X \rel Y) \fun (X \rel Z)
\end{gendef}

%%pname \pfun $"-+>"
%%pname \ffun $"-++>"
%%pname \rel $"<->"
%%pname \seq $"seq"
%%pname \bag $"bag"
%%pname \finset $"F"
%%pname \nat $"NN"
%%pname \num $"ZZ"
%%pname \upto $".."
%%pname \# $"#"
%%pname \power_1 $"P_1"
%%pname \id $"id"
%%pname \mapsto $"|->"
%%pname \dom $"dom"
%%pname \ran $"ran"
%%pname \fun $"-->"
%%pname \pinj $">+>"
%%pname \inj $">->"
%%pname \psurj $"-+>>"
%%pname \surj $"-->>"
%%pname \bij $">->>"
%%pname \finj $">++>"
%%pname \disjoint $"disjoint"
%%pname \partition $"partition"

%%tame \cup \cap \setminus \bigcup \bigcap count \uplus items
%%tame \plus \cat head last tail front rev \filter \dcat
%%tame first second \mapsto \dom \ran \dres \rres \ndres \nrres 
%%tame \inv \limg\rimg \oplus \comp \circ
