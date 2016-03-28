#
# absyn.x
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
# $Id: absyn.x,v 1.2 2007-02-16 12:45:13 mike Exp $
#

> para	::=	GIVEN [ident] | AXDEF schema | SDEF shead schema
>		| DEFEQ shead sexp | PRED pred
>		| DEFINE [ident] schema | EQEQ lhs expr
>		| DATA ident [arm]
>		| XTAME [word] | XABBREV [word]
: GIVEN		given
: AXDEF		axdef
: SDEF		heading schema_body
: DEFEQ		lhs rhs
: PRED		pred
: DEFINE	def_params def_body
: EQEQ		lhs rhs
: DATA		lhs rhs
: XTAME		args
: XABBREV	args

> shead ::=     SHEAD word [ident]
: SHEAD		tag params

> lhs	::=	LHS ident [ident] | INLHS ident [ident] | PRELHS ident [ident]
: LHS		tag params
: INLHS		tag params
: PRELHS	tag params

> arm	::=	ARM ident (optional expr)
: ARM		tag template

> schema ::=	BODY [decl] [pred]
: BODY		decls axioms

> sexp	::=	SREF word decor [expr] [rename] line_no
>		| TEXT schema | NOT sexp | PRE sexp
>		| AND sexp sexp | OR sexp sexp | IMPLIES sexp sexp 
>		| EQUIV sexp sexp | PROJECT sexp sexp | HIDE sexp [ident] 
>		| FATSEMI sexp sexp | PIPE sexp sexp | EXISTS schema sexp
>		| FORALL schema sexp | EXISTS1 schema sexp
: TEXT		text
: SREF		sref_tag sref_decor sref_params sref_renames sref_line

> rename ::=	RENAME ident ident
: RENAME	rename_to rename_from

> decl	::=	DECL [ident] expr | SDECL sexp
: DECL		decl_names decl_expr
: SDECL		ref

> pred	::=	EQUAL expr expr | MEMBER expr expr | TRUTH | FALSITY
>		| NOT pred | AND pred pred | OR pred pred
>		| EQUIV pred pred | IMPLIES pred pred | EXISTS schema pred
>		| EXISTS1 schema pred | FORALL schema pred | SPRED sexp
>		| INREL ident expr expr | PREREL ident expr
>		| LETPRED [letdef] pred
: SPRED		ref
: EQUAL		lhs rhs
: MEMBER	lhs rhs
: INREL		op rand1 rand2
: PREREL	op rand
: LETPRED	defs body

> expr	::=	REF ident [expr] line_no | NUMBER word | SEXPR sexp
>		| EXT [expr] | COMP schema (optional expr)
>		| POWER expr | TUPLE [expr] | SEQ [expr] | CROSS [expr]
>		| THETA word decor [rename] | SELECT expr selector
>		| APPLY expr expr | LAMBDA schema expr
>		| MU schema (optional expr)
>		| INGEN ident expr expr | PREGEN ident expr
>		| INOP ident expr expr | POSTOP ident expr
>		| LETEXPR [letdef] expr | IF pred expr expr
>		| BINDING [binding]
: SEXPR		ref
: REF		tag params ref_line
: NUMBER	number
: EXT		elements
: TUPLE		elements
: CROSS		factors
: THETA		the_name the_decor the_rename
: SELECT	arg field
: INGEN		tag param1 param2
: PREGEN	tag param
: INOP		op rand1 rand2
: POSTOP	op rand
: IF		if then else
: BINDING	elements
: LETEXPR	defs body

> letdef ::= EQEQ ident expr
: EQEQ		lhs rhs

> selector ::= ident | THETA word decor [rename]

> binding ::= BIND ident expr
: BIND		lhs rhs

: UNARY		arg
: BINARY	arg1 arg2
: QUANT		bvar body
