/*
 * zparse.y
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
 * $Id: zparse.y,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

%start		text 

/* Tokens produced by the lexical analyser */
%token		AND BAXDEF BGENDEF BSCHEMA BSUP BSYNTAX BZED COCOEQ
%token		CROSS DECOR DEFEQ DUMMY EAXDEF EGENDEF EQEQ EQUIV ESCHEMA
%token		ESUP ESYNTAX EXISTS EXISTS1 EZED FALSITY FATSEMI
%token		FORALL GREEK HIDE IMPLIES IN INGEN INREL LAMBDA LBAG LDATA
%token		LET LIMG MU NL NOT NUMBER OR
%token		PIPE POWER PRE PROJECT RBAG RDATA RIMG THETA TRUTH UMINUS
%token		VDASH WHERE WORD XINGEN XINOP XINREL XPOSTOP XPREGEN XPREREL
%token		XABBREV XTAME XPNAME SNAME PREGEN PREREL POSTOP IMAGE 
%token		STRING LBRACE RBRACE YINREL EDIR IF THEN ELSE LANGLE 
%token		BIND RANGLE XTOKEN XIGNORE IGNORE XDEBUG XLINE
%token		BADTOK

/* Other tokens used as tags in the tree */
%token		APPLY BAG BODY COMP DECL EQUAL EXT GIVEN IDENT INLHS
%token		INOP JUST LHS MEMBER NOTHING PRELHS REF SDEF SREF SELECT
%token		TEXT SEQ TUPLE CONS DATA ARM BINDING LETDEF
%token		PRED AXDEF DEFINE RENAME SHEAD PARA SEXPR SPRED SDECL
%token		LETEXPR LETPRED SNOT SAND SOR SIMPLIES SEQUIV SEXISTS
%token		SFORALL SEXISTS1

%nonassoc	ERROR	/* removes conflicts with error prodns. */
%right		SET 	/* removes conflicts with set expressions. */

/* The lines below marked 'paren matching' are there only to avoid
   conflicts with the error production (expr3 --> '(' expr). */
%left		PIPE
%left		FATSEMI
%left		HIDE
%left		PROJECT
%left		EQUIV
%right		IMPLIES
%left		OR
%left		AND
%left		NOT PRE
%nonassoc	INREL '=' IN YINREL /* paren matching */
%nonassoc	ELSE
%right		INGEN
%right		CROSS
%nonassoc	INOP '-'
%nonassoc	UMINUS
%nonassoc	LIMG
%nonassoc	POSTOP /* paren matching */
%nonassoc	WORD NUMBER THETA '(' LBRACE
%nonassoc	BSUP '^' '.' LBAG LANGLE /* paren matching */

%right		SNAME GREEK ','
%right		')' ']' RBRACE

%token		MAX_TOKEN

%{
#include "fuzz.h"
#include "absyn.h"
#include <string.h>
#include <ctype.h>

#define yylex zzlex /* link to newline filter */

#ifdef DEBUG
#define YYDEBUG 1
#endif

PRIVATE tree schema_head(tok name, tok decor, tree formals, int loc);
PRIVATE tree promote(tree a, int loc);
PRIVATE tree sexp_fix(tree a);
PRIVATE tree pred_fix(tree a);
PRIVATE void check_end(tok found);
PRIVATE void set_value(tree syms, int val, int prio);
PRIVATE tree reduce_ops(tree exp);

PRIVATE tree finger;
PRIVATE int end_tok;

EXTERN int line_count;

#define line first_line

#define mk_id(n,d)	((tree) mk_symbol((tok) (n), (tok) (d)))
%}

%%

/* SPECIFICATIONS */

text :
    text section
  | /* empty */ ;

section :
    para
  | error  			{ sflag = TRUE; } ;

para :
    box				{ do_para($1); }
  | display
  | dir ;

box :
    BAXDEF { end_tok = EAXDEF; } long.schema 
				{ $$ = node(1,@1.line,AXDEF,$3); }
  | BGENDEF { end_tok = EGENDEF; } g.formal.opt long.schema
				{ $$ = node(2,@1.line,DEFINE,$3,$4); }
  | BSCHEMA { end_tok = ESCHEMA; } schema.head long.schema
				{ $$ = node(2,@1.line,SDEF,$3,$4); } ;

display :
    display.begin zed.text end  { check_end((tok) $3); } ;

zed.text :
    short.para
  | zed.text sep short.para ;

display.begin :	
    BZED			{ end_tok = EZED; }
  | BSYNTAX			{ end_tok = ESYNTAX; } ;

end : EAXDEF | EGENDEF | ESCHEMA | EZED | ESYNTAX ;

long.schema :
    decl.part WHERE axiom.part  { $$ = node(2,@1.line,BODY,$1,$3); }
  | decl.part end		{ check_end((tok) $2); 
				  $$ = node(2,@1.line,BODY,$1,nil); } ;

axiom.part :
    axioms end			{ check_end((tok) $2); $$ = $1; }
  | error end			{ $$ = nil; } ;

axioms	:	
    pred			{ $$ = list1($1); }
  | axioms sep pred		{ $$ = snoc($1,$3); } ;

sep : ';' | NL ;

short.para :
    short.para.ok		{ do_para($1); }
  | error			{ sflag = TRUE; } ;

short.para.ok :	
    '[' dname.list ']'		{ $$ = node(1,@1.line,GIVEN,$2); }
  | schema.lhs DEFEQ sexp	{ $$ = node(2,@2.line,DEFEQ,$1,$3); }
  | def.lhs EQEQ expr		{ $$ = node(2,@2.line,EQEQ,$1,$3); }
  | schema.lhs DEFEQ %prec ERROR
	{ yyerror("Schema expression expected after \\defs"); YYERROR; }
  | ident COCOEQ data.rhs	{ $$ = node(2,@2.line,DATA,$1,$3); }
  | pred			{ $$ = node(1,@1.line,PRED,$1); }
  | sexp VDASH pred		{ $$ = node(2,@2.line,VDASH,$1,$3); } ;

/* The trouble here is that the left-hand sides of == and \defs
   (and also = in a predicate, etc.) can sometimes look identical
   until the symbol itself is seen: */

def.lhs :
    /* unfolded: vname g.formal.opt	*/
    ident g.formal.opt		
	{ $$ = node(2,@1.line,LHS,$1,$2); }
  | '(' op.name ')' g.formal.opt
	{ $$ = node(2,@2.line,LHS,$2,$4); }
  | PREGEN decor ident		
	{ $$ = node(2,@1.line,PRELHS,mk_id($1,$2),list1($3)); }
  | ident INGEN decor ident
	{ $$ = node(2,@2.line,INLHS,mk_id($2,$3),list2($1,$4)); } ;

schema.lhs :	
    ident g.formal.opt		
	{ sym s = (sym) $1;
	  $$ = schema_head(s->s_basename, s->s_decor, $2, @1.line); }
  | sname decor g.formal.opt  
	{ $$ = schema_head((tok) $1, (tok) $2, $3, @1.line); } ;

schema.head :	
    '{' future.sname '}' g.formal.opt
	{ $$ = schema_head((tok) $2, empty, $4, @2.line); }
  | /* Obsolete form: */
    '{' future.sname gen.formals '}'
	{ $$ = schema_head((tok) $2, empty, $3, @2.line); } ;

future.sname : sname | WORD ;

sname :
    SNAME
  | GREEK SNAME			{ $$ = (tree) mk_greek((tok) $1, (tok) $2); }
  | GREEK WORD			{ $$ = (tree) mk_greek((tok) $1, (tok) $2); } ;

dir :
    XPREGEN symbols EDIR	{ set_value($2, PREGEN, 0); }
  | XINGEN symbols EDIR		{ set_value($2, INGEN, 0); }
  | XPREREL symbols EDIR	{ set_value($2, PREREL, 0); }
  | XINREL symbols EDIR		{ set_value($2, INREL, 0); }
  | XINOP symbols NUMBER EDIR	{ set_value($2, INOP, atoi((char *) $3)); }
  | XPOSTOP symbols EDIR	{ set_value($2, POSTOP, 0); }
  | /* There should be a way to make decorated names into
       abbreviations or tame functions, but there isn't. */
    XABBREV symbols EDIR	{ do_para(node(1,@1.line,XABBREV,$2)); }
  | XTAME symbols EDIR		{ do_para(node(1,@1.line,XTAME,$2)); }
  | XPNAME symbol STRING EDIR	{ ((tok) $2)->l_pname = (char *) $3; }
  | XTOKEN STRING NUMBER EDIR	{ enter((char *) $2, atoi((char *) $3)); }
  | XIGNORE symbols EDIR	{ set_value($2, IGNORE, 0); }
  | XDEBUG STRING EDIR
	{ char *s;
	  for (s = (char *) $2; *s != '\0'; s++)
		  if (islower(*s)) debflag(*s)++;
	  yydebug = debug('y'); }
  | XLINE NUMBER string.opt EDIR
	{ line_count = atoi((char *) $2);
	  if ((char *) $3 != NULL) 
		file_name = (char *) $3; } ;

string.opt :
    STRING
  | /* empty */			{ $$ = nil; } ;

data.rhs :
    arm				{ $$ = list1($1); }
  | data.rhs '|' arm		{ $$ = snoc($1,$3); } ;

arm :
    ident
	{ $$ = node(2, @1.line, ARM, $1, node(0, @1.line, NOTHING)); }
  | vname LDATA expr RDATA	
	{ $$ = node(2, @1.line, ARM, $1, node(1, @3.line, JUST, $3)); } ;

schema :
    decl.seq pred.seq.opt	{ $$ = node(2,@1.line,BODY,$1,$2); } ;


/* SCHEMA DESIGNATORS */

sref :
    sname sref.tail		{ $$ = $2; } ;

/* This is a hack to allow 'easy.decl'. */
sref.tail :
    decor '[' expr.list ']' rename.opt
				{ $$ = sref($0,$1,$3,$5,@0.line); }
  | decor rename.opt		{ $$ = sref($0,$1,nil,$2,@0.line); } ;

rename.opt :
    '[' rename.list ']'		{ $$ = $2; }
  | /* empty */			{ $$ = nil; } ;

rename.list :
    rename			{ $$ = list1($1); }
  | rename.list ',' rename	{ $$ = snoc($1,$3); } ;

rename :
    dname '/' dname		{ $$ = node(2,@2.line,RENAME,$1,$3); } ;


/* DECLARATIONS */

decl.part :
    easy.decl 			{ $$ = list1($1); }
  | decl.part sep easy.decl	{ $$ = snoc($1,$3); } ;

decl :
    dname.list ':' expr		{ $$ = node(2,@2.line,DECL,$1,$3); }
  | sref %prec SET		{ $$ = node(1,@1.line,SDECL,$1); } ;

/* An 'easy.decl' allows an undeclared name to be used as a schema name */
easy.decl :
    decl
  | WORD sref.tail		{ ((tok) $1)->l_value = SNAME; 
				  $$ = node(1,@1.line,SDECL,$2); } ;


/* SCHEMA EXPRESSIONS and PREDICATES */

sexp : logic			{ $$ = sexp_fix($1); } ;

pred : logic			{ $$ = pred_fix($1); } ;

logic :
    FORALL schema '@' logic	{ $$ = node(2,@1.line,FORALL,$2,$4); }
  | EXISTS schema '@' logic	{ $$ = node(2,@1.line,EXISTS,$2,$4); }
  | EXISTS1 schema '@' logic	{ $$ = node(2,@1.line,EXISTS1,$2,$4); }
  | LET let.defs '@' logic	{ $$ = node(2,@1.line,LETPRED,$2,$4); }
  | logic1 %prec ERROR ;

logic1 :
    rel.chain
  | PREREL decor expr		{ $$ = node(2,@1.line,PREREL,mk_id($1,$2),$3); }
  | bra.sref %prec ')'		{ $$ = $1; }
  | '[' schema ']'		{ $$ = node(1,@1.line,TEXT,$2); }
  | TRUTH			{ $$ = node(0,@1.line,TRUTH); }
  | FALSITY			{ $$ = node(0,@1.line,FALSITY); }
  | NOT logic1			{ $$ = node(1,@1.line,NOT,$2); }
  | PRE logic1			{ $$ = node(1,@1.line,PRE,$2); }
  | logic1 AND logic1		{ $$ = node(2,@2.line,AND,$1,$3); }
  | logic1 OR logic1		{ $$ = node(2,@2.line,OR,$1,$3); }
  | logic1 IMPLIES logic1	{ $$ = node(2,@2.line,IMPLIES,$1,$3); }
  | logic1 EQUIV logic1		{ $$ = node(2,@2.line,EQUIV,$1,$3); }
  | logic1 PROJECT logic1	{ $$ = node(2,@2.line,PROJECT,$1,$3); }
  | logic1 HIDE '(' dname.list ')'  { $$ = node(2,@2.line,HIDE,$1,$4); }
  | logic1 FATSEMI logic1	{ $$ = node(2,@2.line,FATSEMI,$1,$3); }
  | logic1 PIPE logic1		{ $$ = node(2,@2.line,PIPE,$1,$3); }
  | '(' logic ')'		{ $$ = $2; }
  | '(' logic			{ yyerror("Closing parenthesis expected");
			  	  YYERROR; } 
  | quant %prec ERROR		{ yyerror("Opening parenthesis expected");
				  YYERROR; } ;

quant : FORALL | EXISTS | EXISTS1 ;

rel.chain :
    expr mark.expr rel.item { $$ = $3; }
  | rel.chain rel.item		{ $$ = node(2,@2.line,AND,$1,$2); } ;

rel.item :	
    last.expr '=' expr mark.expr
	{ $$ = node(2,@2.line,EQUAL,$1,$3); }
  | last.expr IN expr mark.expr  
	{ $$ = node(2,@2.line,MEMBER,$1,$3); }
  | last.expr relation expr mark.expr
	{ $$ = node(3,@2.line,INREL,$2,$1,$3); } ;

/*
 * mark.expr remembers the item to its left just long enough for an
 * immediately following last.expr to return it.  The possibility of
 * nested relation chains -- e.g. a = { x: N | x = y = z } = b -- means
 * that our finger can easily lose its place. Luckily, last.expr has
 * pushed it on the value stack first.
 */
mark.expr :	
    /* empty */ %prec ERROR	{ finger = $0; } ;
last.expr :
    /* empty */			{ $$ = finger; } ;


/* EXPRESSIONS */

/*
 * The syntax generalizes that of ZRM by allowing decorations on
 * operator symbols -- they are the natural result of decorating a
 * schema that has components named by infix symbols, etc.
 */

expr0 : quant.expr | expr ;

quant.expr :	
    LAMBDA schema '@' expr 	{ $$ = node(2,@1.line,LAMBDA,$2,$4); }
  | MU schema expr.opt		{ $$ = node(2,@1.line,MU,$2,$3); }
  | LET let.defs '@' expr	{ $$ = node(2,@1.line,LETEXPR,$2,$4); } ;

let.defs :
    let.def			{ $$ = list1($1); }
  | let.defs ';' let.def	{ $$ = snoc($1, $3); } ;

let.def	:
    vname EQEQ expr		{ $$ = node(2,@2.line,LETDEF,$1,$3); } ;

expr :
    IF pred THEN expr ELSE expr  { $$ = node(3,@1.line,IF,$2,$4,$6); }
  | expr0.5 %prec ERROR ;

expr0.5	:
    expr0.5 INGEN decor expr0.5	{ $$ = node(3,@2.line,INGEN,mk_id($2,$3),$1,$4); }
  | product %prec CROSS		{ $$ = node(1,@1.line,CROSS,$1); }
  | expr1 %prec ERROR
  | bad.exp.quant		{ yyerror("Opening parenthesis expected");
			  	  YYERROR; } ;

bad.exp.quant :	
    LAMBDA %prec ERROR
  | MU %prec ERROR
  | LET %prec ERROR ;

/*
 * Sometimes the parser doesn't know whether to expect an expression or
 * the left-hand side of a definition; if it sees an identifier
 * followed by an infix generic, the decision has to be delayed a
 * little longer:
 */
expr0.5	:
    ident INGEN decor expr0.5
	{ $$ = node(3,@2.line,INGEN,mk_id($2,$3),simply($1,@1.line),$4); } ;

product :
    expr1 CROSS expr1		{ $$ = list2($1,$3); }
  | product CROSS expr1		{ $$ = snoc($1,$3); } ;

expr1 :
    op.chain			{ $$ = reduce_ops($1); } ;

op.chain :
    expr1.5 %prec ERROR		{ $$ = list2($1, (tree) infop); }
  | expr1.5 inop decor op.chain %prec INOP
				{ $$ = cons($1,cons(mk_id($2,$3),$4)); } ;

inop : '-' | INOP ;

expr1.5	:
    POWER expr3 %prec ERROR	{ $$ = node(1,@1.line,POWER,$2); }
  | PREGEN decor expr3 %prec ERROR
				{ $$ = node(2,@1.line,PREGEN,mk_id($1,$2),$3); }
  | '-' decor expr3 %prec UMINUS
  	 { $$ = node(2, @1.line, APPLY, simply(mk_id(uminus, $2), @1.line), $3); }
  | expr3 LIMG expr0 RIMG decor
  	 { $$ = node(3, @2.line, INOP, mk_id(image, $5), $1, $3); }
  | expr2 %prec ERROR ; 

expr2	:
    expr2 expr3 %prec ERROR	{ $$ = node(2,@2.line,APPLY,$1,$2); }
  | expr3 %prec ERROR ;

expr3	:	
    /* unfolded: vname g.actual.opt */
    ident g.actual.opt		{ $$ = ref($1,$2,@1.line); }
  | '(' op.name ')' g.actual.opt  { $$ = ref($2,$4,@2.line); }
  | NUMBER			{ $$ = node(1,@1.line,NUMBER,$1); }
  | bra.sref %prec ')'		{ $$ = $1; }
  | set
  | LANGLE binding.list RANGLE	{ $$ = node(1,@1.line,BINDING,$2); }
  | '(' expr ',' expr.list ')' 	{ $$ = node(1,@1.line,TUPLE,cons($2,$4)); }
  | LANGLE expr.list.opt RANGLE { $$ = node(1,@1.line,SEQ,$2); }
  | LBAG expr.list.opt RBAG 	{ $$ = node(1,@1.line,BAG,$2); }
  | theta
  | expr3 '.' selector		{ $$ = node(2,@2.line,SELECT,$1,$3); }
  | expr3 POSTOP decor /* ZRM makes this an expr1. */
  	 			{ $$ = node(2,@2.line,POSTOP,mk_id($2,$3),$1); }
  | expr3 BSUP expr ESUP /* ZRM makes this an expr1. */
  	 { $$ = node(2, @2.line, APPLY, node(2, @2.line, APPLY,
  	 		simply((tree) iter, @2.line), $3), $1); }
  | expr3 '^' '{' expr '}' /* ZRM makes this an expr1. */
  	 { $$ = node(2,@2.line,APPLY,node(2,@2.line,APPLY,
  	 		simply((tree) iter, @2.line), $4), $1); }
  | /* unfolded: '(' expr0 ')' */
    '(' expr ')'		{ $$ = $2; }
  | '(' quant.expr ')'		{ $$ = $2; } ; 

selector : vname | theta ;

binding.list :
    binding			{ $$ = list1($1); }
  | binding.list ',' binding	{ $$ = snoc($1,$3); } ;

binding :	
    dname BIND expr		{ $$ = node(2,@2.line,BIND,$1,$3); } ;

theta :
    THETA word decor rename.opt { $$ = node(3,@1.line,THETA,$2,$3,$4); } ;

/*
 * This error production gives a useful error message for missing
 * right parentheses. The mark.expr and last.expr turn a reduce/reduce
 * conflict with the productions for rel.chains into a shift/reduce
 * conflict that can be resolved in favour of shifting the relation
 * sign, e.g. in (3 + 4 < ... .
 */
expr3 :
    '(' expr mark.expr last.expr %prec ERROR
	{ yyerror("Closing parenthesis expected"); YYERROR; }
  | '(' expr ',' expr.list %prec ERROR
	{ yyerror("Closing parenthesis expected"); YYERROR; } ;


/* SET EXPRESSIONS */

/*
 * The syntax of set expressions (set) is:

set :
    LBRACE expr.list.opt RBRACE	{ $$ = node(1,@1.line,EXT,$2); }
  | LBRACE schema expr.opt RBRACE { $$ = node(2,@1.line,COMP,$2,$3); } ;

 * but this is (i) ambiguous because { sref } may be either an
 * extension or a comprehension, and (ii) not LR(1), because a schema
 * may start with a list of identifiers. So after a LBRACE, we look for a
 * list of identifiers; if we eventually find a colon, we put together
 * a declaration and continue with a comprehension. If not, then the
 * identifiers are promoted to exprs and we continue with an extension.
 * According to ZRM, the form { sref } is regarded as a comprehension.
 */

set :
    LBRACE expr.list.opt RBRACE	{ $$ = node(1,@1.line,EXT,$2); }
  | LBRACE schema expr.opt RBRACE { $$ = node(2,@1.line,COMP,$2,$3); }
  | LBRACE some.idents expr.list RBRACE
	{ $$ = node(1, @1.line, EXT, join(promote($2, @2.line), $3)); }
  | LBRACE set.decl more.decls pred.seq.opt expr.opt RBRACE
	{ $$ = node(2, @1.line, COMP, 
		    node(2, @2.line, BODY, cons($2, $3), $4), $5); }
  | LBRACE sref RBRACE
	{ $$ = node(2, @1.line, COMP,
		    node(2, @2.line, BODY,
			 list1(node(1, @2.line, SDECL, $2)),
			 nil),
		    node(0, @3.line, NOTHING)); } ;

set.decl :
    some.idents dname.list ':' expr
	{ $$ = node(2,@1.line,DECL,join($1,$2),$4); } ;

some.idents :
    ident ','			{ $$ = list1($1); }
  | some.idents ident ','	{ $$ = snoc($1,$2); } ;

more.decls :
    ';' decl.seq		{ $$ = $2; }
  | /* empty */			{ $$ = nil; } ;

/*
 * A bracketed schema designator (bra.sref) is an sref in arbitrarily
 * deep parentheses. The idea is needed to avoid a reduce/reduce
 * conflict when parsing a schema-as-predicate. Relevant precedence
 * assignments are on the productions (logic1 --> bra.sref) and (expr3
 * --> bra.sref). Closing parenthesis is left-associative, so the
 * parser shifts it.
 *
 * Let-constructs add even more variety, and we're forced to build a tree
 * that is labelled as a predicate or expression later.
 */
bra.sref :
    '(' bra.sref ')'		{ $$ = $2; }
  | '(' LET let.defs '@' bra.sref ')'
				{ $$ = node(2,@2.line,LETEXPR,$3,$5); }
  | sref %prec SET		{ $$ = node(1,@1.line,SEXPR,$1); } ;


/* GENERIC PARAMETER LISTS */

/*
 * Sometimes when the parser sees a [, it doesn't know whether to
 * expect a formal or an actual parameter list; the symbol following
 * the ] always decides the question, however, if no compound
 * expression decides it before then.  It's worse! Sometimes the [
 * could start a rename -- then the issue is decided by the token
 * following the first ident, so we need to let the parser shift the
 * ident before deciding.
 */

g.actual.opt :
    '[' ref.head ident ']'	{ $$ = promote(snoc($2, $3), @2.line); }
  | '[' ref.head expr.list ']'	{ $$ = join(promote($2, @2.line), $3); }
  | '[' ident ']'		{ $$ = promote(list1($2), @2.line); }
  | '[' expr.list ']'		{ $$ = $2; }
  | /* empty */ %prec SET	{ $$ = nil; } ;

g.formal.opt :	
    gen.formals
  | /* empty */			{ $$ = nil; } ;

gen.formals :	
    '[' ref.head ident ']'	{ $$ = snoc($2,$3); }
  | '[' ident ']'		{ $$ = list1($2); } ;

ref.head :
    ident ','			{ $$ = list1($1); }
  | ref.head ident ','		{ $$ = snoc($1,$2); } ;


/* OPTIONAL THINGS AND LISTS OF THINGS */

decl.seq :
    decl			{ $$ = list1($1); }
  | decl.seq ';' decl		{ $$ = snoc($1,$3); } ;

pred.seq.opt :
    '|' pred.seq		{ $$ = $2; }
  | /* empty */			{ $$ = nil; } ;

pred.seq :
    pred			{ $$ = list1($1); }
  | pred.seq ';' pred		{ $$ = snoc($1,$3); } ;

expr.list :
    expr %prec ERROR		{ $$ = list1($1); }
  | expr.list ',' expr		{ $$ = snoc($1,$3); } ;

expr.list.opt :	
    expr.list
  | /* empty */			{ $$ = nil; } ;

expr.opt :
    '@' expr			{ $$ = node(1, @2.line, JUST, $2); }
  | /* empty */			{ $$ = node(0, 0, NOTHING); } ;

dname.list :
    dname			{ $$ = list1($1); }
  | dname.list ',' dname	{ $$ = snoc($1,$3); } ;


/* BITS AND PIECES */

vname :
    ident
  | '(' op.name ')'		{ $$ = $2; } ;

dname :
    ident %prec SET
  | op.name ;

ident :
    WORD decor			{ $$ = mk_id($1,$2); } ;

op.name	:
    DUMMY in.sym decor DUMMY	{ $$ = mk_id($2,$3); }
  | DUMMY relation DUMMY	{ $$ = $2; }
  | /* unfolded: pre.sym decor DUMMY */
    PREGEN decor DUMMY		{ $$ = mk_id($1,$2); }
  | PREREL decor DUMMY		{ $$ = mk_id($1,$2); }
  | DUMMY post.sym decor	{ $$ = mk_id($2,$3); }
  | '-' decor			{ $$ = mk_id(uminus,$2); }
  | DUMMY LIMG DUMMY RIMG decor	{ $$ = mk_id(image,$5); } ;

pre.sym	: PREGEN | PREREL ;
in.sym : INOP | INGEN | '-' ;
post.sym : POSTOP ;

relation :
    INREL decor			{ $$ = mk_id($1,$2); }
  | YINREL '{' ident '}'  	{ $$ = $3; } ;

decor :
    DECOR
  | /* empty */			{ $$ = (tree) empty; } ;

word : WORD | sname ;
symbol : 
    word | pre.sym | in.sym | post.sym | INREL
  | LIMG RIMG			{ $$ = (tree) image; } ;

symbols :
    symbol			{ $$ = list1($1); }
  | symbols symbol		{ $$ = snoc($1,$2); } ;

%%

/* schema_head -- check and construct a schema heading */
PRIVATE tree schema_head(tok name, tok decor, tree formals, int loc)
{
     if (decor != empty)
	  yyerror("Decoration ignored in schema def");
     name->l_value = SNAME;
     return node(2, loc, SHEAD, (tree) name, formals);
}

/* join -- destructively concatenate two lists */
PUBLIC tree join(tree a, tree b)
{
     tree p = a;

     if (a == nil)
	  return b;
     else {
	  while (cdr(p) != nil)
	       p = cdr(p);
	  cdr(p) = b;
	  return a;
     }
}

/* promote -- destructively convert a list of identifiers into references */
PRIVATE tree promote(tree a, int loc)
{
     tree b;

     for (b = a; b != nil; b = cdr(b))
	  car(b) = simply(car(b), loc);
     return a;
}

PRIVATE struct { short old, new; } fix_table[] = {
     {FORALL, SFORALL}, {EXISTS, SEXISTS}, {EXISTS1, SEXISTS1}, 
     {NOT, SNOT}, {AND, SAND}, {OR, SOR}, {IMPLIES, SIMPLIES},
     {EQUIV, SEQUIV}, {0, 0}
} ;

/* schemafy -- schema version of logical operator */
PRIVATE int schemafy(int op)
{
     int i;

     for (i = 0; fix_table[i].old != 0; i++)
	  if (fix_table[i].old == op)
	       return fix_table[i].new;

     return op;
}

/* sexp_fix -- convert logical expression into schema expression */
PRIVATE tree sexp_fix(tree a)
{
     switch (a->x_kind) {
     case FORALL: case EXISTS: case EXISTS1:
	  a->x_kind = schemafy(a->x_kind);
	  a->x_body = sexp_fix(a->x_body);
	  return a;

     case TEXT:
	  return a;

     case SEXPR:
	  return a->x_arg;

     case NOT: case PRE:
	  a->x_kind = schemafy(a->x_kind);
	  a->x_arg = sexp_fix(a->x_arg);
	  return a;

     case AND: case OR: case IMPLIES: case EQUIV:
     case PROJECT: case FATSEMI: case PIPE:
	  a->x_kind = schemafy(a->x_kind);
	  a->x_arg1 = sexp_fix(a->x_arg1);
	  a->x_arg2 = sexp_fix(a->x_arg2);
	  return a;

     case HIDE:
	  a->x_arg1 = sexp_fix(a->x_arg1);
	  return a;

     default:
	  tc_error(a->x_loc, "Syntax error in schema expression (%d)",
		   a->x_kind);
	  tc_e_end();
	  sflag = TRUE;
	  return nil;
     }
}

/* pred_fix -- convert logical expression to predicate */
PRIVATE tree pred_fix(tree a)
{
     switch (a->x_kind) {
     case FORALL: case EXISTS: case EXISTS1:
	  a->x_body = pred_fix(a->x_body);
	  return a;

     case LETPRED: case LETEXPR:
	  /* LETEXPR is created by bra.sref */
	  a->x_kind = LETPRED;
	  a->x_body = pred_fix(a->x_body);
	  return a;

     case EQUAL: case MEMBER: case INREL: case PREREL:
     case TRUTH: case FALSITY:
	  return a;

     case SEXPR:
	  a->x_kind = SPRED;
	  return a;

     case PRE:
	  if (a->x_arg->x_kind == SEXPR)
	       a->x_arg = a->x_arg->x_arg;

	  if (a->x_arg->x_kind != SREF) {
	       tc_error(a->x_loc, 
			"argument of pre must be a schema reference");
	       tc_e_end();
	       sflag = TRUE;
	       return nil;
	  }
	  return node(1,a->x_loc,SPRED,a);

     case NOT:
	  a->x_arg = pred_fix(a->x_arg);
	  return a;

     case AND: case OR: case IMPLIES: case EQUIV:
	  a->x_arg1 = pred_fix(a->x_arg1);
	  a->x_arg2 = pred_fix(a->x_arg2);
	  return a;

     default:
	  tc_error(a->x_loc, "Syntax error in predicate (%d)",
		   a->x_kind);
	  tc_e_end();
	  sflag = TRUE;
	  return nil;
     }
}

/* set_value -- set token values of a list of symbols */
PRIVATE void set_value(tree syms, int val, int prio)
{
     tree t;

     for (t = syms; t != nil; t = cdr(t)) {
	  tok x = (tok) car(t);
	  if (x->l_value != '-') x->l_value = val;
	  x->l_prio = prio;
     }
}

/* check_end -- check that \begin{foo} and \end{foo} match */
PRIVATE void check_end(tok found)
{
     if (found->l_value != end_tok)
	  yyerror("\\end does not match \\begin");
}

/* name_of_tag -- translate tree tag to string */
PUBLIC char *name_of_tag(int x)
{
	return (char *) yytname[YYTRANSLATE(x)];
}


#define get_prio(s) s->s_basename->l_prio

/* reduce_ops -- reduce a list of infix operators and operands */
PRIVATE tree reduce_ops(tree t)
{
     tree rand[MAX_ARGS];
     sym rator[MAX_ARGS];
     int n = 0;

     for (; t != nil; t = cddr(t)) {
	  tree u = car(t);
	  sym w = (sym) cadr(t);
	  while (n > 0 && get_prio(rator[n-1]) >= get_prio(w)) {
	       n--; 
	       u = node(3, rand[n]->x_loc, INOP, rator[n], rand[n], u);
	  }
	  rand[n] = u;
	  rator[n++] = w;
	  if (debug('o')) {
	       int i;
	       grind(stdout, "Redop:");
	       for (i = 0; i < n; i++)
		    grind(stdout, " <%z> %n(%d)", 
			  rand[i], rator[i], get_prio(rator[i]));
	       grind(stdout, "\n");
	  }
     }

     if (n != 1) panic("reduce_ops");
     return rand[0];
}
