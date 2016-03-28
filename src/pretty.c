/*
 * pretty.c
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
 * $Id: pretty.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

/* The main entry in this file is grind(), a special version of fprintf
   which handles trees, types, symbols and so on.  The function
   do_grind() is like grind(), but takes a pointer to an array of
   arguments rather than a variable-length argument list (cf.
   _doprnt): it is used by tc_error, etc.

   These are the conversions supported:

   	%d	Decimal integer.
	%i	Identifier: < prints as _ < _.
        %j      Jump 'n' blank spaces.
	%n	Identifier: < prints as <.
	%s	String.
	%t	Type.
	%x	Address (printed according to ADDR_FMT).
	%z	Tree. The identifier < prints as (_ < _).

   For internal use, there are a few more conversions:

	%b	Long schema. The lists of declarations and predicates
		are printed vertically, indented with four spaces and
		separated by `where'.
   	%k	Comma-separated list of %i's.
	%l/aaa/bbb/ccc/
		List of %z's. The empty list prints as the empty
		string, and the list t1, t2, ..., tn prints as
		aaa<t1>bbb<t2>bbb...bbb<tn>ccc.
	%p	Print nothing, but set the precedence for subsequent
		conversions from the next arg. The default is MAX.
	%q/aaa/ccc/
		Optional %z. Nothing prints as the empty string, and
		the tree t prints as aaa<t>ccc.

   Tabs in the output are printed as spaces, but used as good points
   to do line-breaking.
*/

#include "fuzz.h"
#include "symbol.h"
#include "absyn.h"
#include <string.h>
     
#define MAX 99			/* Maximum precedence for printing trees */

PUBLIC void grind(FILE *fp, char *fmt, ...)
{
     va_list a;

     va_begin(a, fmt);
     do_grind(fp, fmt, &a);
     va_end(a);
}

#define MINLINE 50
#define MAXLINE 70
#define GBUF 128

PRIVATE char gbuf[GBUF];
PRIVATE int gp = 0, breakpt = 0, spacept = 0;

PRIVATE void gputc(char ch, FILE *fp)
{
     if (ch == '\n') {
	  gbuf[gp++] = '\0';
	  fputs(gbuf, fp);
	  fputc('\n', fp);
	  gp = breakpt = spacept = 0;
	  return;
     }

     if (ch == ' ')
	  spacept = gp+1;
     else if (ch == '\t') {
	  breakpt = gp+1;
	  ch = ' ';
     }
     gbuf[gp++] = ch;

     if (gp > MAXLINE) {
	  if (breakpt <= MINLINE) breakpt = spacept;
	  if (breakpt <= MINLINE) { gbuf[gp++] = ' '; breakpt = gp; }
	  gbuf[breakpt-1] = '\0';
	  fputs(gbuf, fp);
	  fputc('\n', fp);
	  strcpy(gbuf, "        ");
	  strncat(gbuf, &gbuf[breakpt], gp-breakpt);
	  gp -= breakpt-8;
	  spacept = (spacept > breakpt ? spacept-breakpt : 0);
	  breakpt = 0;
     }
}

PRIVATE void gputs(char *s, FILE *fp)
{
     while (*s != '\0')
	  gputc(*s++, fp);
}

#define LL 		1
#define RR 		2
#define UU 		4
#define PP		8

#define NEITHER 	0
#define LEFT		LL
#define RIGHT		RR
#define ASSOC		(LL | RR)
#define UNARY		UU | LL
#define PRIM		UU | PP

#define MAXOP		59

/* tag_alist associates some kinds of node with a precedence and
   associativity; the rest have precedence 0.  I've simplified the
   precedences a little to make the output have helpful brackets. */
PRIVATE struct alist {
     int a_tag, a_prec, a_assoc;
} tag_alist[] = {
     {FORALL, 90, UNARY},
     {SFORALL, 90, UNARY},
     {EXISTS, 90, UNARY},
     {SEXISTS, 90, UNARY},
     {EXISTS1, 90, UNARY},
     {SEXISTS1, 90, UNARY},
     {FATSEMI, 85, NEITHER},
     {PIPE, 85, NEITHER},
     {HIDE, 85, LEFT},
     {PROJECT, 85, LEFT},
     {EQUIV, 83, NEITHER},
     {SEQUIV, 83, NEITHER},
     {IMPLIES, 83, NEITHER},
     {SIMPLIES, 83, NEITHER},
     {OR, 82, ASSOC},
     {SOR, 82, ASSOC},
     {AND, 81, ASSOC},
     {SAND, 81, ASSOC},
     {PRE, 80, UNARY},
     {NOT, 80, UNARY},
     {SNOT, 80, UNARY},
     {IF, 75, ASSOC},
     {INGEN, 70, NEITHER},
     {CROSS, 60, NEITHER},
     {INOP, 50, NEITHER},
     {APPLY, 40, LEFT},
     {PREGEN, 40, PRIM},
     {POWER, 40, PRIM},
     {POSTOP, 0, PRIM},
     {0, 0, 0}
};

/* check -- find the precedence of a node and of its left and right
   args.  Returns TRUE if brackets are needed. */
PRIVATE bool check(int kind, int prec, int *left, int *right)
{
     struct alist *p = tag_alist;

     while (p->a_tag != 0 && p->a_tag != kind)
	  p++;

     if (p->a_tag == 0) return FALSE;
     if (p->a_prec > prec) return TRUE;
	
     *left = *right = p->a_prec - 1;
     if (p->a_assoc & LL) (*left)++;
     if (p->a_assoc & PP) *left = 0;
     if (p->a_assoc & RR) (*right)++;
     return FALSE;
}

PRIVATE void pp_tag(FILE *fp, sym s, char *bra, char *ket)
{
     if (s->x_kind != IDENT) {
	  grind(fp, "*unknown-ident(tag=%d)*", s->x_kind);
	  return;
     }
     
     switch (lexval(s)) {
     case WORD: case SNAME:
	  grind(fp, "%n", s);
	  break;

     case PREGEN: case PREREL:
	  grind(fp, "%s%n _%s", bra, s, ket);
	  break;

     case INOP: case INGEN: case INREL: case '-':
	  grind(fp, "%s_ %n _%s", bra, s, ket);
	  break;

     case POSTOP:
	  grind(fp, "%s_ %n%s", bra, s, ket);
	  break;

     case IMAGE: case UMINUS:
	  grind(fp, "%s%n%s", bra, s, ket);
	  break;

     default:
	  grind(fp, "*unknown-ident(val=%d)*", lexval(s));
	  break;
     }
}

PRIVATE void pp_dname(FILE *fp, int dummy, tree t)
{
     pp_tag(fp, (sym) t, "", "");
}

PRIVATE void pp_tok(FILE *fp, tok x)
{
     char *s;

     if (x->x_kind != WORD) {
	  grind(fp, "*unknown-token(tag=%d)*", x->x_kind);
	  return;
     }
     
     for (s = x->l_pname; *s != '\0'; s++) {
	  if (*s == '\\' && *(s+1) == '_')
	       s++;
	  (void) gputc(*s, fp);
     }
}

#ifdef ANSI
typedef void (*elemfun)(FILE *fp, int prec, tree t);
#else
typedef void (*elemfun)();
#endif

PRIVATE void pp_list(FILE *fp, int prec, elemfun fun,
		     char *open, char *sep, char *close, tree t)
{
     tree s;
     
     if (t != nil) {
	  gputs(open, fp);
	  (*fun)(fp, prec, car(t));
	  for (s = cdr(t); s != nil; s = cdr(s)) {
	       gputs(sep, fp);
	       (*fun)(fp, prec, car(s));
	  }
	  gputs(close, fp);
     }
}

PUBLIC int list_len(tree t)
{
     int len = 0;
     for (; t != nil; t = cdr(t))
	  len++;
     return len;
}

PRIVATE char *symbol(int s)
{
     switch (s) {
     case PRE:		return "pre";
     case FORALL:
     case SFORALL:      return "forall";
     case EXISTS:	
     case SEXISTS:	return "exists";
     case EXISTS1:
     case SEXISTS1:	return "exists1";
     case LAMBDA:    	return "lambda";
     case NOT:
     case SNOT:		return "not";
     case POWER:	return "P";
     case AND:
     case SAND:		return "/\\";
     case OR:
     case SOR:		return "\\/";
     case IMPLIES:
     case SIMPLIES:	return "==>";
     case EQUIV:
     case SEQUIV:	return "<=>";
     case PROJECT:	return "project";
     case FATSEMI:	return "semi";
     case PIPE:	        return ">>";
     case TRUTH:	return "true";
     case FALSITY:	return "false";
     default:		return "*unknown-symbol*";
     }
}

PRIVATE void pp_tree(FILE *fp, int prec, tree t)
{
     int left, right;

     if (t == nil) {
	  gputs("*nil*", fp);
	  return;
     }

     if (check(t->x_kind, prec, &left, &right)) {
	  grind(fp, "(%z)", t);
	  return;
     }

     switch (t->x_kind) {
     case GIVEN:  
	  grind(fp, "[%l//, //]", t->x_given);			
	  break;

     case AXDEF: 	  
	  grind(fp, "axdef\n%bend", t->x_axdef);
	  break;

     case EQEQ:
	  grind(fp, "%z\t== %z", t->x_lhs, t->x_rhs);		
	  break;

     case DEFEQ:
	  grind(fp, "%z\t=^= %z", t->x_lhs, t->x_rhs);
	  break;

     case SDEF:
	  grind(fp, "schema %z\n%bend", t->x_heading, t->x_schema_body);
	  break;

     case VDASH:
	  grind(fp, "%z |- %z", t->x_lhs, t->x_rhs);
	  break;

     case XTAME:
	  grind(fp, "%%tame ...");
	  break;

     case XABBREV:
	  grind(fp, "%%abbrev ...");
	  break;

     case EQUAL:
	  grind(fp, "%z\t= %z", t->x_lhs, t->x_rhs);
	  break;

     case MEMBER:
	  grind(fp, "%z\tin %z", t->x_lhs, t->x_rhs);
	  break;

     case INREL:
	  grind(fp, "%z\t%n %z", t->x_rand1, t->x_op, t->x_rand2);
	  break;

     case PREREL:
	  grind(fp, "%n %z", t->x_op, t->x_rand);
	  break;

     case DATA:
	  grind(fp, "%n\t::= %l//\t| //", t->x_lhs, t->x_rhs);
	  break;

     case TEXT:
	  grind(fp, "[%z]", t->x_text);
	  break;

     case RENAME:
	  grind(fp, "%i/%i", t->x_rename_to, t->x_rename_from);
	  break;

     case DECL:
	  grind(fp, "%k:\t%z", t->x_decl_names, t->x_decl_expr);
	  break;

     case NUMBER:
	  grind(fp, "%s", (char *) t->x_number);
	  break;

     case TUPLE:
	  grind(fp, "(%l//,\t//)", t->x_elements);
	  break;

     case SEQ:
	  grind(fp, "<%l//,\t//>", t->x_elements);
	  break;

     case BINDING:
	  grind(fp, "< %l//,\t// >", t->x_elements);
	  break;

     case BIND:
	  grind(fp, "%i =>\t%z", t->x_lhs, t->x_rhs);
	  break;

     case BAG:
	  grind(fp, "[[%l//,\t//]]", t->x_elements);
	  break;

     case THETA:
	  grind(fp, "theta %z%s%l/[/, /]/",
		t->x_the_name, ((tok) (t->x_the_decor))->l_chars,
		t->x_the_rename);
	  break;

     case SELECT:
	  grind(fp, "%p%z.%z", 0, t->x_arg, t->x_field);
	  break;

     case PRED:
	  grind(fp, "%z", t->x_pred);
	  break;

     case DEFINE:
	  grind(fp, "gendef%l/ [/, /]/\n%bend",
		t->x_def_params, t->x_def_body);
	  break;

     case TRUTH:
     case FALSITY:
	  grind(fp, "%s", symbol(t->x_kind));
	  break;

     case SHEAD:
	  grind(fp, "%z%l/[/, /]/", t->x_tag, t->x_params);
	  break;

     case BODY:
	  grind(fp, "%l//;\t//%l/\t| /;\t//", t->x_decls, t->x_axioms);
	  break;

     case SREF:
	  grind(fp, "%z%s%l/[/, /]/%l/[/, /]/",
		t->x_sref_tag, ((tok) t->x_sref_decor)->l_chars,
		t->x_sref_params, t->x_sref_renames);
	  break;

     case ARM:
	  grind(fp, "%z%q/ << / >>/", t->x_tag, t->x_template);
	  break;

     case COMP:
	  grind(fp, "{ %z%q/ @\t// }", t->x_bvar, t->x_body);
	  break;

     case LAMBDA:
	  grind(fp, "(lambda %z @\t%z)", t->x_bvar, t->x_body);
	  break;

     case MU:
	  grind(fp, "(mu %z%q/ @\t//)", t->x_bvar, t->x_body);
	  break;

     case INLHS:
	  grind(fp, "%z\t%n %z",
		car(t->x_params), t->x_tag, cadr(t->x_params));
	  break;

     case PRELHS:
	  grind(fp, "%n %z", t->x_tag, car(t->x_params));
	  break;

     case LHS:
	  grind(fp, "%z%l/[/, /]/", t->x_tag, t->x_params);
	  break;

     case REF:
	  grind(fp, "%z%l/[/, /]/", t->x_tag, t->x_params);
	  break;

     case SDECL: case SEXPR: case SPRED:
	  grind(fp, "%z", t->x_ref);
	  break;

     case AND: case OR: case IMPLIES: case EQUIV:
     case SAND: case SOR: case SIMPLIES: case SEQUIV:
     case PROJECT: case FATSEMI: case PIPE:
	  grind(fp, "%p%z\t%s %p%z",
		left, t->x_arg1, symbol(t->x_kind), right, t->x_arg2);
	  break;

     case INGEN:
	  grind(fp, "%p%z\t%n %p%z",
		left, t->x_param1, t->x_tag, right, t->x_param2);
	  break;

     case HIDE:
	  grind(fp, "%p%z\t\\ (%k)", left, t->x_arg1, t->x_arg2);
	  break;

     case CROSS:
	  grind(fp, "%p%l//\tcross //", left, t->x_factors);
	  break;

     case FORALL: case EXISTS: case EXISTS1:
     case SFORALL: case SEXISTS: case SEXISTS1:
  	  grind(fp, "%s %z @\t%p%z", symbol(t->x_kind),
		t->x_bvar, left, t->x_body);
	  break;

     case LETEXPR: case LETPRED:
	  grind(fp, "(let %l//;\t// @\t%z)", t->x_defs, t->x_body);
	  break;

     case IF:
	  grind(fp, "%pif %z\tthen %z\telse %z",
		left, t->x_if, t->x_then, t->x_else);
	  break;

     case PRE: case NOT: case SNOT: case POWER:	  
	  grind(fp, "%s %p%z", symbol(t->x_kind),
		left, t->x_arg);
	  break;

     case PREGEN:
	  grind(fp, "%n %p%z", t->x_tag, left, t->x_param);
	  break;

     case POSTOP:
	  grind(fp, "%p%z %n", left, t->x_rand, t->x_op);
	  break;

     case IDENT:
	  pp_tag(fp, (sym) t, "(", ")");
	  break;

     case WORD:
	  pp_tok(fp, (tok) t);
	  break;

     case CONS:
	  grind(fp, "%l/[/, /]/", t);
	  break;

#define is_ref(t,kind)	((t)->x_kind == REF && ref_kind(t) == kind)
#define ref_kind(t)	symval((t)->x_tag)

     case APPLY:
	  if (t->x_arg1->x_kind == REF 
	      && lexval((sym) t->x_arg1->x_tag) == UMINUS
	      && t->x_arg1->x_params == nil) {
	       if (prec < MAXOP)
		    grind(fp, "(%n %p%z)", t->x_arg1->x_tag, 0, t->x_arg2);
	       else
		    grind(fp, "%n %p%z", t->x_arg1->x_tag, 0, t->x_arg2);
	  }
	  else
	       grind(fp, "%p%z %p%z", left, t->x_arg1, right, t->x_arg2);

	  break;

     case INOP:
	  if (lexval((sym) t->x_op) == IMAGE)
	       grind(fp, "%p%z (| %p%z |)%s", 0, t->x_rand1,
		     MAX, t->x_rand2, ((sym) t->x_op)->s_decor->l_chars);
	  else
	       grind(fp, "%p%z\t%n %p%z",
		     left, t->x_rand1, t->x_op, right, t->x_rand2);
	  break;

     case EXT:
	  if (list_len(t->x_elements) == 1
	      && car(t->x_elements)->x_kind == SEXPR)
	       grind(fp, "{(%z)}", car(t->x_elements));
	  else
	       grind(fp, "{%l//,\t//}", t->x_elements);
	  break;

     default:
	  grind(fp, "*unknown-expr(tag=%d)*", t->x_kind);
	  break;
     }
}

/* Grammar for printing types:

                                               PREC
	TYPE 	::= 	*NULL*			0	*null pointer
		 |	IDENT			0	given set or abbrev
		 |	@NUMBER			0	unbound type var
		 |	?			0	unassigned type var
		 |	P TYPE0			1	power-set
		 |	empty-product		2	*product of 0 terms
		 |	unit-product TYPE0	2	*product of 1 term
		 |	TYPE1 x ... x TYPE1	2	cartesian product
		 |	<| IDENT: TYPE3; ... |>	0	schema product
		 |	TYPE2 INGEN TYPE2	3	infix abbrev
		 |	PREGEN TYPE0		1	prefix abbrev
		 |	IDENT[TYPE3, ...,]	0	abbrev
		 |	(TYPE3)

   Rules marked * are only used in error output.
*/

PRIVATE bool pp_abbrev(FILE *fp, int prec, type t, frame f);

PRIVATE void pp_type(FILE *fp, int prec, type a, frame f)
{
     if (a == NULL) {
	  gputs("*NULL*", fp);
	  return;
     }
     
     switch (a->t_kind) {
	  /* If prec is high enough, each case prints the type and */
	  /* returns. Otherwise, the code at the end shoves in */
	  /* brackets and tries again with prec = 3. */
	  
     case GIVENT:
	  grind(fp, "%n", a->t_given->d_name);
	  return;
	  
     case TYPEVAR: {
	  int index = a->t_typevar;
	  type t;
	  
	  if (f == NULL || index >= fsize(f))
	       (void) grind(fp, "@%d", index+1);
	  else if ((t = f->f_var[index]) == NULL)
	       (void) grind(fp, (debug('u') ? "?%x" : "?"),
			    &f->f_var[index]);
	  else
	       pp_type(fp, prec, t, arid);
	  return;
     }
	  
     case POWERT:
	  if (prec < 1) break;
	  gputs("P ", fp);
	  pp_type(fp, 0, a->t_base, f);
	  return;
	  
     case CPRODUCT: {
	  int i;
	  
	  if (prec < 2) break;

	  if (a->t_nfields == 0)
	       gputs("empty-product", fp);
	  else if (a->t_nfields == 1) {
	       gputs("unit-product ", fp);
	       pp_type(fp, 0, a->t_field[0], f);
	  }
	  else {
	       pp_type(fp, 1, a->t_field[0], f);
	       for (i = 1; i < a->t_nfields; i++) {
		    gputs("\tx ", fp);
		    pp_type(fp, 1, a->t_field[i], f);
	       }
	  }
	  return;
     }

     case SPRODUCT: {
	  schema s = a->t_schema;
	  int i;
	  
	  gputs("<| ", fp);
	  for (i = 0; i < s->z_ncomps; i++) {
	       if (i > 0)
		    gputs(";\t", fp);
	       grind(fp, "%i:\t", s->z_comp[i].z_name);
	       pp_type(fp, 3, s->z_comp[i].z_type, f);
	  }
	  gputs(" |>", fp);
	  return;
     }
	  
     case MOLECULE:
	  pp_type(fp, prec, a->t_mtype, a->t_mframe);
	  return;	       
	  
     case ABBREV:
	  if (pp_abbrev(fp, prec, a, f))
	       return;
	  break;

     default:
	  grind(fp, "*unknown-type(tag=%d)*", a->t_kind);
	  return;
     }
     
     /* No case matches -- brackets needed */
     if (prec >= 3) {
	  grind(fp, "*unprintable-type(tag=%d)*", a->t_kind);
	  return;
     }
     gputs("(", fp);
     pp_type(fp, 3, a, f);
     gputs(")", fp);
}

/* pp_abbrev -- print a type abbrev. Returns FALSE if brackets needed. */
PRIVATE bool pp_abbrev(FILE *fp, int prec, type t, frame f)
{
     sym x = t->t_def->d_name;
     frame p = t->t_params;
     
     if (lexval(x) == INGEN && fsize(p) == 2) {
	  if (prec < 3)
	       return FALSE;
	  pp_type(fp, 2, p->f_var[0], f);
	  grind(fp, "\t%n ", x);
	  pp_type(fp, 2, p->f_var[1], f);
     } else if (lexval(x) == PREGEN && fsize(p) == 1) {
	  if (prec < 1)
	       return FALSE;
	  grind(fp, "%n ", x);
	  pp_type(fp, 0, p->f_var[0], f);
     } else {
	  grind(fp, "%z", x);
	  if (fsize(p) > 0) {
	       int i;
	       gputs("[", fp);
	       pp_type(fp, 3, p->f_var[0], f);
	       for (i = 1; i < fsize(p); i++) {
		    gputs(",\t", fp);
		    pp_type(fp, 3, p->f_var[i], f);
	       }
	       gputs("]", fp);
	  }
     }
     return TRUE;
}

PRIVATE void copy_string(char **s, char delim, char buf[])
{
     char ch, *bp = buf;

     while ((ch = *(*s)++) != delim)
	  *bp++ = ch;
     *bp = '\0';
}

PRIVATE char numbuf[20];

PUBLIC void do_grind(FILE *fp, char *fmt, va_list *ap)
{
     char ch, *s = fmt;
     int prec = MAX;

     while ((ch = *s++) != '\0') {
	  if (ch != '%') {
	       (void) gputc(ch, fp);
	       continue;
	  }

	  switch (ch = *s++) {
	  case 'd':
	       sprintf(numbuf, "%d", va_arg(*ap, int));
	       gputs(numbuf, fp);
	       break;
		    
	  case 's':
	       gputs(va_arg(*ap, char *), fp);
	       break;
		    
	  case 'x':
	       sprintf(numbuf, "%#lx", (unsigned long) va_arg(*ap, char *));
	       gputs(numbuf, fp);
	       break;

	  case 'k':
	       pp_list(fp, prec, pp_dname, "", ", ", "",
		       va_arg(*ap, tree));
	       break;

	  case 'l': {
	       char delim = *s++;
	       char open[16], sep[16], close[16];
	       copy_string(&s, delim, open);
	       copy_string(&s, delim, sep);
	       copy_string(&s, delim, close);
	       pp_list(fp, prec, pp_tree, open, sep, close,
		       va_arg(*ap, tree));
	       break;
	  }
		    
	  case 'q': {
	       tree t = va_arg(*ap, tree);
	       char delim = *s++;
	       char open[10], close[10];
	       copy_string(&s, delim, open);
	       copy_string(&s, delim, close);
	       if (exists(t))
		    grind(fp, "%p%s%z%s", prec, open, the(t), close);
	       break;
	  }

	  case 'b': {
	       tree t = va_arg(*ap, tree);
	       grind(fp, "%l/    /\n    //%l/\nwhere\n    /\n    //\n",
		     t->x_decls, t->x_axioms);
	       break;
	  }
		    
	  case 'n': {
	       sym x = va_arg(*ap, sym);
	       pp_tok(fp, x->s_basename);
	       gputs(x->s_decor->l_chars, fp);
	       break;
	  }

	  case 'j': {
	       int i, n = va_arg(*ap, int);
	       for (i = 0; i < n; i++)
		    gputc(' ', fp);
	       break;
	  }

	  case 'i': pp_dname(fp, prec, va_arg(*ap, tree)); break;
	  case 't': pp_type(fp, 3, va_arg(*ap, type), arid); break;
	  case 'z': pp_tree(fp, prec, va_arg(*ap, tree)); break;
	  case 'p': prec = va_arg(*ap, int); break;
	
	  default:  (void) gputc(ch, fp);
	  }
     }
}

PUBLIC void show_tree(tree t)
{
     grind(stdout, "%z\n", t);
}

PUBLIC void show_type(type t)
{
     grind(stdout, "%t\n", t);
}
