/*
 * schema.c
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
 * $Id: schema.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 *
 */

#include "fuzz.h"
#include "symbol.h"
#include "absyn.h"
#include <string.h>

/* open_sref -- find a schema and process its parameters */
PUBLIC bool open_sref(tree t, env e, def *d, frame *f)
{
     if ((*d = get_schema((tok) t->x_sref_tag, t->x_loc)) == NULL)
	  return FALSE;
     *f = mk_frame((*d)->d_nparams);
     get_params("Schema", mk_symbol((tok) t->x_sref_tag, empty),
		t->x_sref_params, e, *f, t->x_loc);
     return TRUE;
}

/* check_rename -- check that all renamed components exist */
PUBLIC void check_rename(schema s, tok dec, tree rename, tree cxt)
{
     tree r;
     int i;

     for (r = rename; r != nil; r = cdr(r)) {
	  sym x = (sym) car(r)->x_rename_from;
	  for (i = 0; i < s->z_ncomps; i++)
	       if (paint(s->z_comp[i].z_name, dec) == x)
		    goto ok;
	  tc_error(cxt->x_loc, "Renamed component %n does not exist", x);
	  tc_e_etc("Expression: %z", cxt);
	  tc_e_end();
     ok:
	  ;
     }
}

/* get_rename -- decorate and rename an identifier */
PUBLIC sym get_rename(sym x, tok dec, tree rename)
{
     tree r;
     sym xx = paint(x, dec);

     for (r = rename; r != nil; r = cdr(r))
	  if ((sym) car(r)->x_rename_from == xx)
	       return ((sym) car(r)->x_rename_to);

     return xx;
}

/* do_sref -- add the variables of a schema reference to an env */
PUBLIC void do_sref(tree t, env e, env v)
{
     tok dec = (tok) t->x_sref_decor;
     tree renames = t->x_sref_renames;
     def d;
     schema s;
     frame params;
     int i;

     if (t->x_kind != SREF) bad_tag("do_sref", t->x_kind);
     if (! open_sref(t, e, &d, &params)) {
	  v->e_partial = TRUE;
	  return;
     }
     s = d->d_schema;
     check_rename(d->d_schema, dec, renames, t);
     for (i = 0; i < s->z_ncomps; i++)
	  merge_def(VAR, get_rename(s->z_comp[i].z_name, dec, renames),
		    seal(s->z_comp[i].z_type, params), v, t, t->x_loc);
}

#ifdef ANSI
typedef void (*mergeop)(def d1, def d2, env vars, tree cxt);
#else
typedef void (*mergeop)();
#endif

/* binary_sexp -- compute a binary schema exp */
PRIVATE env binary_sexp(mergeop f, tree t, env e)
{
     env e1 = sort_env(tc_sexp(t->x_arg1, e));
     env e2 = sort_env(tc_sexp(t->x_arg2, e));
     env ee = new_env(e);
     def d1, d2;

     d1 = pop_def(e1);
     d2 = pop_def(e2);
     while (d1 != NULL || d2 != NULL) {
	  int c = (d1 == NULL ? 1 : d2 == NULL ? -1 
		   : my_compare(&d1, &d2));
	  if (c < 0) {
	       (*f)(d1, (def) NULL, ee, t);
	       d1 = pop_def(e1);
	  }
	  else if (c > 0) {
	       (*f)((def) NULL, d2, ee, t);
	       d2 = pop_def(e2);
	  }
	  else {
	       (*f)(d1, d2, ee, t);
	       d1 = pop_def(e1);
	       d2 = pop_def(e2);
	  }
     }
     return ee;
}

PRIVATE void and_fun(def d1, def d2, env v, tree cxt)
{
     if (d1 == NULL)
	  push_def(d2, v);
     else if (d2 == NULL)
	  push_def(d1, v);
     else {
	  (void) check_types(d1->d_type, d2->d_type, d1->d_name,
			     cxt, cxt->x_loc);
	  push_def(d1, v);
     }
}

PRIVATE void or_fun(def d1, def d2, env v, tree cxt)
{
     if (d1 == NULL) {
	  d2->d_type = super_expand(d2->d_type, arid);
	  push_def(d2, v);
     }
     else if (d2 == NULL) {
	  d1->d_type = super_expand(d1->d_type, arid);
	  push_def(d1, v);
     }
     else {
	  if (check_types(d1->d_type, d2->d_type, d1->d_name, 
			  cxt, cxt->x_loc))
	       d1->d_type = type_union(d1->d_type, arid, d2->d_type, arid);
	  push_def(d1, v);
     }
}

PRIVATE void implies_fun(def d1, def d2, env v, tree cxt)
{
     if (d1 == NULL) {
	  d2->d_type = super_expand(d2->d_type, arid);
	  push_def(d2, v);
     }
     else {
	  if (d2 != NULL)
	       (void) check_types(d1->d_type, d2->d_type, d1->d_name,
				  cxt, cxt->x_loc);
	  d1->d_type = super_expand(d1->d_type, arid);
	  push_def(d1, v);
     }
}

PRIVATE void project_fun(def d1, def d2, env v, tree cxt)
{
     if (d2 == NULL) return;
     if (d1 != NULL)
	  check_types(d1->d_type, d2->d_type, d1->d_name,
		      cxt, cxt->x_loc);
     push_def(d2, v);
}

/* compose -- evaluate a sequential composition or piping */
PRIVATE env compose(tree t, env e, tok ldec, tok rdec, char *kind)
{
     env e1 = tc_sexp(t->x_arg1, e);
     env e2 = tc_sexp(t->x_arg2, e);
     env ee = new_env(e);
     def q;

     /* Get vars from left arg that don't match */
     for (;;) {
	  def d = pop_def(e1);
	  if (d == NULL) 
	       break;
	  else if (d->d_name->s_decor != ldec)
	       push_def(d, ee);
	  else {
	       sym rname = mk_symbol(d->d_name->s_basename, rdec);
	       type rtype = del_var(rname, e2);
	       if (rtype == NULL)
		    push_def(d, ee);
	       else if (! unify(d->d_type, rtype)) {
		    tc_error(t->x_loc, "Type mismatch in %s", kind);
		    tc_e_etc("Expression: %z", t);
		    tc_e_etc("Type of %n in LHS: %t", d->d_name, d->d_type);
		    tc_e_etc("Type of %n in RHS: %t", rname, rtype);
		    tc_e_end();
	       }
	  }
     }

     /* Now merge the unmatched vars from the right */
     for (q = e2->e_defs; q != NULL; q = q->d_next)
	  merge_def(VAR, q->d_name, q->d_type, ee, t, t->x_loc);

     return ee;
}

/* precond -- pre operator */
PRIVATE env precond(tree t, env e)
{
     env e1 = tc_sexp(t->x_arg, e);
     env e2 = new_env(e);
     def d;
     tok dec;

     for (;;) {
	  d = pop_def(e1);
	  if (d == NULL) break;
	  dec = d->d_name->s_decor;
	  if (dec != prime && dec != pling)
	       push_def(d, e2);
     }
     return e2;
}

/* tc_sexp -- check a schema expression */
PUBLIC env tc_sexp(tree t, env e)
{
     env e1, e2;
     tree u;
     def d;

     switch (t->x_kind) {
     case TEXT:
	  return tc_schema(t->x_text, e);
	  
     case SREF:
	  e1 = new_env(e);
	  do_sref(t, e, e1);
	  return e1;

     case SNOT:
	  e1 = tc_sexp(t->x_arg, e);
	  for (d = e1->e_defs; d != NULL; d = d->d_next)
	       d->d_type = super_expand(d->d_type, arid);
	  return e1;
	  
     case SAND:	    return binary_sexp(and_fun, t, e);
     case SOR:	    return binary_sexp(or_fun, t, e);
     case SIMPLIES: case SEQUIV:
		    return binary_sexp(implies_fun, t, e);
     case PROJECT:  return binary_sexp(project_fun, t, e);

     case FATSEMI:
	  return compose(t, e, prime, empty, "sequential composition");

     case PIPE:
	  return compose(t, e, pling, query, "piping");

     case HIDE:
	  e1 = tc_sexp(t->x_arg1, e);
	  for (u = t->x_arg2; u != nil; u = cdr(u))
	       hide_var((sym) car(u), (type) NULL, e1, t, t->x_loc);
	  return e1;

     case SFORALL: case SEXISTS: case SEXISTS1:
	  e2 = tc_schema(t->x_bvar, e);
	  e1 = tc_sexp(t->x_body, e);
	  for (d = e2->e_defs; d != NULL; d = d->d_next)
	       hide_var(d->d_name, d->d_type, e1, t, t->x_loc);
	  return e1;

     case PRE:
	  return precond(t, e);

     default:
	  bad_tag("get_sexp", t->x_kind);
	  return NULL;
     }
}
