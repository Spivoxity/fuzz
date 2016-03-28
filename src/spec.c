/*
 * spec.c
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
 * $Id: spec.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
#include "symbol.h"
#include "absyn.h"
#include <string.h>

PRIVATE env tc_decl(tree t, env e);

/* tc_eqmem -- check an equality or membership predicate. */
PRIVATE void tc_eqmem(tree t, env e)
{
     type lt, rt;
     
     lt = tc_expr(t->x_lhs, e);
     rt = tc_expr(t->x_rhs, e);
     if (! unify(t->x_kind == MEMBER ? mk_power(lt) : lt, rt)) {
	  tc_error(t->x_loc, "Types do not agree in %s",
		   t->x_kind == MEMBER ? "set membership" : "equation");
	  tc_e_etc("Predicate: %z", t);
	  tc_e_etc("LHS type:  %t", lt);
	  tc_e_etc("RHS type:  %t", rt);
	  tc_e_end();
     }
}

/* tc_inrel -- check an infix relation */
PRIVATE void tc_inrel(tree t, env e)
{
     type rtype[2], ltype[2], rt;
     int i;

     ltype[0] = tc_expr(t->x_rand1, e);
     ltype[1] = tc_expr(t->x_rand2, e);

     if (! anal_rel_type(rt = ref_type((sym) t->x_op, nil, e, t),
			 &rtype[0], &rtype[1], t->x_op)) {
	  tc_error(t->x_loc,
		   "Infix relation symbol %n is not a binary relation",
		   t->x_op);
	  tc_e_etc("Predicate: %z", t);
	  tc_e_etc("%n type: %t", t->x_op, rt);
	  tc_e_end();
	  return;
     }

     for (i = 0; i < 2; i++) {
	  if (! unify(ltype[i], rtype[i])) {
	       tc_error(t->x_loc,
			"Type mismatch in %s argument of infix relation",
			i == 0 ? "left" : "right");
	       tc_e_etc("Predicate: %z", t);
	       tc_e_etc("Arg type:  %t", ltype[i]);
	       tc_e_etc("Expected:  %t", rtype[i]);
	       tc_e_end();
	  }
     }
}

/* tc_prerel -- check a prefix relation */
PRIVATE void tc_prerel(tree t, env e)
{
     type lt, rt, rbase;

     lt = tc_expr(t->x_rand, e);
     
     if (! anal_power(rt = ref_type((sym) t->x_op, nil, e, t),
		      &rbase, t->x_op)) {
	  tc_error(t->x_loc,
		   "Prefix relation symbol %n is not a set", 
		   t->x_op);
	  tc_e_etc("Predicate: %z", t);
	  tc_e_etc("%n type: %t", t->x_op, rt);
	  tc_e_end();
     }

     if (! unify(lt, rbase)) {
	  tc_error(t->x_loc,
		   "Argument of prefix relation has wrong type");
	  tc_e_etc("Predicate: %z", t);
	  tc_e_etc("Arg type:  %t", lt);
	  tc_e_etc("Expected:  %t", rbase);
	  tc_e_end();
     }
}

/* tc_pred -- check a predicate */
PUBLIC void tc_pred(tree t, env e)
{
     switch (t->x_kind) {
     case EQUAL:
     case MEMBER:
	  tc_eqmem(t, e);
	  break;

     case INREL:
	  tc_inrel(t, e);
	  break;

     case PREREL:
	  tc_prerel(t, e);
	  break;
	  
     case TRUTH:
     case FALSITY:
	  break;

     case NOT:
	  tc_pred(t->x_arg, e);
	  break;
	  
     case AND:
     case OR: 
     case EQUIV: 
     case IMPLIES:
	  tc_pred(t->x_arg1, e);
	  tc_pred(t->x_arg2, e);
	  break;
	  
     case EXISTS:
     case EXISTS1:
     case FORALL:
	  tc_pred(t->x_body, tc_schema(t->x_bvar, e));
	  break;

     case LETPRED:
	  tc_pred(t->x_body, tc_letdefs(t->x_defs, e));
	  break;

     case SPRED: {
	  env s;
	  def d;

	  s = tc_sexp(t->x_ref, e);
	  for (d = s->e_defs; d != NULL; d = d->d_next) {
	       type tt = ref_type(d->d_name, nil, e, t);
	       if (unify(tt, d->d_type)) continue;
	       tc_error(t->x_loc,
			"Component %n has wrong type in schema reference",
			d->d_name);
	       tc_e_etc("Predicate:  %z", t);
	       tc_e_etc("Found type: %t", tt);
	       tc_e_etc("Expected:   %t", d->d_type);
	       tc_e_end();
	  }
	  break;
     }

     default:
	  bad_tag("tc_pred", t->x_kind);
     }
}

/* tc_given -- declare a list of given set names. */
PUBLIC env tc_given(tree t, env e, int loc)
{
     tree u;
     env e1 = new_env(e);
     for (u = t; u != nil; u = cdr(u))
	  merge_def(GSET, (sym) car(u), (type) NULL, e1, nil, loc);
     return e1;
}

/* rhs_type -- check an exp has a set type and return the base type. */
PUBLIC type rhs_type(tree t, env e, char *kind)
{
     type tt1, tt2;
     if (! anal_power(tt1 = tc_expr(t, e), &tt2, t)) {
	  tc_error(t->x_loc, "Set-valued expression required in %s", kind);
	  tc_e_etc("Expression: %z", t);
	  tc_e_etc("Type:       %t", tt1);
	  tc_e_end();
     }
     return tt2;
}

/* op_check -- warn if infix symbol (etc.) declared with odd type. */
PRIVATE void op_check(sym x, type t, int loc)
{
     char *kind, *needed;
     bool ok;

     switch(lexval(x)) {
     case PREREL:
	  kind = "prefix relation symbol";
	  needed = "set";
	  ok = (type_kind(t) == POWERT);
	  break;

     case INREL:
	  kind = "infix relation symbol";
	  needed = "binary relation";
	  ok = is_fun_type(t,1);
	  break;

     case POSTOP:
	  kind = "postfix function symbol";
	  needed = "function";
	  ok = is_fun_type(t,1);
	  break;

     case INOP:
	  kind = "infix function symbol";
	  needed = "binary function";
	  ok = is_fun_type(t,2);
	  break;

     default:
	  ok = TRUE;
     }

     if (! ok) {
	  tc_warn(loc, "%s %n is not declared as a %s", kind, x, needed);
	  tc_e_etc("%n type: %t", x, t);
	  tc_e_end();
     }
}
			
/* tc_decl -- check a declaration, returning the env it creates. */
PRIVATE env tc_decl(tree t, env e)
{
     tree u;
     env e1 = new_env(e);

     for (u = t; u != nil; u = cdr(u))
	  switch (car(u)->x_kind) {
	  case DECL: {
	       tree v;
	       type tt;

	       tt = rhs_type(car(u)->x_decl_expr, e, "declaration");
	       for (v = car(u)->x_decl_names; v != nil; v = cdr(v)) {
		    op_check((sym) car(v), tt, car(u)->x_loc);
		    merge_def(VAR, (sym) car(v), tt, e1,
			      nil, car(u)->x_loc);
	       }
	       break;
	  }

	  case SDECL:
	       do_sref(car(u)->x_ref, e, e1);
	       break;

	  default:
	       bad_tag("tc_decl", car(u)->x_kind);
	  }
	       
     return e1;
}
	       
/* tc_schema -- check a schema text and return the env it creates. */
PUBLIC env tc_schema(tree t, env e)
{
     tree u;
     env e1 = tc_decl(t->x_decls, e);
     for (u = t->x_axioms; u != nil; u = cdr(u))
	  tc_pred(car(u), e1);
     return e1;
}

#define schema_decl(x, d, p) \
     node(1, 0, SDECL, sref((tree) x, (tree) d, p, nil, 0))
#define theta(x, d, r) \
     node(3, 0, THETA, (tree) x, (tree) d, r)

PRIVATE tree dummy_var(int i)
{
     static char buf[10];
     (void) sprintf(buf, "*%d*", i);
     return (tree) mk_symbol(enter(buf, WORD), empty);
}

PRIVATE def delta_def(tok dx)
{
     tree para, formals = nil, actuals = nil;
     tok x = dx->l_root;
     def d = global_def(mk_symbol(x, empty));
     int i;

     if (d == NULL || d->d_kind != SCHEMA)
	  return NULL;

     for (i = d->d_nparams; i > 0; i--) {
	  tree ii = dummy_var(i);
	  formals = cons(ii, formals);
	  actuals = cons(simply(ii,0), actuals);
     }

     para = node(2, 0, SDEF,
		 node(2, 0, SHEAD, dx, formals),
		 node(2, 0, BODY,
		      list2(schema_decl(x, empty, actuals),
			    schema_decl(x, prime, actuals)),
		      (dx->l_prefix == Delta ? nil :
		       list1(node(2, 0, EQUAL,
				  theta(x, empty, nil),
				  theta(x, prime, nil))))));

     tc_para(para);

     return global_def(mk_symbol(dx, empty));
}

/* get_schema -- find the definition of a schema */
PUBLIC def get_schema(tok x, int loc)
{
     sym xx = mk_symbol(x, empty);
     def d = global_def(xx);

     if (d == NULL && x->l_prefix != NULL)
	  d = delta_def(x);

     if (d == NULL) {
	  tc_error(loc, "Schema %n is not defined", xx);
	  tc_e_end();
	  return NULL;
     }
     else if (d->d_kind != SCHEMA) {
	  tc_error(loc, "Name %n is not a schema", xx);
	  tc_e_end();
	  return NULL;
     }

     return d;
}

/* get_params -- analyse parameters of a schema ref or generic constant. */
PUBLIC void get_params(char *kind, sym x, tree actual, 
		       env e, frame formal, int loc)
{
     tree u;
     int i, n = fsize(formal);

     if (list_len(actual) != n) {
	  tc_error(loc, "%s %n expects %d parameters", kind, x, n);
	  tc_e_end();
	  for (i = 0; i < n; i++)
	       formal->f_var[i] = err_type;
	  return;
     }

     i = 0;
     for (u = actual; u != nil; u = cdr(u))
	  formal->f_var[i++] =
	       rhs_type(car(u), e, "actual generic parameter");
}     

/* tc_data -- check a free type definition */
PRIVATE void tc_data(tree t)
{
     sym obj = (sym) t->x_lhs;
     def obj_def = obj->s_glodef;
     type ran, dom, tt;
     tree u;
     env e1 = new_env(global);

     if (obj_def != NULL && obj_def->d_kind == GSET) {
	  tc_warn(t->x_loc, "%n already declared as a basic type", obj);
	  tc_e_end();
     }
     else
	  obj_def = def_gset(obj, t->x_loc);
     
     ran = gset_type(obj_def);

     begin_frame();
     for (u = t->x_rhs; u != nil; u = cdr(u)) {
	  if (! exists(car(u)->x_template))
	       tt = ran;
	  else {
	       dom = rhs_type(the(car(u)->x_template), global,
			      "free type definition");
	       tt = (aflag ? rel_type(dom, ran) : mk_pfun(dom, ran));
	  }
          op_check((sym) car(u)->x_tag, tt, car(u)->x_loc);
	  merge_def(DATACONS, (sym) car(u)->x_tag, tt, e1,
		    nil, car(u)->x_loc);
     }
     end_frame();
     install_defs(e1, t->x_loc);
}

/* tc_body -- check a top-level schema body (cf. tc_schema) */
PRIVATE env tc_body(tree t, env e)
{
     tree u;
     env e1;

     begin_frame();
     e1 = tc_decl(t->x_decls, e);
          
     for (u = t->x_axioms; u != nil; u = cdr(u)) {
#ifdef ASSUME
	  begin_assume();
#endif
	  tc_pred(car(u), e1);
#ifdef ASSUME
	  end_assume(car(u));
#endif
     }
     end_frame();

     return e1;
}


/* tc_letdefs -- process the def list of a let construct */
PUBLIC env tc_letdefs(tree t, env e)
{
     tree u;
     env e1 = new_env(e);

     for (u = t; u != nil; u = cdr(u)) {
	  type tt = tc_expr(car(u)->x_rhs, e);
	  op_check((sym) car(u)->x_lhs, tt, car(u)->x_loc);
	  merge_def(LETVAR, (sym) car(u)->x_lhs, tt, e1,
		    nil, car(u)->x_loc);
     }
     return e1;
}

/* make_attr -- mark list of symbols as type abbrevs or tame functions */
PRIVATE void make_attr(tree args, int attr, int loc)
{
     tree t;

     for (t = args; t != nil; t = cdr(t)) {
	  sym x = mk_symbol((tok) car(t), empty);
	  def d = global_def(x);
 
	  if (d == NULL) {
	       tc_error(loc, "%s %n has no global definition",
		   (attr == XABBREV ? "Type abbreviation" : "Tame function"),
		   x);
	       tc_e_end();
	       return;
	  }

	  switch (attr) {
	  case XABBREV:
	       if (d->d_kind != SCHEMA && type_kind(d->d_type) != POWERT) {
		    tc_error(loc, "Type abbreviation %n is not a set", x);
		    tc_e_etc("%n type: %t", x, d->d_type);
		    tc_e_end();
		    return;
	       }
	       d->d_abbrev = TRUE;
	       break;

	  case XTAME:
	       if (d->d_kind != GENCONST || ! is_fun_type(d->d_type, 1)) {
		    tc_error(loc,
			     "Tame function %n is not a generic function", x);
		    if (d->d_kind == GENCONST)
			 tc_e_etc("%n type: %t", x, d->d_type);
		    tc_e_end();
		    return;
	       }
	       d->d_tame = TRUE;
	       break;
	  }
     }
}

/* real_tc_para -- check a paragraph */	     
PRIVATE void real_tc_para(tree t)
{
     switch (t->x_kind) {
     case GIVEN:
	  install_defs(tc_given(t->x_given, global, t->x_loc), t->x_loc);
	  break;

     case AXDEF:
	  install_defs(tc_body(t->x_axdef, global), t->x_loc);
	  break;

     case SDEF:
	  (void) def_schema((tok) t->x_heading->x_tag,
			    tc_body(t->x_schema_body,
				    tc_given(t->x_heading->x_params,
					     global, t->x_heading->x_loc)),
			    t->x_loc);
	  break;
	  
     case DEFEQ: 
	  {
	       env e = tc_given(t->x_lhs->x_params, global, t->x_lhs->x_loc);
	       env s;
	       begin_frame();
	       s = tc_sexp(t->x_rhs, e);
	       end_frame();
	       (void) def_schema((tok) t->x_lhs->x_tag, s, t->x_loc);
	       break;
	  }

     case PRED:
	  begin_frame();
	  tc_pred(t->x_pred, global);
	  end_frame();
	  break;

     case DEFINE:
	  install_genconsts(tc_body(t->x_def_body,
				    tc_given(t->x_def_params, 
					     global, t->x_loc)),
			    t->x_loc);
	  break;
	  
     case EQEQ: 
	  {
	       env e = tc_given(t->x_lhs->x_params, global, t->x_lhs->x_loc);
	       type tt;
	       begin_frame();
	       tt = tc_expr(t->x_rhs, e);
	       end_frame();
	       op_check((sym) t->x_lhs->x_tag, tt, t->x_loc);
	       (void) def_genconst((sym) t->x_lhs->x_tag, e, tt, t->x_loc);
	       break;
	  }

     case DATA:
	  tc_data(t);
	  break;

     case VDASH: 
	  tc_pred(t->x_rhs, tc_sexp(t->x_lhs, global));
	  break;
	  
     case XTAME:
     case XABBREV:
	  make_attr(t->x_args, t->x_kind, t->x_loc);
	  break;

     default:
	  bad_tag("tc_para", t->x_kind);
     }
}

PUBLIC void tc_para(tree t)
{
     if (debug('l')) {
	  /* dump before processing (for debugging) */
	  dump_tree(t);
	  printf("\n");
     }

     real_tc_para(t);

     if (lflag && debugging) {
	  /* dump after processing (to get dependency order) */
	  dump_tree(t);
	  printf("\n");
     }
}
