/*
 * expr.c
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
 * $Id: expr.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
#include "symbol.h"
#include "absyn.h"

PRIVATE tree char_tuple(tree t);
#ifdef ASSUME
PRIVATE type assume_type(sym x);
#endif

/* ref_type -- find type of a reference */
PUBLIC type ref_type(sym x, tree p, env e, tree cxt)
{
     def d = find_def(x, e);
     frame f;

     if (d == NULL) {
#ifdef ASSUME
	  type t;
	  if (qflag && p == nil && (t = assume_type(x)) != NULL)
	       return t;
#endif

	  if (! partial_env(e)) {
	       tc_error(cxt->x_loc, "Identifier %n is not declared", x);
	       if (cxt->x_kind != REF)
		    tc_e_etc("Expression: %z", cxt);
	       tc_e_end();
	  }

	  return err_type;
     }

     f = new_frame(d->d_nparams, cxt);
     
     if (p != nil)
	  switch (d->d_kind) {
	  case GSET:
	  case VAR:
	       tc_error(cxt->x_loc, "%s %n cannot have parameters",
			d->d_kind == GSET ? "Basic type" : "Variable", x);
	       tc_e_etc("Expression: %z", cxt);
	       tc_e_end();
	       return err_type;

	  case GENCONST:
	       get_params("Generic constant", x, p, e, f, cxt->x_loc);
	       break;

	  default:
	       bad_tag("ref_type", d->d_kind);
	  }
	   
     if (! aflag && d->d_abbrev)
	  return mk_power(mk_abbrev(d, (p != nil ? f : alias(f))));
     else
	  return seal(d->d_type, f);
}

/* tc_apply -- check a function application */
PRIVATE type tc_apply(int kind, tree t, tree fun, tree arg, env e)
{
     type actual = tc_expr(arg, e);
     type fun_type, formal, result;
     type formarg[MAX_ARGS], actarg[MAX_ARGS];
     int n_formals, n_actuals, i;
     frame param = arid;
     def d;
	            
     if (! aflag && fun->x_kind == REF 
	 && (d = find_def((sym) fun->x_tag, e)) != NULL
	 && d->d_tame && fun->x_params == nil)
	  /* A tame function */
	  fun_type = seal(d->d_type,
			  param = new_frame(d->d_nparams, fun));
     else
	  fun_type = tc_expr(fun, e);

     if (! anal_rel_type(fun_type, &formal, &result, fun)) {
	  if (fun_type != err_type) {
	       if (kind == APPLY)
		    tc_error(t->x_loc, "Application of a non-function");
	       else
		    tc_error(t->x_loc, "%s operator %n is not a function",
			     (kind == INOP ? "Infix" : "Postfix"),
			     fun->x_tag);
	       tc_e_etc("Expression: %z", t);
	       tc_e_etc("Found type: %t", fun_type);
	       tc_e_end();
	  }
	  mark_error();
	  return err_type;
     }

     if (arg->x_kind == TUPLE
	 && anal_cproduct(formal, formarg, &n_formals)) {
	  /* Special case: actual parameter is a tuple, and formal
	     parameter type is a cproduct. Check args one by one. */
	  if (! anal_cproduct(actual, actarg, &n_actuals))
	       panic("tc_apply");
	  if (n_actuals != n_formals) {
	       tc_error(t->x_loc, "Function expects %d arguments",
			n_formals);
	       tc_e_etc("Expression: %z", t);
	       tc_e_end();
	       mark_error();
	       return err_type;
	  }
	  for (i = 0; i < n_actuals; i++) {
	       if (param_unify(actarg[i], formarg[i], param))
		    continue;

	       if (kind == INOP)
		    tc_error(t->x_loc,
			     "%s argument of operator %n has wrong type",
			     (i == 0 ? "Left" : "Right"), fun->x_tag);
	       else
		    tc_error(t->x_loc, "Argument %d has wrong type", i+1);
	       tc_e_etc("Expression: %z", t);
	       tc_e_etc("Arg type:   %t", actarg[i]);
	       tc_e_etc("Expected:   %t", formarg[i]);
	       tc_e_end();
	  }
     }
     else if (! param_unify(actual, formal, param)) {
	  /* General case: check the single arg as a whole. */
	  tc_error(t->x_loc, "Argument of %s has wrong type",
		   (kind == POSTOP ? "postfix operator" : "application"));
	  tc_e_etc("Expression: %z", t);
	  tc_e_etc("Arg type:   %t", actual);
	  tc_e_etc("Expected:   %t", formal);
	  tc_e_end();
     }

     return result;
}

/* theta_type -- compute type of a theta-exp or theta-select */
PRIVATE type theta_type(tree t, env e, type a, tree cxt)
{
     def d = get_schema((tok) t->x_the_name, t->x_loc);
     schema s;
     env e1 = new_env(e);
     type b;
     int i;

     if (d == NULL)
	  return err_type;

     s = d->d_schema;
     check_rename(s, (tok) t->x_the_decor, t->x_the_rename, t);

     for (i = 0; i < s->z_ncomps; i++) {
	  sym x = s->z_comp[i].z_name;
	  sym xp = get_rename(x, (tok) t->x_the_decor, t->x_the_rename);
	  type tt = (a == NULL
		     ? ref_type(xp, nil, e, t)
		     : comp_type(a, xp, cxt, t->x_loc));
	  add_def(VAR, x, tt, e1);
     }

     b = mk_sproduct(mk_schema(e1));
     if (! aflag && d->d_abbrev && d->d_nparams == 0
	 && type_equal(b, arid, mk_sproduct(s), arid))
	  return mk_abbrev(d, arid);
     else
	  return b;
}

/* tc_expr -- check an expression and return its type */
#ifdef DEBUG
PRIVATE type the_real_tc_expr(tree t, env e)
#else
PUBLIC type tc_expr(tree t, env e)
#endif
{
     switch (t->x_kind) {
     case REF:
	  return ref_type((sym) t->x_tag, t->x_params, e, t);
	  
     case INGEN:
	  return ref_type((sym) t->x_tag,
			  list2(t->x_param1, t->x_param2), e, t);

     case PREGEN:
	  return ref_type((sym) t->x_tag, list1(t->x_param), e, t);

     case NUMBER:
	  return nat_type;
	  
     case SEXPR: {
	  def d;
	  frame params;

	  if (! open_sref(t->x_ref, e, &d, &params))
	       return err_type;

	  if ((tok) t->x_ref->x_sref_decor != empty) {
	       tc_error(t->x_loc, "Decoration ignored in schema reference");
	       tc_e_etc("Expression: %z", t);
	       tc_e_end();
	  }

	  if (t->x_ref->x_sref_renames != nil) {
	       tc_error(t->x_loc, "Renaming ignored in schema reference");
	       tc_e_etc("Expression: %z", t);
	       tc_e_end();
	  }

	  if (! aflag && d->d_abbrev)
	       return mk_power(mk_abbrev(d, params));
	  else
	       return mk_power(seal(mk_sproduct(d->d_schema), params));
     }

     case POWER: {
	  type tt1, tt2;	  
	  if (! anal_power(tt1 = tc_expr(t->x_arg, e), &tt2, t->x_arg)) {
	       tc_error(t->x_loc, "Argument of \\power must be a set");
	       tc_e_etc("Expression: %z", t);
	       tc_e_etc("Arg type:   %t", tt1);
	       tc_e_end();
	  }
	  return mk_power(mk_power(tt2));
     }
	   
     case TUPLE : {
	  type a[MAX_ARGS];
	  int n = 0;
	  tree u;
	       
	  for (u = t->x_elements; u != nil; u = cdr(u)) {
	       if (n >= MAX_ARGS)
		    panic("tc_expr - tuple too big");
	       a[n++] = tc_expr(car(u), e);
	  }
	  return mk_cproduct(n, a);
     }

     case CROSS: {
	  type a[MAX_ARGS];
	  type tt1, tt2;
	  int n = 0;
	  tree u;

	  for (u = t->x_factors; u != nil; u = cdr(u)) {
	       if (n >= MAX_ARGS)
		    panic("tc_expr - product too big");
	       tt1 = tc_expr(car(u), e);
	       if (! anal_power(tt1, &tt2, car(u))) {
		    tc_error(t->x_loc,
			     "Argument %d of \\cross must be a set", n+1);
		    tc_e_etc("Expression: %z", t);
		    tc_e_etc("Arg %d type: %t", n+1, tt1);
		    tc_e_end();
	       }
	       a[n++] = tt2;
	  }
	  return mk_power(mk_cproduct(n, a));
     }

     case EXT:
     case SEQ:
     case BAG: {
	  type elem_type;
	  type tt;
	  tree u;

	  if (t->x_elements == nil)
	       elem_type = new_typevar(t);
	  else {
	       elem_type = tc_expr(car(t->x_elements), e);
	       for (u = cdr(t->x_elements); u != nil; u = cdr(u)) {
		    if (unify(elem_type, tt = tc_expr(car(u), e)))
			 elem_type = type_union(elem_type, arid, tt, arid);
		    else {
			 tc_error(t->x_loc, "Type mismatch in %s display",
				  (t->x_kind == EXT ? "set" :
				   t->x_kind == SEQ ? "sequence" : "bag"));
			 tc_e_etc("Expression: %z", car(u));
			 tc_e_etc("Has type:   %t", tt);
			 tc_e_etc("Expected:   %t", elem_type);
			 tc_e_end();
		    }
	       }
	  }
	  switch (t->x_kind) {
	  case EXT:
	       return mk_power(elem_type);
	  case SEQ:
	       return (aflag ? rel_type(num_type, elem_type) 
			     : mk_seq(elem_type));
	  case BAG:
	       return (aflag ? rel_type(elem_type, num_type) 
			     : mk_bag(elem_type));
	  }
     }

     case THETA: 
	  return theta_type(t, e, (type) NULL, t);

     case BINDING: {
	  tree u;
	  env e1 = new_env(e);
	  for (u = t->x_elements; u != nil; u = cdr(u))
	       add_def(VAR, (sym) car(u)->x_lhs, 
		       tc_expr(car(u)->x_rhs, e), e1);
	  return mk_sproduct(mk_schema(e1));
     }

     case SELECT: {
	  type a = tc_expr(t->x_arg, e);

	  if (type_kind(a) != SPRODUCT) {
	       tc_error(t->x_loc,
			"Argument of selection must have schema type");
	       tc_e_etc("Expression: %z", t);
	       tc_e_etc("Arg type:   %t", a);
	       tc_e_end();
	       mark_error();
	       return err_type;
	  }

	  switch (t->x_field->x_kind) {
	  case IDENT:
	       return (comp_type(a, (sym) t->x_field, t, t->x_loc));

	  case THETA:
	       return (theta_type(t->x_field, e, a, t));

	  default:
	       bad_tag("tc_expr.SELECT", t->x_field->x_kind);
	       return (type) NULL;
	  }
     }

     case APPLY:
	  return tc_apply(APPLY, t, t->x_arg1, t->x_arg2, e);

     case INOP:
	  return tc_apply(INOP, t, simply(t->x_op, t->x_loc), 
			  pair(t->x_rand1, t->x_rand2), e);

     case POSTOP:
	  return tc_apply(POSTOP, t, simply(t->x_op, t->x_loc), 
			  t->x_rand, e);

     case LAMBDA: {
	  env e1 = tc_schema(t->x_bvar, e);
	  type dom = tc_expr(char_tuple(t->x_bvar), e1);
	  type ran = tc_expr(t->x_body, e1);
	  return (aflag ? rel_type(dom, ran) : mk_pfun(dom, ran));
     }
    
     case COMP:
     case MU: {
	  env e1 = tc_schema(t->x_bvar, e);
	  type a = tc_expr(exists(t->x_body) ? the(t->x_body) :
			   char_tuple(t->x_bvar), e1);
	  return (t->x_kind == COMP ? mk_power(a) : a);
     }

     case LETEXPR:
	  return tc_expr(t->x_body, tc_letdefs(t->x_defs, e));

     case IF: {
	  type a, b;
	  tc_pred(t->x_if, e);
	  a = tc_expr(t->x_then, e);
	  b = tc_expr(t->x_else, e);
	  if (unify(a, b))
	       return type_union(a, arid, b, arid);
	  else {
	       tc_error(t->x_loc,
			"Type mismatch in conditional expression");
	       tc_e_etc("Expression: %z", t);
	       tc_e_etc("Then type:  %t", a);
	       tc_e_etc("Else type:  %t", b);
	       tc_e_end();
	       return err_type;
	  }
     }

     default:
	  bad_tag("tc_expr", t->x_kind);
	  /* dummy */ return (type) NULL;
     }
}

/* char_tuple -- find characteristic tuple of a declaration */
PRIVATE tree char_tuple(tree t)
{
     tree rep[MAX_ARGS];
     tree s, u;
     int n = 0;

     for (s = t->x_decls; s != nil; s = cdr(s)) {
	  if (n >= MAX_ARGS)
	       panic("char_tuple - too many representatives");
	  switch (car(s)->x_kind) {
	  case DECL:
	       for (u = car(s)->x_decl_names; u != nil; u = cdr(u))
		    rep[n++] = simply(car(u), car(s)->x_loc);
	       break;
	  case SDECL:
	       rep[n++] = node(3, car(s)->x_loc, THETA, 
			       car(s)->x_ref->x_sref_tag,
			       car(s)->x_ref->x_sref_decor, 
			       car(s)->x_ref->x_sref_renames);
	       break;
	  }
     }
     if (n == 0) {
	  panic("char_tuple");
	  return (tree) NULL;
     } else if (n == 1)
	  return rep[0];
     else {
	  s = nil;
	  for (n--; n >= 0; n--)
	       s = cons(rep[n], s);
	  return node(1, t->x_loc, TUPLE, s);
     }
}

#ifdef DEBUG
PRIVATE int level = 0;

PUBLIC type tc_expr(tree t, env e)
{
     type result;

     level++;
     result = the_real_tc_expr(t, e);
     level--;
     if (debug('e'))
	  grind(stdout, "Expr: %j%z ==> %t\n", level, t, result);
     return result;
}
#endif
     
#ifdef ASSUME
PRIVATE bool assume = FALSE;
PRIVATE int n_assumes;

PRIVATE struct a_rec {
     sym a_name;
     type a_type;
     int a_refs;
} *a_table;

PUBLIC void begin_assume(void)
{
     assume = TRUE;
     n_assumes = 0;
     if (a_table == NULL)
          a_table = (struct a_rec *)
               alloc_mem(VAR_SIZE, sizeof(struct a_rec), "begin_assume");
}
     
PUBLIC void end_assume(tree cxt)
{
     int i;
     
     assume = FALSE;

     for (i = 0; i < n_assumes; i++)
	  if (a_table[i].a_refs == 1) {
	       tc_warn(cxt->x_loc,
		       "implicitly quantified name %n appears only once",
		       a_table[i].a_name);
	       tc_e_etc("Predicate: %z", cxt);
	       tc_e_end();
	  }
}

PRIVATE type assume_type(sym x)
{
     int i;

     if (! assume)
	  return NULL;

     for (i = 0; i < n_assumes; i++)
	  if (a_table[i].a_name == x) {
	       a_table[i].a_refs++;
	       return a_table[i].a_type;
	  }

     if (n_assumes >= VAR_SIZE)
	  panic("assume_type -- Too many assumes");

     a_table[n_assumes].a_name = x;
     a_table[n_assumes].a_refs = 1;
     return a_table[n_assumes++].a_type = new_typevar((tree) x);
}
#endif
