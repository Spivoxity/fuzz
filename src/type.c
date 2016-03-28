/*
 * type.c
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
 * $Id: type.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
#include "symbol.h"

/* Inside this module, types are represented by a pair (t, f), where t
   is a `type' and f is a `frame'. The frame is an array of types used
   as values of any type variables which appear in the type itself:
   these type variables contain offsets in the frame, and their values
   are either a type or NULL, meaning not-yet-determined. A frame may
   also be null, when there is no possibility that the type may
   contain `free' type variables. A molecule is a type bound up with a
   frame in which it should be interpreted: all types passed to other
   parts of the program are sealed up like this using seal(). An
   abbrev is used for type-abbreviations: the generic parameters are
   represented by a frame as for a molecule, but this time they are to
   be interpreted in the current frame rather than the null frame.

   Using the notation t[f] for the type represented by t in the frame
   f, and {t1 t2 ... tn} for a frame of n types, we can list the
   possibilities as follows:

   	(GIVENT x)[f]			= GIVENT x
	(TYPEVAR i)[f]			= f[i]
	(POWERT t')[f]			= POWERT (t'[f])
	(CPRODUCT t1 t2 ... tn)[f]	= CPRODUCT t1[f] t2[f] ... tn[f]
	(SPRODUCT x1:t1 ... xn:tn)[f]   = SPRODUCT x1:t1[f] ... xn:tn[f]
	(MOLECULE t' f')[f]		= t'[f']
	(ABBREV d {t1 t2 ... tn})[f]	= type(d)[{t1[f] t2[f] ... tn[f]}]

   When a generic definition is installed, lambda_type() is used to
   replace the formal generic parameters in its type with type
   variables. As a side product, lambda_type() copies the type into
   permanent storage: because of this, it is used for ordinary
   variables too, with NULL as the bound variable list. As an 
   optimization, lambda_type() which keeps a memo-table
   of types whose abstractions have already been computed.
*/

/* The array typevar contains the only TYPEVAR nodes: typevar[i] is the
   one with index i. Unify relies on this uniqueness of representation. */
PRIVATE type typevar[MAX_ARGS];

/* var_rep -- lambda-abstraction of a given set name */
PRIVATE type var_rep(env bv, type v)
{
     def x = v->t_given, d;
     int i;

     if (bv != NULL)
	  for (d = bv->e_defs, i = 0; d != NULL; d = d->d_next, i++)
	       if (d == x)
		    /* lambda bv . bv[i] -- return var i. */
		    return typevar[i];

     /* lambda bv . y -- return y */
     return v;
}

PRIVATE type lambda_ty(env bv, type t, frame f)
{
#ifdef DEBUG
     if (debug('t'))
          grind(stdout, "lambda_ty(%x)\n", t);
#endif
     if (t == NULL) panic("lambda_ty");

     if (f == arid && is_perm((univ) t)) {
	  /* It's lambda bv . t[NULL] where t is already permanent.
	     This means that t contains no bound vars: return t */
#ifdef DEBUG
	  if (debug('t'))
	       grind(stdout, "Reused type %t\n", t);
#endif
	  return t;
     }

     switch (t->t_kind) {
     case GIVENT:
	  /* lambda bv . v[f] -- look for v among the bv's */
	  return var_rep(bv, t);

     case TYPEVAR:
	  /* lambda bv . Vi[f] = lambda bv. fi[NULL] */
	  return lambda_ty(bv, tv_val(t, f), arid);

     case POWERT:
	  /* lambda bv . (P t)[f] = P (lambda bv . t[f]) */
	  return mk_power(lambda_ty(bv, t->t_base, f));

     case CPRODUCT: {
	  /* lambda bv . (t1 x ... x tn)[f]
	       = (lambda bv . t1[f]) x ... x (lambda bv . tn[f]) */
	  type a[MAX_ARGS];
	  int i;

	  if (t->t_nfields > MAX_ARGS)
	       panic("lambda_ty - too many fields");
	  for (i = 0; i < t->t_nfields; i++)
	       a[i] = lambda_ty(bv, t->t_field[i], f);
	  return mk_cproduct(t->t_nfields, a);
     }

     case SPRODUCT: {
	  /* lambda bv . <| x1: t1; ... |>[f]
	       = <| x1: lambda bv. t1[f]; ... |> */
	  schema s = t->t_schema;
	  int n = s->z_ncomps;
	  schema s1 = alloc_schema(n);
	  int i;

	  for (i = 0; i < n; i++) {
	       s1->z_comp[i].z_name = s->z_comp[i].z_name;
	       s1->z_comp[i].z_type
		    = lambda_ty(bv, s->z_comp[i].z_type, f);
	  }
	  return mk_sproduct(s1);
     }	       

     case MOLECULE:
	  /* lambda bv . (MOLECULE t f')[f] = lambda bv . t[f'] */
	  return lambda_ty(bv, t->t_mtype, t->t_mframe);

     case ABBREV: {
	  /* lambda bv . (ABBREV d {t1 t2 ... tn})[f]
	       = ABBREV d {(lambda bv . t1[f]) ... } */
	  frame p = t->t_params;
	  frame pp = mk_frame(fsize(p));
	  int i;

	  for (i = 0; i < fsize(p); i++)
	       pp->f_var[i] = lambda_ty(bv, p->f_var[i], f);
	  return mk_abbrev(t->t_def, pp);
     }

     default:
	  bad_tag("lambda_ty", t->t_kind);
	  return (type) NULL;
     }
}

PRIVATE struct t_rec {
     type conc, abs;
} *type_cache;

PRIVATE int n_cached;

PUBLIC void clear_cache(void)
{
     n_cached = 0;
}

/* lambda_type -- abstract over a list of given set names in a type */
PUBLIC type lambda_type(env bv, type t)
{
     type at;
     int i;

     for (i = 0; i < n_cached; i++)
	  if (type_cache[i].conc == t) {
#ifdef DEBUG
	       if (debug('t'))
		    grind(stdout, "Cache hit for type %t at %x\n", t, t);
#endif
	       return type_cache[i].abs;
	  }

     if (bv != NULL && bv->e_ndefs > MAX_ARGS)
	  panic("lambda_type - too many type variables");
     at = lambda_ty(bv, t, arid);
     if (n_cached < VAR_SIZE) {
	  type_cache[n_cached].conc = t;
	  type_cache[n_cached].abs = at;
	  n_cached++;
#ifdef DEBUG
	  if (debug('t'))
	       grind(stdout, "Cached type %t at %x\n", t, t);
#endif	  
     }
     return at;
}
     
/* expand -- expand a type abbreviation */
PUBLIC type expand(type t, frame f)
{
     def d = t->t_def;
     frame p = t->t_params;
     frame pp;
     int i;

     if (f == arid) {
	  /* Special case: parameters are already absolute */
	  pp = p;
     } else {
	  pp = mk_frame(d->d_nparams);
	  for (i = 0; i < d->d_nparams; i++)
	       pp->f_var[i] = seal(p->f_var[i], f);
     }

     if (d->d_kind == SCHEMA)
	  return seal(mk_sproduct(d->d_schema), pp);
     else {
	  type base;
	  if (! anal_power(seal(d->d_type, pp), &base, nil))
	       panic("expand");
	  return base;
     }
}

#define alloc_type() (type) alloc(sizeof(struct type))

/* mk_givent -- make a given set type */
PUBLIC type mk_givent(def d)
{
     type tt = alloc_type();
     tt->t_kind = GIVENT;
     tt->t_given = d;
     return tt;
}

/* mk_power -- make a power set type */
PUBLIC type mk_power(type t)
{
     type tt = alloc_type();
     tt->t_kind = POWERT;
     tt->t_base = t;
     return tt;
}

/* mk_cproduct -- make a Cartesian product type from an array of types */
PUBLIC type mk_cproduct(int n, type a[])
{
     type tt = alloc_type();
     int i;

     tt->t_kind = CPRODUCT;
     tt->t_nfields = n;
     tt->t_field = (type *) alloc(n * sizeof(type));
     for (i = 0; i < n; i++)
	  tt->t_field[i] = a[i];
     return tt;
}

/* mk_sproduct -- make a schema product type from a schema. */
PUBLIC type mk_sproduct(schema s)
{
     type tt = alloc_type();
     tt->t_kind = SPRODUCT;
     tt->t_schema = s;
     return tt;
}

/* mk_molecule -- make a molecule */
PRIVATE type mk_molecule(type t, frame f)
{
     type tt = alloc_type();
     tt->t_kind = MOLECULE;
     tt->t_mtype = t;
     tt->t_mframe = f;
     return tt;
}

#define tv_ref(n, f) mk_molecule(typevar[n], f)

/* seal -- render a type independent of its frame */
PUBLIC type seal(type t, frame f)
{
     if (t == NULL)
	  panic("seal");

     if (f == NULL || t->t_kind == GIVENT || t->t_kind == MOLECULE)
	  return t;
     else
	  return mk_molecule(t,f);
}

/* alias -- make a frame of references to a frame's variables */
PUBLIC frame alias(frame f)
{
     int i, n = fsize(f);
     frame ff = mk_frame(n);

     for (i = 0; i < n; i++)
	  ff->f_var[i] = tv_ref(i, f);

     return ff;
}

/* new_typevar -- create a fresh type variable */
PUBLIC type new_typevar(tree cxt)
{
     return tv_ref(0, new_frame(1, cxt));
}

/* mk_abbrev -- make an abbreviation with params given as a frame */
PUBLIC type mk_abbrev(def d, frame p)
{
     type tt = alloc_type();
     tt->t_kind = ABBREV;
     tt->t_def = d;
     tt->t_params = p;
     return tt;
}

/* mk_xabbrev -- make an abbreviation with an explicit list of params */
PUBLIC type mk_xabbrev(def d, int n, ...)
{
     frame f = mk_frame(n);
     va_list a;
     int i;

     va_begin(a, n);
     for (i = 0; i < n; i++)
	  f->f_var[i] = va_arg(a, type);
     va_end(a);

     return mk_abbrev(d, f);
}

/* mk_binprod -- make a binary Cartesian product type */
PUBLIC type mk_binprod(type a, type b)
{
     type t[2];
     t[0] = a;
     t[1] = b;
     return mk_cproduct(2, t);
}

/* unpack -- unwrap a type from molecules, bound vars, and maybe abbrevs */
PUBLIC void unpack(type *t, frame *f, bool expand_abbrevs)
{
     type t1 = *t, u;
     frame f1 = *f;

     for (;;) {
	  switch (t1->t_kind) {
	  case TYPEVAR:
	       if (f1 == NULL || (u = tv_val(t1, f1)) == NULL)
		    goto out;
	       t1 = u;
	       f1 = arid;
	       break;

	  case MOLECULE:
	       f1 = t1->t_mframe;
	       t1 = t1->t_mtype;
	       break;

	  case ABBREV:
	       if (! expand_abbrevs)
		    goto out;
	       t1 = expand(t1, f1);
	       f1 = arid;
	       break;

	  default:
	       goto out;
	  }
     }
 out:
     *t = t1;
     *f = f1;
}

/* anal_rel_type -- View a type as P (dom x ran) */
PUBLIC bool anal_rel_type(type t, type *dom, type *ran, tree cxt)
{
     frame f = arid;

     unpack(&t, &f, TRUE);
     switch (t->t_kind) {
     case POWERT:
	  t = t->t_base;
	  unpack(&t, &f, TRUE);
	  switch (t->t_kind) {
	  case CPRODUCT: {
	       if (t->t_nfields != 2)
		    return FALSE;
	       *dom = seal(t->t_field[0], f);
	       *ran = seal(t->t_field[1], f);
	       return TRUE;
	  }

	  case TYPEVAR: 
	       if (f == arid)
		    return FALSE;
	       else {
		    frame ff = new_frame(2, cxt);
		    tv_val(t, f) = mk_binprod(*dom = tv_ref(0, ff),
					      *ran = tv_ref(1, ff));
		    return TRUE;
	       }
	   
	  default:
	       return FALSE;
	  }

     case TYPEVAR: 
	  if (f == arid)
	       return FALSE;
	  else {
	       frame ff = new_frame(2, cxt);
	       tv_val(t, f) = mk_power(mk_binprod(*dom = tv_ref(0, ff),
						  *ran = tv_ref(1, ff)));
	       return TRUE;
	  }

     default:
	  return FALSE;
     }
}

/* anal_cproduct - test if a type is a product and get its components */
PUBLIC bool anal_cproduct(type t, type a[], int *n)
{
     frame f = arid;
     int i;

     unpack(&t, &f, TRUE);
     if (t->t_kind != CPRODUCT)
	  return FALSE;
     *n = t->t_nfields;
     for (i = 0; i < *n; i++)
	  a[i] = seal(t->t_field[i], f);
     return TRUE;
}

/* anal_power -- try to view a type as (P base) */
PUBLIC bool anal_power(type t, type *base, tree cxt)
{
     /* The common case (where t is simply a set type) accounts for a big
	fraction of the frames and type variables allocated if done by
	unification -- so it's just about worth doing directly. */

     frame f = arid;

     unpack(&t, &f, TRUE);
     switch (t->t_kind) {
     case POWERT:
	  *base = seal(t->t_base, f);
	  return TRUE;

     case TYPEVAR:
	  if (f == arid) return FALSE;
	  tv_val(t, f) = mk_power(*base = new_typevar(cxt));
	  return TRUE;

     default:
	  *base = err_type;
	  mark_error();
	  return (t == err_type);
     }
}
     
/* is_fun_type -- test if T is a function type with N arguments */
PUBLIC bool is_fun_type(type t, int n)
{
     frame f = arid;

     unpack(&t, &f, TRUE);
     if (t->t_kind != POWERT)
	  return FALSE;
     t = t->t_base;
     unpack(&t, &f, TRUE);
     if (t->t_kind != CPRODUCT || t->t_nfields != 2)
	  return FALSE;
     if (n == 1)
	  return TRUE;
     else {
	  t = t->t_field[0];
	  unpack(&t, &f, TRUE);
	  return (t->t_kind == CPRODUCT
		  && t->t_nfields == n);
     }
}

/* type_kind -- find what kind a type is when unpacked */
PUBLIC kind_t type_kind(type t)
{
     frame f = arid;
     unpack(&t, &f, TRUE);
     return t->t_kind;
}

/* comp_type -- get component type in a schema */
PUBLIC type comp_type(type t, sym x, tree cxt, int loc)
{
     type tt = t;
     frame f = arid;
     schema s;
     int i;

     unpack(&t, &f, TRUE);
     if (t->t_kind != SPRODUCT) panic("comp_type");
     s = t->t_schema;
     for (i = 0; i < s->z_ncomps; i++)
	  if (s->z_comp[i].z_name == x)
	       return seal(s->z_comp[i].z_type, f);

     tc_error(loc, "Selecting non-existent component %z", x);
     if (cxt != nil) {
	  tc_e_etc("Expression: %z", cxt);
	  tc_e_etc("Arg type:   %t", tt);
     }
     tc_e_end();
     mark_error();
     return err_type;
}

/* type_equal -- test two types for equality */
PUBLIC bool type_equal(type t1, frame f1, type t2, frame f2)
{
     unpack(&t1, &f1, FALSE);
     unpack(&t2, &f2, FALSE);
     if (t1 == t2 && f1 == f2)
	  return TRUE;
     else if (t1->t_kind != t2->t_kind)
	  return FALSE;
     else {
	  switch (t1->t_kind) {
	  case GIVENT:	 return (t1->t_given == t2->t_given);
	  case TYPEVAR:	 return (t1->t_typevar == t2->t_typevar);
	  case POWERT:	 return (type_equal(t1->t_base, f1,
					    t2->t_base, f2));
				 
	  case CPRODUCT:
	       if (t1->t_nfields != t2->t_nfields)
		    return FALSE;
	       else {
		    int i;
		    for (i = 0; i < t1->t_nfields; i++)
			 if (! type_equal(t1->t_field[i], f1,
					  t2->t_field[i], f2))
			      return FALSE;
		    return TRUE;
	       }
		    
	  case SPRODUCT: {
		    schema s1 = t1->t_schema, s2 = t2->t_schema;
		    int i;
		    if (s1->z_ncomps != s2->z_ncomps)
			 return FALSE;
		    for (i = 0; i < s1->z_ncomps; i++)
			 if (s1->z_comp[i].z_name != s2->z_comp[i].z_name
			     || ! type_equal(s1->z_comp[i].z_type, f1,
					     s2->z_comp[i].z_type, f2))
			      return FALSE;
		    return TRUE;
	       }

	  case ABBREV:
	       if (t1->t_def != t2->t_def)
		    return FALSE;
	       else {
		    int i;
		    for (i = 0; i < t1->t_def->d_nparams; i++)
			 if (! type_equal(t1->t_params->f_var[i], f1,
					  t2->t_params->f_var[i], f2))
			      return FALSE;
		    return TRUE;
	       }

	  default:
	       bad_tag("type_equal", t1->t_kind);
	       return FALSE;
	  }
     }
}

/* init_type -- create unique type variables */
PUBLIC void init_type(void)
{
     int i;

     type_cache = (struct t_rec *)
          alloc_mem(VAR_SIZE, sizeof(struct t_rec), "init_type");

     begin_perm();
     for (i = 0; i < MAX_ARGS; i++) {
	  type tt = alloc_type();
	  tt->t_kind = TYPEVAR;
	  tt->t_typevar = i;
	  typevar[i] = tt;
     }
     end_perm();
}
