/*
 * unify.c
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
 * $Id: unify.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
     
PRIVATE frame param_frame;

PRIVATE void bind(type v, frame f, type t)
{
#ifdef DEBUG
     if (debug('u'))
	  (void) grind(stdout, "Bind: %x to %t\n", &tv_val(v, f), t);
#endif

     tv_val(v, f) = t;
}

/* super_exp -- expand all abbrevs in a type */
PRIVATE type super_exp(type t, frame f)
{
     unpack(&t, &f, TRUE);
     switch (t->t_kind) {
     case GIVENT:
	  return t;

     case TYPEVAR:
	  if (f != param_frame)
	       return seal(t, f);
	  else {
	       type new = new_typevar(f->f_cxt);
	       bind(t, f, new);
	       return new;
	  }

     case POWERT:
	  return mk_power(super_exp(t->t_base, f));

     case CPRODUCT: {
	  type a[VAR_SIZE];
	  int i;
	  
	  if (t->t_nfields > VAR_SIZE)
	       panic("super_expand - too many fields");
	  for (i = 0; i < t->t_nfields; i++)
	       a[i] = super_exp(t->t_field[i], f);
	  return mk_cproduct(t->t_nfields, a);
     }

     case SPRODUCT: {
	  schema s = t->t_schema;
	  int n = s->z_ncomps;
	  schema s1 = alloc_schema(n);
	  int i;

	  for (i = 0; i < n; i++) {
	       s1->z_comp[i].z_name = s->z_comp[i].z_name;
	       s1->z_comp[i].z_type
		    = super_exp(s->z_comp[i].z_type, f);
	  }
	  return mk_sproduct(s1);
     }	       

     default:
	  bad_tag("super_exp", t->t_kind);
	  return NULL;
     }
}

/* super_expand -- the public version of super_exp */
PUBLIC type super_expand(type t, frame f)
{
     param_frame = arid;
     return super_exp(t, f);
}

PRIVATE bool u_match(type t1, frame f1, type t2, frame f2);

PRIVATE bool u_occur(type v, frame f1, type t, frame f2)
{
     type w;
     int i;

     switch (t->t_kind) {
     case GIVENT:
	  return FALSE;

     case TYPEVAR:
	  if ((w = tv_val(t, f2)) != NULL)
	       return u_occur(v, f1, w, arid);
	  else
	       return (f1 == f2 && v->t_typevar == t->t_typevar);

     case POWERT:
	  return u_occur(v, f1, t->t_base, f2);

     case CPRODUCT:
	  for (i = 0; i < t->t_nfields; i++)
	       if (u_occur(v, f1, t->t_field[i], f2))
		    return TRUE;
	  return FALSE;

     case SPRODUCT:
	  for (i = 0; i < t->t_schema->z_ncomps; i++)
	       if (u_occur(v, f1, t->t_schema->z_comp[i].z_type, f2))
		    return TRUE;
	  return FALSE;

     case MOLECULE:
	  return u_occur(v, f1, t->t_mtype, t->t_mframe);

     case ABBREV:
	  for (i = 0; i < t->t_def->d_nparams; i++)
	       if (u_occur(v, f1, t->t_params->f_var[i], f2))
		    return TRUE;
	  return FALSE;

     default:
	  bad_tag("u_occur", t->t_kind);
	  return FALSE;
     }
}

PRIVATE bool u_varmatch(type t1, frame f1, type t2, frame f2)
{
     type u;

     if (f1 == arid)
	  panic("u_varmatch");

     if ((u = tv_val(t1, f1)) != NULL) {
	  if (! u_match(u, arid, t2, f2))
	       return FALSE;
	  if (f1 == param_frame)
	       bind(t1, f1, type_union(tv_val(t1, f1), arid, t2, f2));
	  return TRUE;
     }
     else if (t2->t_kind == TYPEVAR && tv_val(t2, f2) != NULL)
	  return u_varmatch(t2, f2, t1, f1);
     else if (u_occur(t1, f1, t2, f2))
	  return FALSE;
     else if (f1 == param_frame)
	  bind(t1, f1, seal(t2, f2));
     else if (t2->t_kind == TYPEVAR && f2 == param_frame)
	  bind(t2, f2, seal(t1, f1));
     else
	  bind(t1, f1, super_exp(t2, f2));
     return TRUE;
}

PRIVATE bool u_match(type t1, frame f1, type t2, frame f2)
{
     schema s1, s2;		    
     int i;

#ifdef DEBUG
     if (debug('u') > 1)
	  grind(stdout, "Match: %t and %t\n", seal(t1, f1), seal(t2, f2));
#endif

     for (;;) {
	  if (t1 == t2 && f1 == f2)
	       /* Includes matching a variable with itself. */
	       return TRUE;
	  else if (t1 == err_type || t2 == err_type) {
	       mark_error();
	       return TRUE;
	  }
	  else if (t1->t_kind == MOLECULE) {
	       f1 = t1->t_mframe;
	       t1 = t1->t_mtype;
	  }
	  else if (t2->t_kind == MOLECULE) {
	       f2 = t2->t_mframe;
	       t2 = t2->t_mtype;
	  }
	  else if (t1->t_kind == TYPEVAR)
	       return u_varmatch(t1, f1, t2, f2);
	  else if (t2->t_kind == TYPEVAR)
	       return u_varmatch(t2, f2, t1, f1);
	  else if (t1->t_kind == ABBREV && t2->t_kind == ABBREV
		   && t1->t_def == t2->t_def) {
	       for (i = 0; i < t1->t_def->d_nparams; i++)
		    if (! u_match(t1->t_params->f_var[i], f1,
				  t2->t_params->f_var[i], f2))
			 return FALSE;
	       return TRUE;
	  }
	  else if (t1->t_kind == ABBREV) {
	       t1 = expand(t1, f1);
	       f1 = arid;
	  }
	  else if (t2->t_kind == ABBREV) {
	       t2 = expand(t2, f2);
	       f2 = arid;
	  }
	  else if (t1->t_kind != t2->t_kind)
	       return FALSE;
	  else switch (t1->t_kind) {
	  case GIVENT:
	       return (t1->t_given == t2->t_given);
	  case POWERT:
	       return u_match(t1->t_base, f1, t2->t_base, f2);
	  case CPRODUCT:
	       if (t1->t_nfields != t2->t_nfields)
		    return FALSE;
	       for (i = 0; i < t1->t_nfields; i++)
		    if (! u_match(t1->t_field[i], f1,
				  t2->t_field[i], f2))
			 return FALSE;
	       return TRUE;
	  case SPRODUCT:
	       /* The vars are sorted (see mk_schema), so it's easy */
	       s1 = t1->t_schema; s2 = t2->t_schema;
	       if (s1->z_ncomps != s2->z_ncomps)
		    return FALSE;
	       for (i = 0; i < s1->z_ncomps; i++)
		    if (s1->z_comp[i].z_name != s2->z_comp[i].z_name
			|| ! u_match(s1->z_comp[i].z_type, f1,
				     s2->z_comp[i].z_type, f2))
			 return FALSE;
	       return TRUE;
	  default:
	       bad_tag("u_match", t1->t_kind);
	       return FALSE;
	  }
     }
}

/* unify(t1,t2) -- unify the types t1 and t2 by assigning to type vars. */
PUBLIC bool unify(type t1, type t2)
{
     return param_unify(t1, t2, arid);
}

/* param_unify(t1, t2, param, shadow) -- unify t1 and t2, allowing 
   abbrevs in the types assigned to the frame param */
PUBLIC bool param_unify(type t1, type t2, frame param)
{
#ifdef DEBUG
     if (debug('u'))
	  grind(stdout, "Unify: %t and %t\n", t1, t2);
#endif
     
     param_frame = param;
     if (! u_match(t1, arid, t2, arid)) {
	  mark_error();
	  return FALSE;
     }
     return TRUE;
}

/* type_union -- take the union of two already unified types */
PUBLIC type type_union(type t1, frame f1, type t2, frame f2)
{
#ifdef DEBUG
     if (debug('u'))
	  grind(stdout, "Union: %t and %t\n", t1, t2);
#endif

     unpack(&t1, &f1, FALSE);
     unpack(&t2, &f2, FALSE);
     if (t1 == t2 && f1 == f2)
	  return seal(t1, f1);
     else if (t1 == err_type || t2 == err_type)
	  return err_type;
     else if (t1->t_kind == ABBREV && t2->t_kind == ABBREV
	      && t1->t_def == t2->t_def) {
	  frame p1 = t1->t_params;
	  frame p2 = t2->t_params;
	  frame pp = mk_frame(fsize(p1));
	  int i;

	  for (i = 0; i < fsize(pp); i++)
	       pp->f_var[i] = type_union(p1->f_var[i], f1,
					 p2->f_var[i], f2);
	  return mk_abbrev(t1->t_def, pp);
     }

     unpack(&t1, &f1, TRUE);
     unpack(&t2, &f2, TRUE);
     switch (t1->t_kind) {
     case GIVENT:
	  return t1;

     case TYPEVAR:
	  return seal(t1, f1);

     case POWERT:
	  return mk_power(type_union(t1->t_base, f1,
				     t2->t_base, f2));

     case CPRODUCT:
	  {
	       type a[MAX_ARGS];
	       int i;

	       for (i = 0; i < t1->t_nfields; i++)
		    a[i] = type_union(t1->t_field[i], f1,
				      t2->t_field[i], f2);
	       return mk_cproduct(t1->t_nfields, a);
	  }

     case SPRODUCT:
	  {
	       schema s1 = t1->t_schema;
	       schema s2 = t2->t_schema;
	       int n = s1->z_ncomps;
	       schema ss;
	       int i;

	       if (s1 == s2 && f1 == f2) 
		    return seal(t1, f1);

	       ss = alloc_schema(n);
	       for (i = 0; i < n; i++) {
		    ss->z_comp[i].z_name = s1->z_comp[i].z_name;
		    ss->z_comp[i].z_type
			 = type_union(s1->z_comp[i].z_type, f1,
				      s2->z_comp[i].z_type, f2);
	       }
	       return mk_sproduct(ss);
	  }

     default:
	  bad_tag("type_union", t1->t_kind);
	  return NULL;
     }
}
