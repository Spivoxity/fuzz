/*
 * dict.c
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
 * $Id: dict.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
#include "symbol.h"

/* The global environment is represented using the s_glodef fields in the
   symbol table. Other environments are extensions of this, and are
   represented as chains of env_rec records.
*/

/* find_def -- find definition of an identifier */
PUBLIC def find_def(sym x, env e)
{
     env e1; def d;

     for (e1 = e; e1 != NULL; e1 = e1->e_next)
	  for (d = e1->e_defs; d != NULL; d = d->d_next)
	       if (d->d_name == x)
		    return d;

     return (global_def(x));
}

/* new_env -- allocate a new environment */
PUBLIC env new_env(env parent)
{
     env e = (env) alloc(sizeof(struct env));
     e->e_next = parent;
     e->e_partial = partial_env(parent);
     e->e_ndefs = 0;
     e->e_defs = NULL;
     return e;
}

/* alloc_def -- allocate space for a def */
PRIVATE def alloc_def(kind_d tag, sym name, int nparams, type t)
{
     def p = (def) alloc(sizeof(struct def));

#ifdef DEBUG
     if (debug('n')) grind(stdout, "Def %z : %t\n", name, t);
#endif

     p->d_kind = tag;
     p->d_name = name;
     p->d_nparams = nparams;
     p->d_abbrev = FALSE;
     p->d_tame = FALSE;
     p->d_next = NULL;
     p->d_type = (tag == GSET ? mk_power(mk_givent(p)) : t);

     return p;
}

/* push_def -- add an existing def to an env */
PUBLIC void push_def(def d, env e)
{
     e->e_ndefs++;
     d->d_next = NULL;
     if (e->e_defs == NULL)
	  e->e_defs = d;
     else {
	  def dd = e->e_defs;
	  while (dd->d_next != NULL) dd = dd->d_next;
	  dd->d_next = d;
     }
}

/* pop_def -- remove a def from an env */
PUBLIC def pop_def(env e)
{
     def d;

     if (e->e_ndefs == 0)
	  return NULL;
     e->e_ndefs--;
     d = e->e_defs;
     e->e_defs = d->d_next;
     return d;
}

/* add_def -- add a definition to an env */
PUBLIC void add_def(kind_d tag, sym x, type t, env e)
{
     push_def(alloc_def(tag, x, 0, t), e);
}

/* check_types -- check that types match */
PUBLIC bool check_types(type t1, type t2, sym x, tree cxt, int loc)
{
     if (! unify(t1, t2)) {
	  tc_error(loc, "Type mismatch in declarations of %n", x);
	  tc_e_etc("Previous type: %t", t1);
	  tc_e_etc("Current type:  %t", t2);
	  if (cxt != nil)
	       tc_e_etc("Expression: %z", cxt);
	  tc_e_end();
	  return FALSE;
     }
     
     return TRUE;
}

/* merge_def -- add a definition if not already present */
PUBLIC void merge_def(kind_d tag, sym x, type t, env e, tree cxt, int loc)
{
     def d;

     for (d = e->e_defs; d != NULL; d = d->d_next)
	  if (x == d->d_name) {
	       switch (tag) {
	       case GSET:
		    tc_error(loc, "Basic type name %n multiply declared", x);
		    if (cxt != nil)
			 tc_e_etc("Expression: %z", cxt);
		    tc_e_end();
		    break;

	       case VAR:
		    check_types(d->d_type, t, x, cxt, loc);
		    break;

	       case DATACONS:
	       case LETVAR:
		    tc_error(loc, "%s %n multiply declared",
			     (tag == DATACONS ? "Constructor name" :
			     "Let variable"), x);
		    if (cxt != nil)
			 tc_e_etc("Expression: %z", cxt);
		    tc_e_end();
		    break;
	       default:
		    bad_tag("merge_def", tag);
	       }
	       return;
	  }

     add_def((tag == GSET ? GSET : VAR), x, t, e);
}

/* del_var -- delete a variable, returning its type or NULL */
PUBLIC type del_var(sym x, env e)
{
     def d, dd;
     for (dd = NULL, d = e->e_defs; d != NULL; dd = d, d = d->d_next) {
	  if (x != d->d_name) continue;
	  e->e_ndefs--;
	  if (dd == NULL)
	       e->e_defs = d->d_next;
	  else
	       dd->d_next = d->d_next;
	  return d->d_type;
     }
     return NULL;
}


/* hide_var -- delete a variable and check its type */
PUBLIC void hide_var(sym x, type t, env e, tree cxt, int loc)
{
     type tt = del_var(x, e);

     if (tt == NULL) {
	  tc_error(loc, "Hiding non-existent component %n", x);
	  tc_e_etc("Expression: %z", cxt);
	  tc_e_end();
     }
     else if (t != NULL && ! unify(t, tt)) {
	  tc_error(loc, "Type mismatch in hiding variable %n", x);
	  tc_e_etc("Expression:    %z", cxt);
	  tc_e_etc("Previous type: %t", tt);
	  tc_e_etc("Current type:  %t", t);
	  tc_e_end();
     }
}

/* my_compare -- tricky ordering on schema components */
PUBLIC int my_compare(def *v1, def *v2)
{
     /* This tricky sorting order groups names with the same decoration
	together, and otherwise preserves order of first mention. */
     tok x1 = (*v1)->d_name->s_basename;
     tok x2 = (*v2)->d_name->s_basename;
     tok d1 = (*v1)->d_name->s_decor;
     tok d2 = (*v2)->d_name->s_decor;

     if (d1 == d2)
	  return x1->l_toknum - x2->l_toknum;

     return d1->l_toknum - d2->l_toknum;
}

#ifdef ANSI
typedef int (*compfun)(const void *, const void *);
#else
typedef int (*compfun)();
#endif

/* sort_env -- sort an env to make comparison and merging easier */
PUBLIC env sort_env(env e)
{
     int i, n = e->e_ndefs;
     def d;
     static def *buf;

     if (buf == NULL)
        buf = (def *) alloc_mem(VBUF_SIZE, sizeof(def), "sort_env");

     if (n == 0) return e;
     if (n > VBUF_SIZE) panic("too many components");
     for (i = 0, d = e->e_defs; i < n; i++, d = d->d_next)
	  buf[i] = d;
     qsort((char *) buf, n, sizeof(def), (compfun) my_compare);
     e->e_defs = buf[0];
     for (i = 1; i < n; i++)
	  buf[i-1]->d_next = buf[i];
     buf[n-1]->d_next = NULL;
     return e;
}

/* mk_schema -- make a schema from an env */
PUBLIC schema mk_schema(env e)
{
     int i, n = e->e_ndefs;
     schema s = alloc_schema(n);
     def d;
     sort_env(e);
     for (i = 0, d = e->e_defs; i < n; i++, d = d->d_next) {
	  s->z_comp[i].z_name = d->d_name;
	  s->z_comp[i].z_type = d->d_type;
     }
     return s;
}     
	  
/* trace_def -- print tracing info about a global def */
PRIVATE void trace_def(def d)
{
     switch (d->d_kind) {
     case GSET:
          grind(stdout, "Given %i", d->d_name);
	  break;

     case VAR:
          grind(stdout, "Var %i: %t", d->d_name, d->d_type);
	  break;

     case GENCONST:
	  if (d->d_nparams == 0)
               grind(stdout, "Abbrev %i: %t", d->d_name, d->d_type);
	  else
               grind(stdout, "Genconst %i[%d]: %t",
		     d->d_name, d->d_nparams, d->d_type);
	  break;
	  
     case SCHEMA: {
	  schema s = d->d_schema;
	  int i;
	  grind(stdout, "Schema %n", d->d_name);
	  if (d->d_nparams != 0)
	       grind(stdout, "[%d]", d->d_nparams);
	  grind(stdout, "\n");
	  for (i = 0; i < s->z_ncomps; i++)
	       grind(stdout, "    %i: %t\n",
		     s->z_comp[i].z_name, s->z_comp[i].z_type);
          grind(stdout, "End");
	  break;
     }

     default:
	  bad_tag("trace_def", d->d_kind);
     }
     grind(stdout, "\n\n");
     (void) fflush(stdout);
}

/* new_global -- install a definition in the global env. */
PRIVATE def new_global(def d, int loc)
{
     sym x = d->d_name;

     if (x->s_glodef != NULL) {
	  tc_error(loc, "Global name %n multiply declared", x);
	  tc_e_end();
     }
     x->s_glodef = d;
     if (tflag && debugging) 
	  trace_def(d);
     return d;
}

/* def_gset -- globally define a given set name */     
PUBLIC def def_gset(sym x, int loc)
{
     def d;

     begin_perm();
     d = alloc_def(GSET, x, 0, (type) NULL);
     end_perm();

     return new_global(d, loc);
}

/* install_defs -- install an environment in the global env. */
PUBLIC void install_defs(env e, int loc)
{
     def d;

     clear_cache();
     begin_perm();
     for (d = e->e_defs; d != NULL; d = d->d_next) {
	  kind_d tag = d->d_kind;
	  def d1 = alloc_def(tag, d->d_name, 0,
			     (tag == GSET ? (type) NULL :
			      lambda_type(global, d->d_type)));
	  new_global(d1, loc);
     }
     end_perm();
}

/* def_var -- globally define a variable */
PUBLIC def def_var(sym x, type t, int loc)
{
     def d;

     clear_cache();
     begin_perm();
     d = alloc_def(VAR, x, 0, lambda_type(global, t));
     end_perm();
     return new_global(d, loc);
}

/* gen_def -- construct and install a generic definition */
PRIVATE def gen_def(sym x, int n, type t, int loc)
{
     def d;

     d = alloc_def(GENCONST, x, n, t);
     d->d_abbrev = (n == 0 && t != err_type && type_kind(t) == POWERT);
     return new_global(d, loc);
}

/* def_genconst -- globally define a generic constant */
PUBLIC def def_genconst(sym x, env e, type t, int loc)
{
     def d;

     clear_cache();
     begin_perm();
     d = gen_def(x, e->e_ndefs, lambda_type(e, t), loc);
     end_perm();
     return d;
}
     
/* install_genconsts -- install generic constants from an env */
PUBLIC void install_genconsts(env e, int loc)
{
     def d;

     clear_cache();
     begin_perm();
     for (d = e->e_defs; d != NULL; d = d->d_next)
	  (void) gen_def(d->d_name, e->e_next->e_ndefs,
			 lambda_type(e->e_next, d->d_type), loc);
     end_perm();
}
     
/* def_schema -- define a schema */
PUBLIC def def_schema(tok x, env s, int loc)
{
     /* The top two layers of the env s contain the generic parameters
	and the components of the schema. */

     env bv = s->e_next, e = new_env(global);
     def d;

     clear_cache();
     begin_perm();
     for (d = s->e_defs; d != NULL; d = d->d_next)
	  add_def(VAR, d->d_name, lambda_type(bv, d->d_type), e);
     d = alloc_def(SCHEMA, mk_symbol(x, empty), bv->e_ndefs, (type) NULL);
     d->d_abbrev = (bv->e_ndefs == 0);
     d->d_schema = mk_schema(e);
     end_perm();

     return new_global(d, loc);
}	  

/* init_dict -- set up standard types and constants */
PUBLIC void init_dict(void)
{
     env bv = tc_given(list1((tree) X), global, 0);
     env bv2 = tc_given(list2((tree) X, (tree) Y), global, 0);
     
#define ex gset_type(bv->e_defs)
#define ex2 gset_type(bv2->e_defs)
#define why2 gset_type(bv2->e_defs->d_next)

     num_type = gset_type(def_gset(num, 0));
     err_type = gset_type(def_gset(_err_, 0));

     nat_def = def_var(nat, mk_power(num_type), 0);
     begin_perm();
     nat_type = (aflag ? num_type : mk_xabbrev(nat_def, 0));
     end_perm();

     pfun_def = def_genconst(pfun, bv2, mk_power(rel_type(ex2, why2)), 0);
     seq_def = def_genconst(seq, bv, mk_power(rel_type(nat_type, ex)), 0);
     bag_def = def_genconst(bag, bv, mk_power(rel_type(ex, nat_type)), 0);
}
