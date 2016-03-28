/*
 * proto.h
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
 * $Id: proto.h,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

/* ALLOC.C */

/* perm_alloc -- allocate SIZE bytes of permanent storage */
PUBLIC univ perm_alloc(int size);

/* alloc -- allocate SIZE bytes from the current pool */
PUBLIC univ alloc(int size);

/* mark_temp -- return marker for temp storage */
PUBLIC univ mark_temp(void);

/* clear_temp -- discard all temporary storage */
PUBLIC void clear_temp(univ mark);

/* begin_perm -- make alloc() return permanent storage */
PUBLIC void begin_perm(void);

/* end_perm -- make alloc() revert to temporary storage */
PUBLIC void end_perm(void);

/* is_perm -- returns TRUE if its argument points to permanent storage */
PUBLIC bool is_perm(univ p);

/* node -- make a tree node in the current pool */
PUBLIC tree node(int n, int loc, int kind, ...);

/* alloc_schema -- allocate space for a schema with N components */
PUBLIC schema alloc_schema(int n);

/* alloc_stats -- print allocation statistics */
PUBLIC void alloc_stats(void);

/* alloc_mem -- allocate memory with malloc */
PUBLIC univ alloc_mem(int n_items, int size, char *cxt);


/* DICT.C */

/* find_def -- find definition of an identifier */
PUBLIC def find_def(sym x, env e);

/* new_env -- allocate a new environment */
PUBLIC env new_env(env parent);

/* check_types -- check that types match */
PUBLIC bool check_types(type t1, type t2, sym x, tree cxt, int loc);

/* pop_def -- remove a def from an env */
PUBLIC def pop_def(env e);

/* push_def -- add an existing def to an env */
PUBLIC void push_def(def d, env v);

/* add_def -- add a definition to an env */
PUBLIC void add_def(kind_d tag, sym x, type t, env v);

/* merge_def -- add a definition if not already present */
PUBLIC void merge_def(kind_d tag, sym x, type t, env v, tree cxt, int loc);

/* del_var -- delete a variable, returning its type or NULL */
PUBLIC type del_var(sym x, env v);

/* hide_var -- delete a variable and check its type */
PUBLIC void hide_var(sym x, type t, env e, tree cxt, int loc);

/* my_compare -- tricky ordering on schema components */
PUBLIC int my_compare(def *d1, def *d2);

/* sort_env -- sort an env to make merging and comparison easier */
PUBLIC env sort_env(env v);

/* mk_schema -- make a schema from an env */
PUBLIC schema mk_schema(env v);

/* def_gset -- globally define a given set name */     
PUBLIC def def_gset(sym x, int loc);

/* install_defs -- installs the defs from an environment as globals */
PUBLIC void install_defs(env e, int loc);

/* def_var -- globally define a variable */
PUBLIC def def_var(sym x, type t, int loc);

/* def_genconst -- globally define a generic constant */
PUBLIC def def_genconst(sym x, env e, type t, int loc);

/* install_genconsts -- install generic constants from an env */
PUBLIC void install_genconsts(env e, int loc);

/* def_schema -- define a schema */
PUBLIC def def_schema(tok x, env s, int loc);

/* init_dict -- set up standard types and constants */
PUBLIC void init_dict(void);


/* ERROR.C */

PUBLIC void yyerror(char *fmt, ...);
PUBLIC void tc_error(int loc, char *fmt, ...);
PUBLIC void tc_warn(int loc, char *fmt, ...);
PUBLIC void tc_e_etc(char *fmt, ...);
PUBLIC void tc_e_end(void);

/* bad_tag -- panic when a structure tag has an impossible value */
PUBLIC void bad_tag(char *s, int v);

/* panic -- print a message and force a core dump */
PUBLIC void panic(char *fmt, ...);


/* EXPR.C */

/* ref_type -- find type of a reference */
PUBLIC type ref_type(sym x, tree p, env e, tree cxt);

/* tc_expr -- check an expression and return its type */
PUBLIC type tc_expr(tree t, env e);

#ifdef ASSUME
PUBLIC void begin_assume(void);
PUBLIC void end_assume(tree cxt);
#endif


/* FRAME.C */

/* begin_frame -- start recording frames */
PUBLIC void begin_frame(void);

/* end_frame -- check all type variables bound */
PUBLIC void end_frame(void);

/* mark_error -- don't complain about unbound variables */
PUBLIC void mark_error(void);

/* mk_frame -- allocate a new frame */
PUBLIC frame mk_frame(int nvars);

/* new_frame -- make a new frame and record it for checking */
PUBLIC frame new_frame(int nvars, tree cxt);


/* PRETTY.C */

PUBLIC void grind(FILE *fp, char *fmt, ...);
PUBLIC int list_len(tree t);
PUBLIC void do_grind(FILE *fp, char *fmt, va_list *ap);
PUBLIC void show_tree(tree t);
PUBLIC void show_type(type t);


/* SPEC.C */

/* tc_given -- declare a list of given set names. */
PUBLIC env tc_given(tree t, env e, int loc);

/* rhs_type -- check an exp has a set type and return the base type. */
PUBLIC type rhs_type(tree t, env e, char *kind);

/* tc_schema -- check a schema text and return the env it creates. */
PUBLIC env tc_schema(tree t, env e);

/* tc_pred -- check a predicate */
PUBLIC void tc_pred(tree t, env e);

/* get_schema -- find the definition of a schema */
PUBLIC def get_schema(tok x, int loc);

/* get_params -- analyse parameters of a schema ref or generic constant. */
PUBLIC void get_params(char *kind, sym x, tree actual, 
		       env e, frame formal, int loc);

/* tc_letdefs -- process the def list of a let construct */
PUBLIC env tc_letdefs(tree t, env e);

/* tc_para -- check a paragraph */	     
PUBLIC void tc_para(tree t);


/* SCHED.C */

/* global_def -- find global definition of an identifier */
PUBLIC def global_def(sym x);

/* do_para -- process a paragraph */
PUBLIC void do_para(tree t);

/* check_file -- do checks for current file */
PUBLIC void check_file(void);


/* SCHEMA.C */

/* open_sref -- find a schema and process its parameters */
PUBLIC bool open_sref(tree t, env e, def *d, frame *f);

/* check_rename -- check that all renamed components exist */
PUBLIC void check_rename(schema s, tok dec, tree rename, tree cxt);

/* get_rename -- decorate and rename an identifier */
PUBLIC sym get_rename(sym x, tok dec, tree rename);

/* do_sref -- add the variables of a schema reference to an env */
PUBLIC void do_sref(tree t, env e, env vars);

/* tc_sexp -- evaluate a schema expression, returning its env. */
PUBLIC env tc_sexp(tree t, env e);


/* TYPE.C */

/* clear_cache -- invalidate the cache used by lambda_type(). */
PUBLIC void clear_cache(void);

/* lambda_type -- abstract over a list of given set names in a type */
PUBLIC type lambda_type(env bv, type t);

/* expand -- expand a type abbreviation */
PUBLIC type expand(type t, frame f);

/* mk_givent -- make a given set type */
PUBLIC type mk_givent(def d);

/* mk_power -- make a power set type */
PUBLIC type mk_power(type t);

/* mk_cproduct -- make a Cartesian product type from an array of types */
PUBLIC type mk_cproduct(int n, type a[]);

/* mk_sproduct -- make a schema product type from a schema. */
PUBLIC type mk_sproduct(schema s);

/* seal -- render a type independent of its frame */
PUBLIC type seal(type t, frame f);

/* alias -- make a frame of references to a frame's variables */
PUBLIC frame alias(frame f);

/* new_typevar -- create a fresh type variable */
PUBLIC type new_typevar(tree cxt);

/* mk_abbrev -- make an abbreviation with params given as a frame */
PUBLIC type mk_abbrev(def d, frame p);

/* mk_xabbrev -- make an abbreviation with an explicit list of params */
PUBLIC type mk_xabbrev(def d, int n, ...);

/* mk_binprod -- make a binary Cartesian product type */
PUBLIC type mk_binprod(type a, type b);

/* anal_rel_type -- View a type as P (dom x ran) */
PUBLIC bool anal_rel_type(type t, type *dom, type *ran, tree cxt);

/* anal_cproduct - test if a type is a product and get its components */
PUBLIC bool anal_cproduct(type t, type a[], int *n);

/* anal_power -- try to view a type as (P base) */
PUBLIC bool anal_power(type t, type *base, tree cxt);

/* is_fun_type -- test if T is a function type with N = 1 or 2 arguments */
PUBLIC bool is_fun_type(type t, int n);

/* type_kind -- find what kind a type is when unpacked */
PUBLIC kind_t type_kind(type a);

/* comp_type -- get component type in a schema */
PUBLIC type comp_type(type t, sym x, tree cxt, int loc);

/* unpack -- unwrap a type from molecules, bound vars, and maybe abbrevs */
PUBLIC void unpack(type *t, frame *f, bool expand_abbrevs);

/* type_equal -- test two types for equality */
PUBLIC bool type_equal(type t1, frame f1, type t2, frame f2);

/* init_type -- create unique type variables */
PUBLIC void init_type(void);


/* UNIFY.C */

/* unify(t1, t2) -- unify the types t1 and t2 by assigning to type vars */
PUBLIC bool unify(type t1, type t2);

/* param_unify(t1, t2, param) -- unify the types t1 and t2, allowing
   abreviations in the types calculated for frame param. */
PUBLIC bool param_unify(type t1, type t2, frame param);

/* type_union -- take the union of two already unified types */
PUBLIC type type_union(type t1, frame f1, type t2, frame f2);

/* super_expand -- expand all abbrevs in a type */
PUBLIC type super_expand(type a, frame f);


/* ZPARSE.Y */

PUBLIC int yyparse(void);

/* join -- destrucitvely append two lists */
PUBLIC tree join(tree a, tree b);

/* name_of_tag -- translate tree tag to string */
PUBLIC char *name_of_tag(int x);

/* ZSCAN.L */

PUBLIC int zzlex(void);
PUBLIC void yyrestart(FILE *input_file);

/* enter -- look up a symbol in the symbol table */
PUBLIC tok enter(char *s, int v);

#ifdef DEBUG
/* dump_hash -- print the hash table */
PUBLIC void dump_hash(void);
#endif

/* mk_symbol -- find or make a symbol from its basename and decoration */
PUBLIC sym mk_symbol(tok name, tok decor);

/* mk_greek -- find or make a Delta or Xi name */
PUBLIC tok mk_greek(tok prefix, tok name);

/* paint -- apply a decoration to an existing symbol */
PUBLIC sym paint(sym name, tok decor);

PUBLIC void init_sym(void);


/* DUMP.C */

/* dump_tree -- output a tree as an S-expression */
PUBLIC void dump_tree(tree t);
