/*
 * fuzz.h
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
 * $Id: fuzz.h,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

/* Possible defines for conditional compilation:
     DEBUG     Include debugging code
     ANSI      Ansi-conformant compiler*
     ASSUME    Implement -q flag

     * determined automatically in Turbo C and MPW C.
*/

#ifndef EXTERN
#define EXTERN extern
#endif
#define PUBLIC
#define PRIVATE static

#ifndef ANSI
#define const
#endif

typedef int bool;
#define TRUE 1
#define FALSE 0

/* Various table sizes ...
     VBUF_SIZE: maximum number of components in a schema.
     MAX_ARGS: maximum number of ...
	  elements in a tuple, factors in a cartesian product.
     VAR_SIZE: maximum number of ...
	  implicitly quantified variables in an expression,
	  variables bound in a unification,
	  types cached during type abstraction.
     MAX_ERRORS: number of error messages before giving up.
     MAX_LEVEL: maximum nesting level for checking paragraphs.
*/

#define VBUF_SIZE 2048
#define MAX_ARGS 32
#define VAR_SIZE 256
#define MAX_ERRORS 100
#define MAX_LEVEL 64
#define HASHSIZE 521
#define FNMAX 128

/* Parameters for storage allocation: CHUNK doit etre un multiple de ALIGN */
#define CHUNK 65536		/* Size of a storage chunk */
#define ALIGN 4			/* Requests rounded to a multiple of this */

#define errout stderr

#include <stdio.h>

#ifdef ANSI
typedef void *univ;
#include <stdlib.h>
#else
typedef char *univ;
univ malloc();
#include <sys/types.h>
#endif

/*
     The arguments of va_start() vary between old-style and ANSI C, so
     we use it through a macro va_begin().  For old-style C, the
     conversion script fixes up the function heading.
*/
#ifdef ANSI
#include <stdarg.h>
#define va_begin(x, y) va_start(x, y)
#else
#include <varargs.h>
#define va_begin(x, y) va_start(x)
#endif

typedef struct blob *tree;

struct blob {
     int x_kind;
     int x_arity;
     int x_loc;
     tree *x_slot;
};

#define YYSTYPE tree

#define cons(x,a)	node(2,0,CONS,x,a)
#define nil		((tree) NULL)
#define car(a)		(a)->x_slot[0]
#define cdr(a)		(a)->x_slot[1]
#define cadr(a)		car(cdr(a))
#define cddr(a)		cdr(cdr(a))
#define list1(x)	cons(x,nil)
#define list2(x,y)	cons(x,cons(y,nil))
#define list3(x,y,z)	cons(x,cons(y,cons(z,nil)))
#define snoc(a,x) join(a,list1(x))

#define exists(a)	((a)->x_kind != NOTHING)
#define the(a)		(a)->x_slot[0]
#define pair(x,y)	node(1,0,TUPLE,list2(x,y))
#define is_pair(t)	((t)->x_kind == TUPLE \
			 && list_len((t)->x_elements) == 2)
#define first(t)	car((t)->x_elements)
#define second(t)	cadr((t)->x_elements)

#define simply(x,n)	ref(x,nil,n)
#define ref(x,p,n)	node(3, n, REF, x, p)
#define sref(x,d,p,r,n)	node(5, n, SREF, x, d, p, r)

#define perm_dup(s)	strcpy((char *) perm_alloc(strlen(s)+1), (s));

typedef struct env *env;
typedef struct type *type;
typedef struct def *def;
typedef struct tok *tok;
typedef struct sym *sym;
typedef struct frame *frame;
 
struct frame {
     int f_nvars;		/* No of type variables */
     frame f_next;	        /* Next frame in chain */
     tree f_cxt;		/* Expression that caused its creation */
     type *f_var;		/* The type variables */
};

#define arid ((frame) NULL)
#define fsize(f) ((f) == arid ? 0 : (f)->f_nvars)
#define fcxt(f) ((f) == arid ? nil : (f)->f_cxt)

typedef struct {
     sym z_name;
     type z_type;
} compnt;

typedef struct schema {
     int z_ncomps;
     compnt *z_comp;
} *schema;

typedef enum { 
     GIVENT, TYPEVAR, POWERT, CPRODUCT, SPRODUCT, MOLECULE, ABBREV
} kind_t;

struct type {
     kind_t t_kind;
     union {
	  def given_t;
	  int typevar_t;
	  type base_t;
	  struct { int t__nfields; type *t__field; } cproduct_t;
	  schema schema_t;
	  struct { type t__mtype; frame t__mframe; } molecule_t;
	  struct { def t__def; frame t__params; } abbrev_t;
     } t_u;
};

#define t_given	    t_u.given_t
#define t_typevar   t_u.typevar_t
#define t_base	    t_u.base_t
#define t_nfields   t_u.cproduct_t.t__nfields
#define t_field	    t_u.cproduct_t.t__field
#define t_schema    t_u.schema_t
#define t_mtype	    t_u.molecule_t.t__mtype
#define t_mframe    t_u.molecule_t.t__mframe
#define t_def	    t_u.abbrev_t.t__def
#define t_params    t_u.abbrev_t.t__params

#define ok_type(t) 	((t) != err_type)
#define tv_val(v, f) 	f->f_var[v->t_typevar]

#define mk_pfun(x, y) 	mk_xabbrev(pfun_def, 2, x, y)
#define mk_seq(x) 	mk_xabbrev(seq_def, 1, x)
#define mk_bag(x)	mk_xabbrev(bag_def, 1, x)
#define rel_type(x, y) 	mk_power(mk_binprod(x, y))

typedef enum {
     GSET, VAR, SCHEMA, GENCONST,	/* Genuine kinds of definition */
     DATACONS, LETVAR			/* Changed to VAR by merge_def */
} kind_d;

struct def {
     kind_d d_kind;
     sym d_name;
     int d_nparams;
     bool d_abbrev, d_tame;
     def d_next;
     union {
	  type type_d;
	  schema schema_d;
     } d_u;
};

#define d_type d_u.type_d
#define d_schema d_u.schema_d
#define gset_type(d) (d)->d_type->t_base

/* Marks on a paragraph */
typedef enum {
     UNTOUCHED, BUSY, DONE
} mark;

typedef struct para *para;

struct para {
     tree p_text;	        /* Text, or NULL if processed */
     int p_serial;	        /* Serial number (mostly debugging) */
     char *p_file;		/* Name of containing file */
     mark p_flag;	        /* Stage of processing */
     para p_next;		/* Next paragraph */
};

struct tok {
     int x_kind;		/* Always WORD */
     char *l_chars;		/* Spelling */
     int l_value;		/* Token returned by yylex() */
     int l_prio;	        /* Operator priority */
     int l_toknum;		/* Serial number for sorting sigs */
     char *l_pname;		/* Print name: used by pp_sym() */
     sym l_sym;			/* Chain of symbols with this basename */
     tok l_link;		/* Next token with same hash value */
     tok l_prefix;		/* Greek prefix (Delta or Xi) */
     tok l_root;		/* Result of stripping Greek prefix */
};

struct sym {
     int x_kind;		/* Always IDENT */
     tok s_basename;		/* Basename */
     tok s_decor;		/* Decoration */
     def s_glodef;		/* Global definition */
     para s_def;	        /* Defining paragraph */
     sym s_link;		/* Next symbol with same basename */
};

#define lexval(s) ((s)->s_basename->l_value)

struct env {
     env e_next;		/* Next lower level */
     int e_ndefs;		/* Number of definitions */
     bool e_partial;		/* TRUE if errors in declaration part */
     def e_defs;		/* The definitions */
};

#define global ((env) NULL)
#define partial_env(e) ((e) != global && e->e_partial)

#include "proto.h"

EXTERN type err_type, num_type, nat_type;
EXTERN def pfun_def, seq_def, bag_def, nat_def;
EXTERN tok uminus, minus, image, empty, prime, query, pling, Delta, Xi;
EXTERN sym num, _err_, nat, pfun, seq, bag, iter, X, Y, infop;

EXTERN int aflag, dflag, lflag, pflag, qflag, sflag, tflag, vflag;
EXTERN bool debugging;
EXTERN int dcode[26];
#define debflag(c) dcode[c - 'a']
#define debug(c) (debugging && debflag(c))

EXTERN char *file_name;
EXTERN int n_errors;
