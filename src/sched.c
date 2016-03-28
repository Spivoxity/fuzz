/*
 * sched.c
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
 * $Id: sched.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
#include "symbol.h"
#include "absyn.h"

PRIVATE void index_add(sym s, para p)
{
     if (debug('d')) grind(stdout, "Indexing %n\n", s);
     if (s->s_def == NULL) s->s_def = p;
}

PRIVATE void index_list(tree t, para p)
{
     tree u;
     for (u = t; u != nil; u = cdr(u))
	  index_add((sym) car(u), p);
}

PRIVATE void index_schema(tree s, para p)
{
     tree q;

     for (q = s->x_decls; q != nil; q = cdr(q))
	  if (car(q)->x_kind == DECL)
	       index_list(car(q)->x_decl_names, p);
}

/* make_index -- store defining text for each global symbol */
PRIVATE void make_index(para p)
{
     tree t = p->p_text, q;

     switch (t->x_kind) {
     case GIVEN:  
	  index_list(t->x_given, p); break;
     case AXDEF:  
	  index_schema(t->x_axdef, p); break;
     case SDEF:   
	  index_add(mk_symbol((tok) t->x_heading->x_tag, empty), p); break;
     case DEFEQ:  
	  index_add(mk_symbol((tok) t->x_lhs->x_tag, empty), p); break;
     case DEFINE: 
	  index_schema(t->x_def_body, p); break;
     case EQEQ:   
	  index_add((sym) t->x_lhs->x_tag, p); break;
     case DATA:
	  index_add((sym) t->x_lhs, p);
	  for (q = t->x_rhs; q != nil; q = cdr(q))
	       index_add((sym) car(q)->x_tag, p);
	  break;
     default:
	  break;
     }
     if (debug('d')) grind(stdout, "\n");
}

PRIVATE para spec_file = (para) NULL;

PRIVATE void store_para(int n, tree t)
{
     para p = (para) perm_alloc(sizeof(struct para)), q;

     p->p_text = t;
     p->p_serial = n;
     p->p_file = file_name;
     p->p_flag = UNTOUCHED;
     p->p_next = NULL;
     make_index(p);

     if (spec_file == NULL)
	  spec_file = p;
     else {
	  q = spec_file;
	  while (q->p_next != NULL) q = q->p_next;
	  q->p_next = p;
     }
}

/* do_para -- process a paragraph */
PUBLIC void do_para(tree t)
{
     static int para_count = 0;

     para_count++;
     if (vflag && debugging) {
	  if (debug('d')) grind(stdout, "Paragraph %d:\n", para_count);
          if ((t->x_kind != XTAME && t->x_kind != XABBREV)
	      || debug('d'))
	       grind(stdout, "%z\n\n", t);
	  (void) fflush(stdout);
     }

     if (sflag) return;

     if (dflag)
	  store_para(para_count, t);
     else {
	  tc_para(t);
	  clear_temp((univ) NULL);
     }
}

typedef struct {
     para w_para;
     sym w_cxt;
} context;     

/* check_para -- check a paragraph */
PRIVATE void check_para(para p, sym cxt)
{
     int i;
     tree c;
     univ mark;
     char *save_file = file_name;

     static int level = 0;
     static context active[MAX_LEVEL];

     if (p == NULL) return;

     switch (p->p_flag) {
     case UNTOUCHED:
	  if (level >= MAX_LEVEL) 
	       panic("check_para -- Nesting too deep");
	  active[level].w_para = p;
	  active[level].w_cxt = cxt;
	  level++;
	  mark = mark_temp();
	  if (debug('a')) grind(stdout, "Mark: %x\n", mark);
	  p->p_flag = BUSY;
	  file_name = p->p_file;
	  if (debug('d'))
	       grind(stdout, "Processing para %d [level %d]\n",
		     (int) p->p_serial, level);
	  tc_para(p->p_text);
	  level--;
	  clear_temp(mark);
	  p->p_flag = DONE;
	  p->p_text = NULL;
	  file_name = save_file;
	  break;

     case BUSY:
	  c = list1((tree) cxt); i = level;
	  do {
	       i--;
	       if (active[i].w_cxt != NULL)
		    c = cons((tree) active[i].w_cxt, c);	  
	  } while (active[i].w_para != p);
	  tc_error(p->p_text->x_loc,
		   "Cyclic dependency involving %n", cxt);
	  tc_e_etc("Cycle: %l//, //", c);
	  tc_e_end();
	  break;

     case DONE:
	  break;

     default:
	  bad_tag("check_para", (int) p->p_flag);
     }
}

/* check_file -- check all paragraphs from a file */
PUBLIC void check_file(void)
{
     para p;

     if (sflag) return;

     for (p = spec_file; p != NULL; p = p->p_next)
	  check_para(p, (sym) NULL);
}

/* global_def -- find global definition of an identifier */
PUBLIC def global_def(sym x)
{
     if (x->s_glodef == NULL)
	  check_para(x->s_def, x);

     return x->s_glodef;
}
