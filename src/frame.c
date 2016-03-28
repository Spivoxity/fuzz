/*
 * frame.c
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
 * $Id: frame.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"

PRIVATE struct {
     frame b_chain;
     bool b_mark;
} stack[MAX_LEVEL];
PRIVATE int level = -1;

/* begin_frame -- start recording frames */
PUBLIC void begin_frame(void)
{
     if (level >= MAX_LEVEL)
	  panic("begin_frame -- Nesting too deep");

     level++;
     stack[level].b_chain = NULL;
     stack[level].b_mark = FALSE;
}

/* end_frame -- check all type variables bound */
PUBLIC void end_frame(void)
{
     frame p;
     int j;
     
     for (p = stack[level].b_chain; p != NULL; p = p->f_next) {
	  int failed = FALSE;

	  for (j = 0; j < p->f_nvars; j++) {
	       if (p->f_var[j] == NULL) {
		    failed = TRUE;
		    p->f_var[j] = err_type;
	       }
	  }

	  if (failed && ! stack[level].b_mark) {
	       tc_error(p->f_cxt->x_loc,
			"Implicit parameters not completely determined");
	       tc_e_etc("Expression: %z", p->f_cxt);
	       tc_e_end();
	  }
     }

     level--;
}
     
/* mark_error -- don't complain about unbound variables */
PUBLIC void mark_error(void)
{
     stack[level].b_mark = TRUE;
}

/* mk_frame -- allocate a new frame */
PUBLIC frame mk_frame(int nvars)
{
     if (nvars == 0)
	  return arid;
     else {
	  frame f = (frame) alloc(sizeof(struct frame));
	  int i;

	  f->f_nvars = nvars;
	  f->f_next = NULL;
	  f->f_cxt = nil;
	  f->f_var = (type *) alloc(nvars * sizeof(type));
	  for (i = 0; i < nvars; i++)
	       f->f_var[i] = NULL;
	  return f;
     }
}

/* new_frame -- make a new frame and record it for checking */
PUBLIC frame new_frame(int nvars, tree cxt)
{
     frame f = mk_frame(nvars);

     if (f != arid) {
	  f->f_cxt = cxt;
	  f->f_next = stack[level].b_chain;
	  stack[level].b_chain = f;
     }
     
     return f;
}
