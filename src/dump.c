/*
 * dump.c
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
 * $Id: dump.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"
#include "symbol.h"
#include "absyn.h"

PRIVATE void dump(tree t, int indent);

PRIVATE void in_dump(tree t, int indent)
{
     printf("\n%*s", 2*indent, "");
     dump(t, indent);
}

PRIVATE void dump_list(tree t, int indent)
{
     if (t == nil)
	  printf(" ()");
     else
	  in_dump(t, indent);
}

PRIVATE void dump(tree t, int indent)
{
     int i; tree u;

     if (t == nil) {
	  printf("()");
	  return;
     }

     switch (t->x_kind) {
     case CONS:
	  printf("(LIST");
	  for (u = t; u != nil; u = cdr(u))
	       in_dump(car(u), indent+1);
	  printf(")");
	  break;

     case WORD:
	  printf("\"%s\"", ((tok) t)->l_chars);
	  break;

     case IDENT:
	  printf("(NAME \"%s\" \"%s\")", 
		 ((sym) t)->s_basename->l_chars,
		 ((sym) t)->s_decor->l_chars);
	  break;

     case REF:
	  printf("(REF %d ", t->x_loc);
	  dump(t->x_tag, 0);
	  dump_list(t->x_params, indent+1);
	  printf(")");
	  break;

     case SREF:
	  printf("(SREF %d ", t->x_loc);
	  dump(t->x_sref_tag, 0);
	  printf(" ");
	  dump(t->x_sref_decor, 0);
	  dump_list(t->x_sref_params, indent+1);
	  dump_list(t->x_sref_renames, indent+1);
	  printf(")");
	  break;

     case NUMBER:
	  printf("(NUMBER %d \"%s\")", t->x_loc, (char *) t->x_number);
	  break;

     default:
	  printf("(%s %d", name_of_tag(t->x_kind), t->x_loc);
	  for (i = 0; i < t->x_arity; i++)
	       in_dump(t->x_slot[i], indent+1);
	  printf(")");
     }
}

PUBLIC void dump_tree(tree t)
{
     dump(t, 0);
     printf("\n");
}
