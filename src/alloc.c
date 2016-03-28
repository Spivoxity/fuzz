/*
 * alloc.c
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
 * $Id: alloc.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#include "fuzz.h"

/* Storage is grabbed in chunks of CHUNK bytes, and temp and perm
   storage is allocated from different chunks. If a request cannot be
   allocated from the current chunk, a new chunk is grabbed, and the
   remaining space in the current chunk is lost. The chunks in use for
   temporary and permanent storage are kept in two double-ended lists;
   there is a third list for free chunks which have been released by
   clear_temp(). */

typedef struct chunk {		/* Header for each chunk: included in CHUNK */
     struct chunk *next;	/* Next chunk in the chain */
} chunk;

typedef struct {
     chunk *c_front;		/* First chunk in the chain: NULL if none */
     chunk *c_back;		/* Last chunk in the chain: nonsense if none */
     char *c_free;		/* First free loc: NULL if no chunks */
} chain;

PRIVATE chain perm_chain, temp_chain, free_chain;
PRIVATE chain *current_chain = &temp_chain;

typedef unsigned long addr;

/* do_alloc -- From chain CH, allocate a block of SIZE bytes. */
PRIVATE univ do_alloc(chain *ch, int size)
{
     int rsize;
     char *p;

     rsize = (size + ALIGN - 1) / ALIGN * ALIGN;
     if (rsize + sizeof(chunk) > CHUNK)
	  panic("allocation request too big (%d bytes)", rsize);

     if (ch->c_front == NULL ||
	 ch->c_free + rsize > ((char *) ch->c_back) + CHUNK) {
	  chunk *new_chunk;
	  if (free_chain.c_front != NULL) {
	       new_chunk = free_chain.c_front;
	       free_chain.c_front = new_chunk->next;
	  }
	  else {
#ifdef DEBUG
               if (debug('a')) {
                    grind(stdout, "Calling malloc() ...\n");
                    fflush(stdout);
               }
#endif
               new_chunk = (chunk *) alloc_mem(CHUNK, 1, "do_alloc");
#ifdef DEBUG
               if (debug('a')) {
		    grind(stdout, "Grabbed chunk at %x\n", new_chunk);
                    fflush(stdout);
               }
#endif
	  }
	  
	  new_chunk->next = NULL;
	  if (ch->c_front == NULL)
	       ch->c_front = new_chunk;
	  else
	       ch->c_back->next = new_chunk;
	  ch->c_back = new_chunk;
	  ch->c_free = (char *) new_chunk + sizeof(chunk);
#ifdef DEBUG
	  if (debug('a'))
	       alloc_stats();
#endif
     }

     p = ch->c_free;
     ch->c_free += rsize;
     return (univ) p;
}

/* perm_alloc -- allocate SIZE bytes of permanent storage */
PUBLIC univ perm_alloc(int size)
{
     return do_alloc(&perm_chain, size);
}

/* alloc -- allocate SIZE bytes from the current pool */
PUBLIC univ alloc(int size)
{
     return do_alloc(current_chain, size);
}

/* mark_temp -- return marker for temp storage */
PUBLIC univ mark_temp(void)
{
     return (univ) temp_chain.c_free;
}

/* clear_temp -- discard temporary storage back to mark */
PUBLIC void clear_temp(univ mark)
{
     chunk *p, *r;
	  
     if (mark == NULL)
	  p = NULL;
     else {
	  p = temp_chain.c_front;
	  while ((addr) mark <= (addr) p 
		 || (addr) mark > (addr) p + CHUNK)
	       p = p->next;
     }

     /* p is the last block to be kept */

#ifdef DEBUG
     if (debug('s')) {
	  /* Scribble on the storage before releasing it */
	  char *q = mark;
	  r = p;
	  while (r != NULL) {
	       while (q < (char *) r + CHUNK) {
		    *((long *) q) = 0xaaaaaaaa;
		    q = q + sizeof(long);
	       }
	       r = r->next;
	       q = (char *) r + sizeof(chunk);
	  }
     }
#endif

     if (p == NULL) {
	  r = temp_chain.c_front;
	  temp_chain.c_front = NULL;
     }
     else {
	  r = p->next;
	  p->next = NULL;
     }

     /* r is the first block discarded */

     if (r != NULL) {
	  if (free_chain.c_front == NULL)
	       free_chain.c_front = r;
	  else
	       free_chain.c_back->next = r;
	  free_chain.c_back = temp_chain.c_back;
     }

     temp_chain.c_back = p;
     temp_chain.c_free = (char *) mark;

#ifdef DEBUG
	  if (debug('a'))
	       alloc_stats();
#endif
}

/* begin_perm -- make alloc() return permanent storage */
PUBLIC void begin_perm(void)
{
     current_chain = &perm_chain;
}

/* end_perm -- make alloc() revert to temporary storage */
PUBLIC void end_perm(void)
{
     current_chain = &temp_chain;
}

/* is_perm -- returns TRUE if its argument points to permanent storage */
PUBLIC bool is_perm(univ p)
{
     chunk *q;

     for (q = perm_chain.c_front ; q != NULL; q = q->next)
	  if ((addr) q <= (addr) p && (addr) p < (addr) q + CHUNK)
	       return TRUE;
     return FALSE;
}

#ifdef DEBUG
PRIVATE int chunk_count(chain *ch)
{
     chunk *p;
     int n = 0;

     for (p = ch->c_front; p != NULL; p = p->next)
	  n++;
     return n;
}

/* alloc_stats -- print allocation statistics */
PUBLIC void alloc_stats(void)
{
     (void) printf(
	      "Storage stats: perm = %d, temp = %d, free = %d blocks of %d\n",
	      chunk_count(&perm_chain), chunk_count(&temp_chain),
	      chunk_count(&free_chain), CHUNK);
}
#endif

PUBLIC tree node(int n, int loc, int k, ...)
{
     int i;
     tree p;
     va_list a;

     p = (tree) alloc(sizeof(struct blob));
     p->x_kind = k;
     p->x_loc = loc;
     p->x_arity = n;
     p->x_slot = (tree *) alloc(n * sizeof(tree));
     va_begin(a, k);
     for (i = 0; i < n; i++)
	  p->x_slot[i] = va_arg(a, tree);
     va_end(a);
     return p;
}

/* alloc_schema -- allocate space for a schema with N components */
PUBLIC schema alloc_schema(int n)
{
     schema s = (schema) alloc(sizeof(struct schema));
     s->z_ncomps = n;
     s->z_comp = (compnt *) alloc(n * sizeof(compnt));
     return s;
}

PUBLIC univ alloc_mem(int n_items, int size, char *cxt)
{
     univ p = malloc(n_items * (size_t) size);
     if (p == NULL) panic("out of memory in %s", cxt);
     return p;
}
