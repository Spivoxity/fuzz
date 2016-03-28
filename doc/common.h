/*
 * common.h
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
 * $Id: common.h,v 1.2 2007-02-16 12:45:13 mike Exp $
 *
 */

#define MAXLINE 1024	      /* Max line length (big for glossary) */
#define HASH 256	      /* Size of hash table */
#define MAXITEMS 1024	      /* Max no of main entries */
#define MAXSUBS 128	      /* Max no of subentries per main entry */
#define MAXPAGES 32	      /* Max pages per symdex entry */
#define MAXOUT 72	      /* Max length of output line (gloss) */
#define MINCORE 1024	      /* Min core left before panicking */

typedef enum { FALSE, TRUE } bool;

extern char *cursor, peek;
extern int lineno;

char *get_field(char *term);
int lexico(char *a, char *b);
void setup_and_read(int argc, char *argv[]);

/* Each program provides ... */
void read_record(void);
