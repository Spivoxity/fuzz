/*
 * main.c
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
 * $Id: main.c,v 1.2 2007-02-16 12:45:13 mike Exp $
 */

#define EXTERN
#include "fuzz.h"
#include <ctype.h>
#include <string.h>

extern char *prelude;
extern FILE *yyin;
extern int line_count, yylloc;

char *fuzz_banner = "The fuzz type-checker for Z  ($Revision: 1.2 $)";
char *fuzz_rcsid = "$Id: main.c,v 1.2 2007-02-16 12:45:13 mike Exp $";
char *fuzz_copyright = "Copyright (C) J.M. Spivey 1989, 1992";

PRIVATE void do_flags(char *flags);
PRIVATE void usage(void);
PRIVATE void debug_help(void);
PRIVATE void open_prelude(void);
PRIVATE void reopen_input(char *name);
PRIVATE void read_a_file(void);

PRIVATE int argc;
PRIVATE char **argv;

#ifdef DEBUG
extern int yydebug;
#endif

PUBLIC int main(int ac, char **av)
{
     int i, n_files;
     char **files;

     argc = ac; argv = av;

     while (--argc > 0 && (*++argv)[0] == '-')
          do_flags(*argv);
     files = argv;
     n_files = argc;

     if (debflag('v')) {
	  fprintf(errout, "%s\n%s\n%s\n", 
		  fuzz_banner, fuzz_rcsid, fuzz_copyright);
	  fflush(errout);
     }

     debugging = debflag('p');
     init_sym();
     init_type();
     init_dict();
     open_prelude();
     read_a_file();
     if (dflag) {
	  check_file();
	  clear_temp((univ) NULL);
     }

     debugging = TRUE;
     if (n_files == 0) {
	  /* Finished prelude, no args: read stdin */
	  file_name = "standard input";
	  yyrestart(stdin);
	  read_a_file();
     }
     else {
	  for (i = 0; i < n_files; i++) {
	       reopen_input(files[i]);
	       yyrestart(yyin);
	       read_a_file();
	  }
     }
     if (dflag) {
	  check_file();
	  clear_temp((univ) NULL);
     }

#ifdef DEBUG
     if (debflag('h'))
	  dump_hash();
#endif

     return (n_errors > 0 ? 1 : 0);
}

PRIVATE void do_flags(char *flags)
{
     char *s;

     for (s = flags+1; *s != '\0'; s++)
          switch (*s) {
          case 'a': aflag++; break;
	  case 'd': dflag++; break;
	  case 'l': lflag++; break;
          case 'q': qflag++; break;
          case 's': sflag++; break;
          case 't': tflag++; break;
          case 'v': vflag++; break;

          case 'p':
               if (--argc == 0) usage();
               pflag++;
               prelude = *++argv;
               break;

          case 'D':
               for (s++; *s != '\0'; s++)
                    if (islower(*s))
                         debflag(*s)++;
                    else
                         debug_help();
               s--;
               break;

          default:  usage();
          }
}

#define uu(msg) (void) fprintf(errout, "%s\n", msg)

PRIVATE void usage(void)
{
     uu("Usage: fuzz [-aqstv] [-p file] file ...");
     uu("Flags: -a       Don't use type abbreviations");
     uu("       -d       Allow use before definition");
     uu("       -l       Lisp-style echoing of input");
     uu("       -p file  Use <file> in place of standard prelude");
     uu("       -q       Assume quantifiers for undeclared variables");
     uu("       -s       Syntax check only");
     uu("       -t       Report types of global definitions");
     uu("       -v       Echo input as it is parsed");
     exit(2);
}

PRIVATE void debug_help(void)
{
#ifdef DEBUG
     uu("Debugging options: -D{adehpstuvy}");
     uu("Codes: Allocation, Dependencies, Expressions, Hash table,");
     uu("       Lisp dump, Prelude, Scribble on freed storage,");
     uu("       Type storage, Unification, Version, Yacc parser");
#else
     uu("No debugging code compiled");
#endif

     exit(2);
}

/* perm_copy -- just like strdup() */
PRIVATE char *perm_copy(char *s)
{
     char *t = (char *) perm_alloc(strlen(s)+1);
     strcpy(t, s);
     return t;
}

/* open_prelude -- find and open the prelude file as yyin */
PRIVATE void open_prelude(void)
{
     char *fn;
     extern char *envname;
     extern char *getenv();
     
     if (pflag)
	  fn = prelude;
     else {
	  fn = getenv(envname);
	  if (fn == NULL) {
	       fn = prelude;
	  }
     }

     if (fn == NULL || (yyin = fopen(fn, "r")) == NULL) {
	  (void) fprintf(errout, "fuzz: can't read prelude file %s\n", fn);
	  exit(2);
     }
     file_name = perm_copy(fn);
}

#ifndef DIRSEP
#define DIRSEP '/'
#endif

PRIVATE void reopen_input(char *name)
{
     char *p;
     FILE *fp;
     static char name_buf[128];

     strcpy(name_buf, name);
     fp = freopen(name_buf, "r", yyin);

     if (fp == NULL) {
	  p = name_buf + strlen(name_buf);
	  while (p > name_buf && p[-1] != DIRSEP) p--;
	  if (strchr(p, '.') == NULL) {
	       strcat(name_buf, ".tex");
	       fp = freopen(name_buf, "r", yyin);
	  }
     }
	 
     if (fp == NULL) {
	  fprintf(errout, "fuzz: can't read %s\n", name_buf);
	  exit(2);
     }

     file_name = perm_copy(name_buf);
}

PRIVATE void read_a_file(void)
{
     yylloc = line_count = 1;
#ifdef DEBUG
     yydebug = debug('y');
#endif
     yyparse();
}
