/*
 * error.c
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
 */

#include "fuzz.h"
#include "zparse.h"
#include <string.h>
#include <ctype.h>

EXTERN char *yytext;
EXTERN int yychar;

/* Kinds of error message */
#define SYNTAX 1
#define SEMANTIC 2
#define WARNING 3

#ifdef DEBUG
PRIVATE bool in_error = FALSE;
#endif

/* report_error -- Print first line of error message */
PRIVATE void report_error(int kind, int line, char *fmt, va_list *a)
{
     char buf[128];

#ifdef DEBUG
     if (in_error) panic("report_error");
     in_error = TRUE;
#endif

     if (kind != WARNING && ++n_errors >= MAX_ERRORS) {
	  fprintf(errout, "fuzz: too many errors - giving up\n");
	  fflush(errout);
	  exit(1);
     }

     /* Fix bison's canned error messages, among others */
     strcpy(buf, fmt);
     if (islower(buf[0]))
	  buf[0] = toupper(buf[0]);

     grind(errout, "\"%s\", line %d: ", file_name, line);
     if (kind == WARNING)
          grind(errout, "Warning - ");
     do_grind(errout, buf, a);
     if (kind == SYNTAX) {
          if (yychar == 0)
               grind(errout, " at end of file");
          else if (yychar == NL)
               grind(errout, " at \"\\\\\" or \"\\also\"");
          else if (yytext[0] != '\0')
          grind(errout, " at symbol \"%s\"", yytext);
     }
     grind(errout, "\n");
}

/* tc_e_etc -- further details of type error */
PUBLIC void tc_e_etc(char *fmt, ...)
{
     va_list a;

#ifdef DEBUG
     if (! in_error) panic("tc_e_etc");
#endif

     grind(errout, "> ");
     va_begin(a, fmt);
     do_grind(errout, fmt, &a);
     va_end(a);
     grind(errout, "\n");
     fflush(errout);
}

/* tc_e_end -- end of error message and supporting lines */
PUBLIC void tc_e_end(void)
{
#ifdef DEBUG
     if (! in_error) panic("tc_e_end");
     in_error = FALSE;
#endif

     grind(errout, "\n");
     fflush(errout);
}

/* yyerror -- syntax error */
PUBLIC void yyerror(char *fmt, ...)
{
     va_list a;
     EXTERN YYLTYPE yylloc;

     if (strcmp(fmt, "parse error") == 0)
	  fmt = "syntax error";

     va_begin(a, fmt);
     report_error(SYNTAX, yylloc.first_line, fmt, &a);
     va_end(a);
     tc_e_end();
}

/* tc_error -- first line of type error */
PUBLIC void tc_error(int loc, char *fmt, ...)
{
     va_list a;

     va_begin(a, fmt);
     report_error(SEMANTIC, loc, fmt, &a);
     va_end(a);
}

/* tc_warn -- first line of warning */
PUBLIC void tc_warn(int loc, char *fmt, ...)
{
     va_list a;

     va_begin(a, fmt);
     report_error(WARNING, loc, fmt, &a);
     va_end(a);
}

/* bad_tag -- panic when a structure tag has an impossible value */
PUBLIC void bad_tag(char *s, int v)
{
     panic("bad tag %d in %s", v, s);
}

/* panic -- print a message and force a core dump */
PUBLIC void panic(char *fmt, ...)
{
     va_list a;

     grind(errout, "\nPanic: ");
     va_begin(a, fmt);
     do_grind(errout, fmt, &a);
     va_end(a);
     grind(errout, "\n");
     fflush(errout);
     abort();
}
