%{
/*
 * zscan.l
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
#include <unistd.h>

PRIVATE int token;
PRIVATE int lookup(int v);
PRIVATE void get_string(int);
PRIVATE void end_line(void);

/*
 * line_count is the actual line the scanner has reached; saved_line
 * is the line on which the last \\ appeared. They are used to set
 * yylloc, the line number tracked by the parser.
 */
PUBLIC int line_count = 1;
PRIVATE int saved_line = 1; 

PRIVATE int prev_mode;
PRIVATE bool in_dir;

#define math_mode()	{ BEGIN (prev_mode = MATH); }
#define prose_mode()	{ BEGIN (prev_mode = PROSE); }
#define dir_mode()	{ BEGIN MATH; prev_mode = PROSE; in_dir = TRUE; }
#define skip_mode()	{ BEGIN (prev_mode = SKIP); }

#define VANILLA 0
#define BINARY 1
#define ENVBEG 2
#define ENVEND 3
PRIVATE char tok_type[MAX_TOKEN];

#define NORMAL	 1
#define NEWLINE	 2
#define AFTEROP  3
#define PENDING  4
#define STARTUP  5
#define SKIPPING 6
PRIVATE int state = STARTUP;

EXTERN YYSTYPE yylval;
EXTERN YYLTYPE yylloc;

PUBLIC int yywrap(void) { return 1; }
%}

%START				PROSE MATH COMMENT SKIP

name				[A-Za-z]([A-Za-z0-9]|\\_)*
number				[0-9]+
box				(axdef|gendef|schema|zed|syntax)
decor				(['!?]|_[0-9])+

%%

%				{ BEGIN COMMENT; }

<COMMENT>.*			;
<SKIP,PROSE>"\\"		;
<SKIP,PROSE>[^%\\\n]+		;

<PROSE>"\\begin{"{box}"}"	{ math_mode(); return (lookup(0)); }
<MATH>"\\end{"{box}"}"		{ prose_mode(); return (lookup(0)); }
<SKIP>"\\end{"{box}"}"		{ prose_mode(); }

<MATH>("\\\\"|"\\also")		{ saved_line = line_count; return NL; }
<MATH>[.,]/[ \t\n]*("\\end{"{box}"}"|\\also)  ;
<MATH>[&~"]			;
<MATH>"{}"			;
<MATH>\\[;:, !]			;
<MATH>\\t([0-9]+|\{[0-9]+\})	{
	if (state != STARTUP && state != NEWLINE && state != SKIPPING)
		yyerror("Tab command not at start of line"); }
<MATH>[@|()\[\]{}.=;:,/^]	return *yytext;
<MATH>\\_			return DUMMY;
<MATH>::=			return COCOEQ;
<MATH>"\\{"			return LBRACE;
<MATH>"\\}"			return RBRACE;
<MATH>{name}/({decor})?[ \t\n]+({name}|{number}) {
	if (! in_dir) yyerror("Adjacent names - possibly missing ~");
	return lookup(WORD); }
<MATH>\\#			|
<MATH>\\[A-Za-z]+		|
<MATH>{name}			|
<MATH>\\exists_1		|
<MATH>[+\-*.=<>]+		|
<MATH>\\power_1			return lookup(WORD);
<MATH>\$\"[^"\n]*\"		{ get_string(yyleng); return STRING; }
<MATH>{number}                  { yylval = (tree) strcpy((char *) 
					perm_alloc(yyleng+1), yytext);
				  return NUMBER; }
<MATH>{decor}			return lookup(DECOR);

<PROSE>^"%%unchecked"		{ skip_mode(); }
<PROSE>^"%%"[a-z]+		{ dir_mode(); return lookup(0); }
^%%				;

\n				{ end_line();
				  if (in_dir) {
					prose_mode();
					in_dir = FALSE;
					return EDIR;
				  }
				  BEGIN prev_mode; }
[ \t]				;
.				return *yytext;

%%
PRIVATE void end_line(void)
{
     /* Though the newline filter is responsible for
        setting yylloc, we do it here too, for the sake
	of the few error messages issued directly by the lexer. */
     yylloc.first_line = ++line_count;
}

/* Symbol table */

PRIVATE tok *hash_table;
PRIVATE int n_toks = 0;

/* enter -- look up a symbol in the symbol table */
PUBLIC tok enter(char *s, int v)
{
     /* If a symbol named s is already present, return it; else if v == 0,
	return NULL; otherwise create a new symbol and return it */

     char *a;
     unsigned h = 0;
     tok p;

     for (a = s; *a != '\0'; a++)
	  h = (h * 17 + *a);
     h %= HASHSIZE;

     for (p = hash_table[h]; p != NULL; p = p->l_link)
	  if (strcmp(s, p->l_chars) == 0)
	       return p;

     if (v == 0)
	  return NULL;
     
     p = (tok) perm_alloc(sizeof(struct tok));
     p->x_kind = WORD;
     p->l_chars = perm_dup(s);
     p->l_value = v;
     p->l_prio = 0;
     p->l_toknum = ++n_toks;
     p->l_pname = p->l_chars;
     p->l_sym = NULL;
     p->l_link = hash_table[h];
     p->l_prefix = NULL;
     p->l_root = NULL;
     hash_table[h] = p;
     return p;
}

#ifdef DEBUG
#define HMAX 10

/* dump_hash -- print the hash table */
PUBLIC void dump_hash(void)
{
     int n, count, oflo = 0;
     tok p;
     int histo[HMAX];

     for (n = 0; n < HMAX; n++)
	  histo[n] = 0;

     printf("Hash table dump (HASHSIZE = %d, n_toks = %d)\n", 
	    HASHSIZE, n_toks);
     for (n = 0; n < HASHSIZE; n++) {
	  count = 0;
	  printf("%3d:", n);
	  for (p = hash_table[n]; p != NULL; p = p->l_link) {
	       count++;
	       printf(" %s", p->l_chars);
	  }
	  if (count < HMAX)
	       histo[count]++;
	  else
	       oflo++;
	  printf("\n");
     }

     printf("\nDistribution of chain sizes (%d entries in %d chains)\n",
	    n_toks, HASHSIZE);
     for (n = 0; n < HMAX; n++)
	  printf("%d: %d\n", n, histo[n]);
     printf("%d or more: %d\n", HMAX, oflo);
}
#endif

/* lookup -- look up yytext in the symbol table. */
PRIVATE int lookup(int v)
{
     tok p;

     if ((p = enter(yytext, v)) == NULL)
	  return BADTOK;
     
     yylval = (tree) p;
     return p->l_value;
}

/* mk_symbol -- find or make a symbol from its basename and decoration */
PUBLIC sym mk_symbol(tok name, tok decor)
{
     sym p;

     for (p = name->l_sym; p != NULL; p = p->s_link)
	  if (p->s_decor == decor)
	       return p;

     p = (sym) perm_alloc(sizeof(struct sym));
     p->x_kind = IDENT;
     p->s_basename = name;
     p->s_decor = decor;
     p->s_glodef = NULL;
     p->s_def = NULL;
     p->s_link = name->l_sym;
     name->l_sym = p;
     return p;
}

/* mk_greek -- find or make a Delta or Xi name */
PUBLIC tok mk_greek(tok prefix, tok name)
{
     tok p;
     char buf[256];

     strcpy(buf, prefix->l_chars);
     strcat(buf, " ");
     strcat(buf, name->l_chars);
     p = enter(buf, SNAME);
     p->l_prefix = prefix;
     p->l_root = name;
     return p;
}

/* paint -- apply a decoration to an existing symbol */
PUBLIC sym paint(sym name, tok decor)
{
     char buf[256];

     if (decor == empty)
	  return name;
     else if (name->s_decor == empty)
	  return mk_symbol(name->s_basename, decor);
     else {
	  strcpy(buf, name->s_decor->l_chars);
	  strcat(buf, decor->l_chars);
	  return mk_symbol(name->s_basename, enter(buf, DECOR));
     }
}

/* get_string -- save a $"..." string */
PRIVATE void get_string(int length)
{
     char *s = (char *) perm_alloc(length-2);
     strncpy(s, yytext+2, length-3);
     s[length-3] = '\0';
     yylval = (tree) s;
}

/* Newline filter -- this goes between the lexer and the parser */

PUBLIC int zzlex()
{
     if (state == PENDING)
	  state = NORMAL;
     else {
	  do {
	       token = yylex();
	       if (token == NL) {
		    if (state == AFTEROP || state == SKIPPING)
			 state = SKIPPING;
		    else
			 state = NEWLINE;
	       }
	  } while (token == NL || token == IGNORE);
     }

     if (tok_type[token] == BINARY) 
	  state = AFTEROP;
     else if (token == WHERE || tok_type[token] == ENVBEG)
	  state = STARTUP;
     else if (state == NEWLINE && tok_type[token] != ENVEND) {
	  state = PENDING;
	  yylloc.first_line = saved_line;
	  return NL;
     }
     else if (token != DECOR)
	  state = NORMAL;

     yylloc.first_line = line_count;
     return token;
}

/* Initialization */

PRIVATE int binops[] = {
     INOP, '-', INREL, INGEN, ';', ':', ',', '|', '@', EQEQ, DEFEQ,
     COCOEQ, '=', IN, AND, OR, IMPLIES, EQUIV, CROSS, HIDE, PROJECT,
     FATSEMI, PIPE, THEN, ELSE, 0
};

PRIVATE int envs[] = {
     BAXDEF, EAXDEF, BGENDEF, EGENDEF, BSCHEMA, ESCHEMA,
     BZED, EZED, BSYNTAX, ESYNTAX, 0
};

/* init_sym -- initialize the symbol table */
PUBLIC void init_sym(void)
{
     int n;
     
     hash_table = (tok *) alloc_mem(HASHSIZE, sizeof(tok), "init_sym");
     for (n = 0; n < HASHSIZE; n++)
	  hash_table[n] = NULL;
     
     enter("%%token", XTOKEN);
     empty = enter("", DECOR);
     prime = enter("'", DECOR);
     query = enter("?", DECOR);
     pling = enter("!", DECOR);
     image = enter("_(|_|)", IMAGE);
     minus = enter("-",	'-');
     uminus = enter("(-)", UMINUS);
     uminus->l_pname = minus->l_chars;
     Delta = enter("\\Delta", GREEK);
     Xi = enter("\\Xi", GREEK);

     num = mk_symbol(enter("\\num", WORD), empty);
     nat = mk_symbol(enter("\\nat", WORD), empty);
     iter = mk_symbol(enter("iter", WORD), empty);
     pfun = mk_symbol(enter("\\pfun", INGEN), empty);
     seq = mk_symbol(enter("\\seq", PREGEN), empty);
     bag = mk_symbol(enter("\\bag", PREGEN), empty);
     X = mk_symbol(enter("*1*", WORD), empty);
     Y = mk_symbol(enter("*2*", WORD), empty);
     _err_ = mk_symbol(enter("*errtype*", WORD), empty);
     infop = mk_symbol(enter("*infop*", INOP), empty);
     infop->s_basename->l_prio = -1;

     for (n = 0; binops[n] != 0; n++)
	  tok_type[binops[n] ] = BINARY;

     for (n = 0; envs[n] != 0; n += 2) {
	  tok_type[envs[n] ] = ENVBEG;
	  tok_type[envs[n+1] ] = ENVEND;
     }

     prose_mode();
}
