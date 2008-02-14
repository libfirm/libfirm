
%name-prefix="_lc_opt_"

%{

/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <string.h>

#include "lc_opts_t.h"
#include "lc_parser_t.h"

static void group_open(void);
static void group_close(void);
static void lc_opt_set(void);
static void path_push(text_t text);

void PMANGLE(error)(const char *str);
int PMANGLE(linenr);

%}

%union {
	text_t text;
	int num;
}

%token SEP DATA
%token <text> IDENT

%%

main: dseq ;

decl: group
	| option
	;

decls: decls decl
	| decl
	;

dseq: decls | ;

group: path { group_open(); } '{' dseq '}' { group_close(); } ;

option: path DATA { lc_opt_set(); } ;

path: path SEP IDENT { path_push($3); }
	| IDENT { path_push($1); }
	;

%%

static lc_opt_error_handler_t *handler;
static struct obstack obst;
const char *optfilename = "";

void PMANGLE(error)(const char *str)
{
	fprintf(stderr, "At line %d: %s\n", PMANGLE(linenr), str);
}

static const char *path_stack[128];
static int path_sp = 0;

static lc_opt_entry_t *grp_stack[128];
static int grp_sp = 0;
#define CURR_GRP (grp_stack[grp_sp - 1])

void lc_opt_init_parser(const char *filename, lc_opt_error_handler_t *err_handler)
{
	PMANGLE(linenr) = 1;
	obstack_init(&obst);
	handler = err_handler;
	optfilename = filename;
	grp_stack[grp_sp++] = lc_opt_root_grp();
}

void _lc_opt_add_to_data_char(char c)
{
	obstack_1grow(&obst, c);
}

static void path_push(text_t text)
{
	obstack_grow0(&obst, text.str, text.len);
	path_stack[path_sp++] = obstack_finish(&obst);
}

static void path_free(void)
{
	obstack_free(&obst, (void *) path_stack[0]);
	path_sp = 0;
}

static void group_open(void)
{
	lc_opt_err_info_t err;
	lc_opt_entry_t *grp = lc_opt_resolve_grp(CURR_GRP, path_stack, path_sp, &err);

	path_free();

	grp_stack[grp_sp++] = grp;
}

static void group_close(void)
{
	grp_sp--;
}

static void lc_opt_set(void)
{
	char *str = obstack_finish(&obst);

	lc_opt_err_info_t err;
	lc_opt_entry_t *opt = lc_opt_resolve_opt(CURR_GRP, path_stack, path_sp, &err);
	lc_opt_raise_error(&err, handler, "At %s(%d): ", optfilename, PMANGLE(linenr));

	lc_opt_occurs(opt, str, &err);
	lc_opt_raise_error(&err, handler, "At %s(%d): ", optfilename, PMANGLE(linenr));

	obstack_free(&obst, str);
	path_free();
}
