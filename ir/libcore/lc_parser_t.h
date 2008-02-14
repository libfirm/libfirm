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



#ifndef _PARSER_T_H
#define _PARSER_T_H

#include "lc_opts_t.h"

typedef struct {
	const char *str;
	int len;
} text_t;

void lc_opt_init_parser(const char *filename, lc_opt_error_handler_t *handler);

void _lc_opt_add_to_data_char(char c);

#define PPREFIX _lc_opt_
#define PMANGLE(name) _lc_opt_ ## name

extern FILE *PMANGLE(in);
extern int PMANGLE(linenr);

int PMANGLE(parse)(void);
int PMANGLE(lex)(void);

#endif
