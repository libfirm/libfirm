
%option prefix="_lc_opt_"

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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#include "lc_parser_t.h"
#include "lc_config_parser.h"

static int _lc_opt_wrap(void)
{
	return 1;
}

%}

num             [0-9]+
hexnum          0x[0-9a-fA-F]+
idcharsbegin    [a-zA-Z$_]
idchars         ({idcharsbegin}|[0-9])
ident           {idcharsbegin}{idchars}*
ws              [\t ]*
nl              \n

%x LINE_COMMENT
%x BIG_COMMENT
%x DAT
%x DAT_CONT
%x STR


%%

"/*"                { BEGIN(BIG_COMMENT); }
<BIG_COMMENT>"*/"   { BEGIN(INITIAL); }
<BIG_COMMENT>\n     PMANGLE(linenr)++;
<BIG_COMMENT>.      ;

("#"|"//")          { BEGIN(LINE_COMMENT); }
<LINE_COMMENT>\n    { BEGIN(INITIAL); PMANGLE(linenr)++; }
<LINE_COMMENT>.     ;


<INITIAL>[=:]{ws}   { BEGIN(DAT); }
<DAT>\"             { BEGIN(STR); }
<DAT>\\             { BEGIN(DAT_CONT); }
<DAT>.              { _lc_opt_add_to_data_char(PMANGLE(text)[0]); }
<DAT>\n             {
						BEGIN(INITIAL);
						PMANGLE(linenr)++;
						_lc_opt_add_to_data_char('\0');
						return DATA;
					}
<DAT_CONT>\n        { BEGIN(DAT); PMANGLE(linenr)++; }
<DAT_CONT>.         ;


<STR>\"             {
						BEGIN(INITIAL);
						_lc_opt_add_to_data_char('\0');
						return DATA;
					}

<STR>\\n            _lc_opt_add_to_data_char('\n');
<STR>\\r            _lc_opt_add_to_data_char('\r');
<STR>\\t            _lc_opt_add_to_data_char('\t');
<STR>\\b            _lc_opt_add_to_data_char('\b');
<STR>\\f            _lc_opt_add_to_data_char('\f');
<STR>\\.            _lc_opt_add_to_data_char(yytext[1]);
<STR>[^\"]          _lc_opt_add_to_data_char(yytext[0]);

{ident}				{
						PMANGLE(lval).text.str = PMANGLE(text);
						PMANGLE(lval).text.len = PMANGLE(leng);
						return IDENT;
					}
[/.]                { return SEP; }

{ws}                ;
{nl}                PMANGLE(linenr)++;

.                   return yytext[0];

%%
