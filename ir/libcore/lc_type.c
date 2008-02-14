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



#include <ctype.h>
#include "lc_opts_t.h"

int lc_opt_type_scan(void *dest, lc_opt_type_t type, const char *str)
{
	static const char *fmts[] = {
		"", "%s", "%i", "%f"
	};

	static const struct {
		const char *str;
		int val;
	} bool_vals[] = {
		{ "true", 1 },
		{ "on", 1 },
		{ "yes", 1 },
		{ "false", 0 },
		{ "no", 0 },
		{ "off", 0 }
	};

	int res = 0;

	switch(type) {
		case lc_opt_type_int:
		case lc_opt_type_double:
		case lc_opt_type_string:
			res = sscanf(str, fmts[type], dest);
			break;
		case lc_opt_type_boolean:
			{
				size_t i, n;
				int *data = dest;
				char buf[10];

				strncpy(buf, str, sizeof(buf));
				for(i = 0, n = strlen(buf); i < n; ++i)
					buf[i] = tolower(buf[i]);

				for(i = 0; i < LC_ARRSIZE(bool_vals); ++i) {
					if(strcmp(buf, bool_vals[i].str) == 0) {
						res = 1;
						*data = bool_vals[i].val;
						break;
					}
				}
			}
			break;
		default:
			break;
	}

	return res;
}

int lc_opt_type_print(char *buf, size_t n, lc_opt_type_t type, void *data)
{
	int res = 0;

	switch(type) {
		case lc_opt_type_int:
			{
				int i = *((int *) data);
				res = snprintf(buf, n, "%d", i);
			}
			break;
		case lc_opt_type_double:
			{
				double d = *((double *) data);
				res = snprintf(buf, n, "%f", d);
			}
			break;
		case lc_opt_type_string:
			res = snprintf(buf, n, "%s", (const char*) data);
			break;
		case lc_opt_type_boolean:
			res = snprintf(buf, n, "%s", *((int *) data) ? "yes" : "no");
			break;
		default:
			res = 0;
	}

	return res;
}
