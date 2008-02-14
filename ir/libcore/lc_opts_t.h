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



#ifndef _OPTS_T_H
#define _OPTS_T_H

#include <stdlib.h>

#include "lc_opts.h"
#include "list.h"

#include "lc_common_t.h"
#include "lc_defines.h"

typedef struct {
	struct list_head opts;
	struct list_head grps;
} lc_grp_special_t;

typedef struct {
	lc_opt_type_t type;
	lc_opt_callback_t *cb;
	lc_opt_dump_t *dump;
	lc_opt_dump_vals_t *dump_vals;
	void *value;
	size_t length;
	unsigned is_set : 1;
} lc_opt_special_t;

struct _lc_opt_entry_t {
	unsigned hash;
	const char *name;
	const char *desc;
	struct _lc_opt_entry_t *parent;

	unsigned is_grp : 1;

	struct list_head list;

	union {
		lc_grp_special_t grp;
		lc_opt_special_t opt;
	} v;
};

#define lc_get_opt_special(ent) (&(ent)->v.opt)
#define lc_get_grp_special(ent) (&(ent)->v.grp)

int lc_opt_raise_error(const lc_opt_err_info_t *err,
					   lc_opt_error_handler_t *handler,
					   const char *fmt, ...);

#endif
