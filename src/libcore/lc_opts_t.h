/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */



#ifndef _OPTS_T_H
#define _OPTS_T_H

#include <stdlib.h>

#include "lc_opts.h"
#include "list.h"

typedef struct {
	struct list_head opts;
	struct list_head grps;
} lc_grp_special_t;

typedef struct {
	lc_opt_type_t       type;
	lc_opt_callback_t  *cb;
	lc_opt_dump_t      *dump;
	lc_opt_dump_vals_t *dump_vals;
	void               *value;
	size_t              length;
	bool                is_set : 1;
} lc_opt_special_t;

struct lc_opt_entry_t {
	unsigned               hash;
	const char            *name;
	const char            *desc;
	struct lc_opt_entry_t *parent;
	bool                   is_grp : 1;
	struct list_head list;
	union {
		lc_grp_special_t grp;
		lc_opt_special_t opt;
	} v;
};

#define lc_get_opt_special(ent) (&(ent)->v.opt)
#define lc_get_grp_special(ent) (&(ent)->v.grp)

#endif
