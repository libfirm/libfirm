/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Methods to manipulate names.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "config.h"

#include <stdio.h>

#include "ident_t.h"
#include "obst.h"
#include "irprintf.h"

/* Make types visible to allow most efficient access */
#include "entity_t.h"
#include "type_t.h"
#include "tpop_t.h"

/** An obstack used for temporary space */
static struct obstack mangle_obst;

/** returned a mangled type name, currently no mangling */
static inline ident *mangle_type(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->name;
}

/* Returns a new ident that represents 'firstscnd'. */
ident *id_mangle(ident *first, ident *scnd)
{
	char *cp;
	int len;
	ident *res;

	obstack_grow(&mangle_obst, get_id_str(first), get_id_strlen(first));
	obstack_grow(&mangle_obst, get_id_str(scnd), get_id_strlen(scnd));
	len = obstack_object_size(&mangle_obst);
	cp = (char*)obstack_finish(&mangle_obst);
	res = new_id_from_chars(cp, len);
	obstack_free(&mangle_obst, cp);
	return res;
}

/** Returns a new ident that represents 'prefixscndsuffix'. */
ident *id_mangle3(const char *prefix, ident *scnd, const char *suffix)
{
	char *cp;
	int len;
	ident *res;

	obstack_grow(&mangle_obst, prefix, strlen(prefix));
	obstack_grow(&mangle_obst, get_id_str(scnd), get_id_strlen(scnd));
	obstack_grow(&mangle_obst, suffix, strlen(suffix));
	len = obstack_object_size(&mangle_obst);
	cp  = (char*)obstack_finish(&mangle_obst);
	res = new_id_from_chars(cp, len);
	obstack_free(&mangle_obst, cp);
	return res;
}

/** Returns a new ident that represents first<c>scnd. */
static ident *id_mangle_3(ident *first, char c, ident *scnd)
{
	char *cp;
	int len;
	ident *res;

	obstack_grow(&mangle_obst, get_id_str(first), get_id_strlen(first));
	obstack_1grow(&mangle_obst, c);
	obstack_grow(&mangle_obst,get_id_str(scnd),get_id_strlen(scnd));
	len = obstack_object_size(&mangle_obst);
	cp = (char*)obstack_finish(&mangle_obst);
	res = new_id_from_chars(cp, len);
	obstack_free(&mangle_obst, cp);
	return res;
}

/* Returns a new ident that represents first_scnd. */
ident *id_mangle_u(ident *first, ident* scnd)
{
	return id_mangle_3(first, '_', scnd);
}

/* Returns a new ident that represents first.scnd. */
ident *id_mangle_dot(ident *first, ident *scnd)
{
	return id_mangle_3(first, '.', scnd);
}

void firm_init_mangle(void)
{
	obstack_init(&mangle_obst);
}

void firm_finish_mangle(void)
{
	obstack_free(&mangle_obst, NULL);
}
