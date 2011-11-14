/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief     Hash table to store names.
 * @author    Goetz Lindenmaier
 */
#include "config.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#include "ident_t.h"
#include "set.h"
#include "xmalloc.h"
#include "hashptr.h"

static set *id_set;

void init_ident(void)
{
	/* it's ok to use memcmp here, we check only strings */
	id_set = new_set(memcmp, 128);
}

ident *new_id_from_chars(const char *str, size_t len)
{
	unsigned hash   = HASH_STR(str, len);
	ident   *result = (ident*) set_hinsert0(id_set, str, len, hash);
	return result;
}

ident *new_id_from_str(const char *str)
{
	assert(str != NULL);
	return new_id_from_chars(str, strlen(str));
}

const char *get_id_str(ident *id)
{
	struct set_entry *entry = (struct set_entry*) id;
	return (const char*) entry->dptr;
}

size_t get_id_strlen(ident *id)
{
	struct set_entry *entry = (struct set_entry*) id;
	return entry->size;
}

void finish_ident(void)
{
	del_set(id_set);
	id_set = NULL;
}

int id_is_prefix(ident *prefix, ident *id)
{
	size_t prefix_len = get_id_strlen(prefix);
	if (prefix_len > get_id_strlen(id))
		return 0;
	return 0 == memcmp(get_id_str(prefix), get_id_str(id), prefix_len);
}

int id_is_suffix(ident *suffix, ident *id)
{
	size_t suflen = get_id_strlen(suffix);
	size_t idlen  = get_id_strlen(id);
	const char *part;

	if (suflen > idlen)
		return 0;

	part = get_id_str(id);
	part = part + (idlen - suflen);

	return 0 == memcmp(get_id_str(suffix), part, suflen);
}

int id_contains_char(ident *id, char c)
{
	return strchr(get_id_str(id), c) != NULL;
}

ident *id_unique(const char *tag)
{
	static unsigned unique_id = 0;
	char buf[256];

	snprintf(buf, sizeof(buf), tag, unique_id);
	unique_id++;
	return new_id_from_str(buf);
}
