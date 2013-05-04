/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Hash table to store names.
 * @author    Goetz Lindenmaier
 */
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
	unsigned   hash   = hash_data((const unsigned char*)str, len);
	set_entry *result = set_hinsert0(id_set, str, len, hash);
	return (ident*)result->dptr;
}

ident *new_id_from_str(const char *str)
{
	assert(str != NULL);
	return new_id_from_chars(str, strlen(str));
}

const char *(get_id_str)(ident *id)
{
	return get_id_str_(id);
}

void finish_ident(void)
{
	del_set(id_set);
	id_set = NULL;
}

ident *id_unique(const char *tag)
{
	static unsigned unique_id = 0;
	char buf[256];

	snprintf(buf, sizeof(buf), tag, unique_id);
	unique_id++;
	return new_id_from_str(buf);
}
