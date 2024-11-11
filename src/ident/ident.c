/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Hash table to store names.
 * @author    Goetz Lindenmaier
 */
#include "ident_t.h"

#include "hashptr.h"
#include "obst.h"
#include "set.h"
#include <stdio.h>
#include <string.h>

static set *id_set;

/** An obstack used for temporary space */
static struct obstack id_obst;

void init_ident(void)
{
	/* it's ok to use memcmp here, we check only strings */
	id_set = new_set(memcmp, 128);
	obstack_init(&id_obst);
}

ident *new_id_from_chars(const char *str, size_t len)
{
	unsigned   hash   = hash_data((const unsigned char*)str, len);
	set_entry *result = set_hinsert0(id_set, str, len, hash);
	return (ident*)result->dptr;
}

ident *new_id_from_str(const char *str)
{
	return new_id_from_chars(str, strlen(str));
}

static ident *new_ident_from_obst(struct obstack *const obst)
{
	size_t const len    = obstack_object_size(obst);
	char  *const string = (char*)obstack_finish(obst);
	ident *const res    = new_id_from_chars(string, len);
	obstack_free(obst, string);
	return res;
}

ident *new_id_fmt(char const *const fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	obstack_vprintf(&id_obst, fmt, ap);
	va_end(ap);
	return new_ident_from_obst(&id_obst);
}

const char *(get_id_str)(ident *id)
{
	return get_id_str_(id);
}

void finish_ident(void)
{
	obstack_free(&id_obst, NULL);
	del_set(id_set);
	id_set = NULL;
}

ident *id_unique(const char *tag)
{
	static unsigned unique_id = 0;
	return new_id_fmt("%s.%u", tag, unique_id++);
}
