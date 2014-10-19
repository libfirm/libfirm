/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Methods to manipulate names.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "ident_t.h"
#include "obst.h"

/** An obstack used for temporary space */
static struct obstack mangle_obst;

static ident *new_ident_from_obst(struct obstack *obst)
{
	size_t len    = obstack_object_size(obst);
	char  *string = (char*)obstack_finish(obst);
	ident *res    = new_id_from_chars(string, len);
	obstack_free(obst, string);
	return res;
}

static void grow_string(char const *const s)
{
	obstack_grow(&mangle_obst, s, strlen(s));
}

/** Returns a new ident that represents 'prefixscndsuffix'. */
ident *id_mangle3(char const *const prefix, ident *const middle, char const *const suffix)
{
	grow_string(prefix);
	grow_string(get_id_str(middle));
	grow_string(suffix);
	return new_ident_from_obst(&mangle_obst);
}

void firm_init_mangle(void)
{
	obstack_init(&mangle_obst);
}

void firm_finish_mangle(void)
{
	obstack_free(&mangle_obst, NULL);
}
