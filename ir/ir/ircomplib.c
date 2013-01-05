/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Compilerlib entity creation related functions.
 * @date    2011-09-22
 * @author  Manuel Mohr
 */
#include "config.h"

#include "irprog.h"
#include "iroptimize.h"
#include "typerep.h"

#include <assert.h>

/* The default implementation does not set a different ld name. */
static ir_entity *compilerlib_entity_def_creator(ident *id, ir_type *mt)
{
	return new_entity(get_glob_type(), id, mt);
}

static compilerlib_entity_creator_t creator = compilerlib_entity_def_creator;

void set_compilerlib_entity_creator(compilerlib_entity_creator_t c)
{
	assert(c != NULL);

	creator = c;
}

compilerlib_entity_creator_t get_compilerlib_entity_creator()
{
	return creator;
}

ir_entity *create_compilerlib_entity(ident *id, ir_type *mt)
{
	return creator(id, mt);
}
