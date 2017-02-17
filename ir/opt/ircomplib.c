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
#include "iroptimize.h"
#include "irprog_t.h"
#include "type_t.h"
#include "typerep.h"
#include <assert.h>

/* The default implementation does not set a different ld name. */
static ident *compilerlib_name_mangle_default(ident *id, ir_type *mt)
{
	(void)mt;
	return id;
}

static compilerlib_name_mangle_t compilerlib_mangler
	= compilerlib_name_mangle_default;

void set_compilerlib_name_mangle(compilerlib_name_mangle_t mangler)
{
	assert(mangler != NULL);
	compilerlib_mangler = mangler;
}

compilerlib_name_mangle_t get_compilerlib_name_mangle(void)
{
	return compilerlib_mangler;
}

ir_entity *create_compilerlib_entity(ident *id, ir_type *mt)
{
	ident *ld_name = compilerlib_mangler(id, mt);

	/* Look for existing entity. */
	ir_entity *entity = ir_get_global(ld_name);
	if (entity != NULL)
		return entity;

	/* Create a new one */
	ir_type *glob = get_glob_type();
	entity = new_entity(glob, ld_name, mt);
	return entity;
}
