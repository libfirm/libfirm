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
#include "ident.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "platform_t.h"
#include "type_t.h"
#include "typerep.h"
#include <assert.h>

ir_entity *create_compilerlib_entity(ident *id, ir_type *mt)
{
	ident *ld_name = platform_mangle_global(id);

	/* Look for existing entity. */
	ir_entity *entity = ir_get_global(ld_name);
	if (entity != NULL)
		return entity;

	/* Create a new one */
	ir_type *glob = get_glob_type();
	entity = new_entity(glob, ld_name, mt);
	return entity;
}
