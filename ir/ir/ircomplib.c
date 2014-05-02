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
#include "irprog.h"
#include "iroptimize.h"
#include "typerep.h"
#include "irtypes.h"

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
	ir_entity *entity = pmap_get(ir_entity, irp->compilerlib_entities, id);
	if (entity != NULL)
		return entity;

	/* let frontend mangle the name */
	ident *ld_name = compilerlib_mangler(id, mt);
	/* search for an existing entity */
	ir_type *glob = get_glob_type();
	for (size_t i = 0, n_members = get_compound_n_members(glob);
	     i < n_members; ++i) {
	    ir_entity *member = get_compound_member(glob, i);
	    if (get_entity_ld_ident(member) == ld_name) {
			entity = member;
			goto found;
		}
	}
	entity = new_entity(glob, id, mt);
	set_entity_ld_ident(entity, ld_name);
	set_entity_visibility(entity, ir_visibility_external);

found:
	pmap_insert(irp->compilerlib_entities, id, entity);
	return entity;
}
