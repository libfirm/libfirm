/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Compilerlib entity creation related functions.
 * @date    2011-09-22
 * @author  Manuel Mohr
 * @version $Id$
 */
#include "config.h"

#include "irprog.h"
#include "iroptimize.h"
#include "typerep.h"

#include <stddef.h>
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
