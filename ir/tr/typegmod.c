/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @file    typegmod.c
 * @brief   Functionality to modify the type graph.
 * @author  Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "type_t.h"
#include "tpop_t.h"
#include "irmode.h"

void exchange_types(ir_type *old_type, ir_type *new_type) {
	unsigned flags = old_type->flags & (tf_frame_type | tf_value_param_type | tf_global_type | tf_tls_type);
	/* Deallocate datastructures not directly contained in the
	   old type.  We must do this now as it is the latest point
	   where we know the original kind of type.
	   */
	free_type_attrs(old_type);

	/* @@@@
	   Things to deal with:
	   * After exchange_types the type has two entries in the list of
	     all types in irp.  So far this is fine for the walker.
	     Maybe it's better to remove the id entry and shrink the list.
	     Does this conflict with the walker?  Might a type be left out
	     during the walk?
	   * Deallocation:  if the Id is removed from the list it will eventually
	     disappear in a memory leak.  When is impossible to determine so we
	     need to hold it in a separate list for deallocation.
	*/

	/* Exchange the types */
	old_type->type_op = type_id;
	old_type->mode = (ir_mode *) new_type;
	/* ensure that the frame, value param, global and tls flags
	   are set right if these types are exchanged */
	new_type->flags |= flags;
}

ir_type *skip_tid(ir_type *tp) {
	/* @@@ implement the self cycle killing trick of skip_id(ir_node *) */
	while (tp->type_op == type_id)
		tp = (ir_type *) tp->mode;
	return tp;
}
