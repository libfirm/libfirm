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
 * @file
 * @brief   Construction and removal of interprocedural representation
 *          (explicit interprocedural dependencies).
 * @author  Hubert Schmid
 * @date    09.06.2002
 * @version $Id$
 */
#ifndef FIRM_IR_IRCGCONS_H
#define FIRM_IR_IRCGCONS_H

#include "firm_types.h"

/** Return the current state of the interprocedural view. */
ip_view_state get_irp_ip_view_state(void);
/** Set the state of the interprocedural view to invalid. */
void set_irp_ip_view_invalid(void);

/** Construction of the interprocedural view.
 *
 * Construction of the interprocedural view.  A prior analysis must have set
 * all possible callees in the corresponding fields of Call nodes.  Sets
 * ip_view_valid in irp.
 *
 * @arg free_methods_arr: An array of all free methods, i.e., methods that
 *                        are external visible.  These methods get an 'Unknown'
 *                        caller.
 * @arg arr_len           The number of free methods. */
void cg_construct(int arr_len, ir_entity *free_methods_arr[]);


/** Deconstruction of the interprocedural view.  Reduces memory consumption of
    the ir. Sets ip_view_no in irp. */
void cg_destruct(void);

#endif
