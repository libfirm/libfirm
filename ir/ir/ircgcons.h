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

/*
 * Project:     libFIRM
 * File name:   ir/ir/ircgcons.h
 * Purpose:     Construction and removal of interprocedural representation
 *              (explicit interprocedural dependencies).
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 */
#ifndef _FIRM_IR_ICGCONS_H_
#define _FIRM_IR_ICGCONS_H_

#include "firm_types.h"

/** The state of the interprocedural view.
 *
 * This value indicates the state of the interprocedural view.
 */
typedef enum {
  ip_view_no,       /**< The interprocedural view is not constructed.  There are no
                         view specific nodes (EndReg, Filter, Break ...) in any graph.  */
  ip_view_valid,    /**< The interprocedural view is valid.  */
  ip_view_invalid   /**< The interprocedural view is invalid.  Specific nodes are
                         all still in the representation, but the graph is no more complete. */
} ip_view_state;

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

#endif /* _FIRM_IR_ICGCONS_H_ */
