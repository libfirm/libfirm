/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @file
 */
#ifndef FIRM_TR_COMPOUND_PATH_T_H
#define FIRM_TR_COMPOUND_PATH_T_H

#include "firm_types.h"
#include "firm_common.h"
#include "compound_path.h"

/** A path in a compound graph. */
struct compound_graph_path {
	firm_kind kind;       /**< The dynamic type tag for compound graph path. */
	ir_type  *tp;          /**< The type this path belongs to. */
	size_t    len;              /**< The length of the path. */
	struct tuple {
		long       index;    /**< Array index.  To compute position of array elements */
		ir_entity *node;    /**< The accessed entity. */
	} list[1];            /**< List of entity/index tuple of length len to express the
	                           access path. */
};

#endif
