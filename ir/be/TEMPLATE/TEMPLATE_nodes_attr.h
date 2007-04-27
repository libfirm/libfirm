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

#ifndef _TEMPLATE_NODES_ATTR_H_
#define _TEMPLATE_NODES_ATTR_H_

#include "../bearch_t.h"

typedef struct _TEMPLATE_attr_t {
	arch_irn_flags_t flags;     /**< indicating if spillable, rematerializeable ... etc. */
	int              n_res;     /**< number of results for this node */

	const arch_register_req_t **in_req;  /**< register requirements for arguments */
	const arch_register_req_t **out_req; /**< register requirements for results */

	/* must be last, dynamically allocated */
	const arch_register_t *slots[1];         /**< register slots for assigned registers */
} TEMPLATE_attr_t;

#endif /* _TEMPLATE_NODES_ATTR_H_ */
