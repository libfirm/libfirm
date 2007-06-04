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
 * @brief   attributes attached to all TEMPLATE nodes
 * @version $Id$
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_NODES_ATTR_H
#define FIRM_BE_TEMPLATE_TEMPLATE_NODES_ATTR_H

#include "../bearch_t.h"

typedef struct TEMPLATE_attr_t  TEMPLATE_attr_t;

struct TEMPLATE_attr_t
{
	arch_irn_flags_t flags;     /**< indicating if spillable, rematerializeable ... etc. */

	const arch_register_req_t **in_req;  /**< register requirements for arguments */
	const arch_register_req_t **out_req; /**< register requirements for results */

	const arch_register_t **slots;       /**< register slots for assigned registers */
};

#endif
