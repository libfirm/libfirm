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
 * @brief       Contains implementation of some useful functions for ia32 backend.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>

#include "irnode.h"
#include "iredges.h"

#include "ia32_util.h"

/**
 * Returns the first Proj with given mode connected to irn.
 * @param irn  The irn
 * @param First proj with mode == mode or NULL if none found
 */
ir_node *ia32_get_proj_for_mode(const ir_node *irn, ir_mode *mode)
{
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) == mode)
			return src;
	}

	return NULL;
}

/**
 * Returns the first Proj with mode != mode_M connected to irn.
 * @param irn  The irn
 * @param First proj with mode != mode_M or NULL if none found
 */
ir_node *ia32_get_res_proj(const ir_node *irn) {
	const ir_edge_t *edge;
	ir_node         *src;

	assert(get_irn_mode(irn) == mode_T && "expected mode_T node");

	foreach_out_edge(irn, edge) {
		src = get_edge_src_irn(edge);

		assert(is_Proj(src) && "Proj expected");

		if (get_irn_mode(src) != mode_M)
			return src;
	}

	return NULL;
}
