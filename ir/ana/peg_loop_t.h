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

/**
 * @file
 * @brief   Compute loop information for PEG graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_ANA_PEG_LOOP_T_H
#define FIRM_ANA_PEG_LOOP_T_H

#include <stdio.h>
#include "firm_types.h"

typedef struct pl_info pl_info;
typedef void* pl_iter;

/** Compute the loop information for the given irg. */
pl_info *pl_init(ir_graph *irg);

/** Free the loop information for the given tree. */
void pl_free(pl_info *info);

/** Get the loop depth of the given irn. */
int pl_get_depth(pl_info *info, ir_node *irn);

/**
 * Get the first eta node in the graph. For the "it" parameter either pass NULL
 * (to only get one node), or a pointer to an allocated pl_iter, to use with
 * pl_iter_next.
 */
ir_node *pl_get_eta(pl_info *info, pl_iter *it);

/**
 * Get the first node that is linked to the given irn. For eta nodes, these
 * are the associated theta nodes and for theta nodes these are the eta nodes.
 * For the "it" parameter either pass NULL (to only get one node), or a pointer
 * to an allocated pl_iter, to use with pl_iter_next.
 */
ir_node *pl_get_link(pl_info *info, ir_node *irn, pl_iter *it);

/** Get the next node from the given iterator. */
ir_node *pl_iter_next(pl_iter *it);

/** Dumps the loop analysis results to the specified file. */
void pl_dump(pl_info *info, FILE* f);

#define foreach_pl_theta(info, irn, it, child) \
	for ((child) = pl_get_theta((info), (irn), &(it)); \
		(child); (child) = pl_iter_next(&(it)))

#define foreach_pl_eta(info, irn, it, child) \
	for ((child) = pl_get_eta((info), (irn), &(it)); \
		(child); (child) = pl_iter_next(&(it)))

#endif
