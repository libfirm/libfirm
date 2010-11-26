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
void pl_free(pl_info *pli);

/** Get the associated graph. */
ir_graph *pl_get_irg(pl_info *pli);

/** Get the loop depth of the given irn. */
int pl_get_depth(pl_info *pli, ir_node *irn);

/**
 * Get the first eta node in the irg. For the "it" parameter either pass NULL
 * (to only get one node), or a pointer to an allocated pl_iter, to use with
 * pl_iter_next.
 */
ir_node *pl_get_irg_eta(pl_info *pli, pl_iter *it);

/**
 * Get the first theta node in the irg. For the "it" parameter either pass NULL
 * (to only get one node), or a pointer to an allocated pl_iter, to use with
 * pl_iter_next.
 */
ir_node *pl_get_irg_theta(pl_info *pli, pl_iter *it);

/**
 * Get the first eta node associated with the given irn. For the "it"
 * parameter either pass NULL (to only get one node), or a pointer to an
 * allocated pl_iter, to use with pl_iter_next.
 */
ir_node *pl_get_eta(pl_info *pli, ir_node *irn, pl_iter *it);

/**
 * Get the first theta node associated with the given eta irn. For the "it"
 * parameter either pass NULL (to only get one node), or a pointer to an
 * allocated pl_iter, to use with pl_iter_next.
 */
ir_node *pl_get_theta(pl_info *pli, ir_node *irn, pl_iter *it);

/**
 * Get the first border node associated with the given eta irn. For the "it"
 * parameter either pass NULL (to only get one node), or a pointer to an
 * allocated pl_iter, to use with pl_iter_next.
 */
ir_node *pl_get_border(pl_info *pli, ir_node *irn, pl_iter *it);

/** Get the number of eta nodes associated with the given irn. */
int pl_get_eta_count(pl_info *pli, ir_node *irn);

/** Get the number of theta nodes associated with the given eta irn. */
int pl_get_theta_count(pl_info *pli, ir_node *irn);

/** Get the number of border nodes associated with the given eta irn. */
int pl_get_border_count(pl_info *pli, ir_node *irn);

/** Get the next node from the given iterator. */
ir_node *pl_iter_next(pl_iter *it);

/**
 * Set depth of the given node. Intended to integrate new nodes into the graph
 * without doing a new analysis, not intended to change existing info.
 */
void pl_set_depth(pl_info *pli, ir_node *irn, int depth);

/**
 * Copy analysis information of src to the node dst. Intended to integrate new
 * nodes into the graph without doing a new analysis, not intended to change
 * existing info.
 */
void pl_copy_info(pl_info *pli, ir_node *src, ir_node *dst);

/** Dumps the loop analysis results to the specified file. */
void pl_dump(pl_info *pli, FILE* f);

#define foreach_pl_irg_eta(pli, it, eta) \
	for ((eta) = pl_get_irg_eta((pli), &(it)); \
		(eta); (eta) = pl_iter_next(&(it)))

#define foreach_pl_irg_theta(pli, it, theta) \
	for ((theta) = pl_get_irg_theta((pli), &(it)); \
		(theta); (theta) = pl_iter_next(&(it)))

#define foreach_pl_eta(pli, irn, it, eta) \
	for ((eta) = pl_get_eta((pli), (irn), &(it)); \
		(eta); (eta) = pl_iter_next(&(it)))

#define foreach_pl_theta(pli, irn, it, theta) \
	for ((theta) = pl_get_theta((pli), (irn), &(it)); \
		(theta); (theta) = pl_iter_next(&(it)))

#define foreach_pl_border(pli, irn, it, border) \
	for ((border) = pl_get_border((pli), (irn), &(it)); \
		(border); (border) = pl_iter_next(&(it)))

#endif
