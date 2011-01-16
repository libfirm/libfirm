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
#include "plist.h"

typedef struct pl_info pl_info;
typedef plist_element_t *pl_irg_etas_iter;
typedef plist_element_t *pl_irg_thetas_iter;
typedef plist_element_t *pl_etas_iter;
typedef plist_element_t *pl_thetas_iter;
typedef plist_element_t *pl_border_iter;

/** Compute the loop information for the given irg. */
pl_info *pl_init(ir_graph *irg);

/** Free the loop information for the given tree. */
void pl_free(pl_info *pli);

/** Get the associated graph. */
ir_graph *pl_get_irg(pl_info *pli);

/** Get the loop depth of the given irn. */
int pl_get_depth(pl_info *pli, ir_node *irn);

/** Get a count of the eta nodes in the graph. */
int pl_get_irg_eta_count(pl_info *pli);

/** Initialize an iterator to iterate the eta nodes in the graph. */
void pl_irg_etas_iter_init(pl_info *pli, pl_irg_etas_iter *it);

/** Get the next element from the given iterator. */
ir_node *pl_irg_etas_iter_next(pl_irg_etas_iter *it);

/** Get a count of the theta nodes in the graph. */
int pl_get_irg_theta_count(pl_info *pli);

/** Initialize an iterator to iterate the theta nodes in the graph. */
void pl_irg_thetas_iter_init(pl_info *pli, pl_irg_thetas_iter *it);

/** Get the next element from the given iterator. */
ir_node *pl_irg_thetas_iter_next(pl_irg_thetas_iter *it);

/** Get a count of the theta nodes associated with irn. */
int pl_get_theta_count(pl_info *pli, ir_node *irn);

/** Initialize an iterator to iterate the theta nodes associated with irn. */
void pl_thetas_iter_init(pl_info *pli, ir_node *irn, pl_thetas_iter *it);

/** Get the next element from the given iterator. */
ir_node *pl_thetas_iter_next(pl_thetas_iter *it);

/** Get a count of the theta nodes associated with irn. */
int pl_get_eta_count(pl_info *pli, ir_node *irn);

/** Initialize an iterator to iterate the theta nodes associated with irn. */
void pl_etas_iter_init(pl_info *pli, ir_node *irn, pl_etas_iter *it);

/** Get the next element from the given iterator. */
ir_node *pl_etas_iter_next(pl_etas_iter *it);

/** Get a count of the border nodes associated with irn. */
int pl_get_border_count(pl_info *pli, ir_node *irn);

/** Initialize an iterator to iterate the border nodes associated with irn. */
void pl_border_iter_init(pl_info *pli, ir_node *irn, pl_border_iter *it);

/** Get the next element from the given iterator. */
ir_node *pl_border_iter_next(pl_border_iter *it);

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

#define foreach_pl_irg_etas(pli, eta, it) \
	for(pl_irg_etas_iter_init((pli), &(it)), \
		(eta) = pl_irg_etas_iter_next(&(it)); \
		(eta); \
		(eta) = pl_irg_etas_iter_next(&(it)))

#define foreach_pl_irg_thetas(pli, theta, it) \
	for(pl_irg_thetas_iter_init((pli), &(it)), \
		(theta) = pl_irg_thetas_iter_next(&(it)); \
		(theta); \
		(theta) = pl_irg_thetas_iter_next(&(it)))

#define foreach_pl_etas(pli, irn, eta, it) \
	for(pl_etas_iter_init((pli), (irn), &(it)), \
		(eta) = pl_etas_iter_next(&(it)); \
		(eta); \
		(eta) = pl_etas_iter_next(&(it)))

#define foreach_pl_thetas(pli, irn, theta, it) \
	for(pl_thetas_iter_init((pli), (irn), &(it)), \
		(theta) = pl_thetas_iter_next(&(it)); \
		(theta); \
		(theta) = pl_thetas_iter_next(&(it)))

#define foreach_pl_border(pli, irn, ir_border, it) \
	for(pl_border_iter_init((pli), (irn), &(it)), \
		(ir_border) = pl_border_iter_next(&(it)); \
		(ir_border); \
		(ir_border) = pl_border_iter_next(&(it)))

#endif
