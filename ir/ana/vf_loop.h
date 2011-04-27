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
 * @brief   Compute loop information for VFirm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_ANA_VF_LOOP_H
#define FIRM_ANA_VF_LOOP_H

#include <stdio.h>
#include "firm_types.h"
#include "plist.h"

/**
 * Loop analysis. This collects all sorts of information about loops in VFirm
 * graphs, such as the nesting depth of nodes, the nodes in a loop, which eta
 * and theta nodes correspond to each other, loop invariant nodes etc.
 */

typedef struct vl_info vl_info;

typedef struct vl_edge {
	ir_node *src;
	ir_node *dst;
} vl_edge;

typedef plist_element_t *vl_eta_it;
typedef plist_element_t *vl_theta_it;
typedef plist_element_t *vl_node_eta_it;
typedef plist_element_t *vl_eta_theta_it;
typedef plist_element_t *vl_eta_invar_it;

/** Compute the loop information for the given irg. */
vl_info *vl_init(ir_graph *irg);

/** Free the loop information for the given tree. */
void vl_free(vl_info *vli);

/** Get the associated graph. */
ir_graph *vl_get_irg(vl_info *vli);

/** Get the loop depth of the given node. */
int vl_node_get_depth(vl_info *vli, ir_node *irn);

/** Iterate the eta nodes in the whole graph. */
void vl_eta_it_init(vl_info *vli, vl_eta_it *it);
ir_node *vl_eta_it_next(vl_eta_it *it);
int vl_get_eta_count(vl_info *vli);

/** Iterate the theta nodes in the whole graph. */
void vl_theta_it_init(vl_info *vli, vl_theta_it *it);
ir_node *vl_theta_it_next(vl_theta_it *it);
int vl_get_theta_count(vl_info *vli);

/** Iterate the eta nodes that (indirectly) access the given node. */
void vl_node_eta_it_init(vl_info *vli, vl_node_eta_it *it, ir_node *irn);
ir_node *vl_node_eta_it_next(vl_node_eta_it *it);
int vl_node_get_eta_count(vl_info *vli, ir_node *irn);

/** Iterate the theta nodes that correspond to the given eta node. */
void vl_eta_theta_it_init(vl_info *vli, vl_eta_theta_it *it, ir_node *eta);
ir_node *vl_eta_theta_it_next(vl_eta_theta_it *it);
int vl_eta_get_theta_count(vl_info *vli, ir_node *eta);

/** Iterate the invariant nodes that correspond to the given eta node. */
void vl_eta_invar_it_init(vl_info *vli, vl_eta_invar_it *it, ir_node *eta);
int vl_eta_invar_it_next(vl_eta_invar_it *it, vl_edge *edge);
int vl_eta_get_invar_count(vl_info *vli, ir_node *eta);

/** Dumps loop analysis debug information to the specified file. */
void vl_dump(vl_info *vli, FILE* f);

#define foreach_vl_eta(info, value, it) \
	for (vl_eta_it_init((info), &(it)), \
		(value) = vl_eta_it_next(&(it)); \
		(value); \
		(value) = vl_eta_it_next(&(it)))

#define foreach_vl_theta(info, value, it) \
	for (vl_theta_it_init((info), &(it)), \
		(value) = vl_theta_it_next(&(it)); \
		(value); \
		(value) = vl_theta_it_next(&(it)))

#define foreach_vl_node_eta(info, container, value, it) \
	for (vl_node_eta_it_init((info), &(it), (container)), \
		(value) = vl_node_eta_it_next(&(it)); \
		(value); \
		(value) = vl_node_eta_it_next(&(it)))

#define foreach_vl_eta_theta(info, container, value, it) \
	for (vl_eta_theta_it_init((info), &(it), (container)), \
		(value) = vl_eta_theta_it_next(&(it)); \
		(value); \
		(value) = vl_eta_theta_it_next(&(it)))

#define foreach_vl_eta_invar(info, container, value, it) \
	for (vl_eta_invar_it_init((info), &(it), (container)); \
		 vl_eta_invar_it_next(&(it), &(value));)

#endif
