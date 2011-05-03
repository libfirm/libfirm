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
 * @brief   Gating condition analysis for VFirm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_ANA_VF_COND_H
#define FIRM_ANA_VF_COND_H

#include <stdio.h>
#include "firm_types.h"
#include "vf_dom.h"
#include "vf_formula.h"

/**
 * The analysis starts at a given root node in an acyclic "weakened" VFirm
 * graph and determines the conditions of all nodes that are dominated by the
 * root node. The result is a tree of conditions, that resembles the dominance
 * tree. Every node contains a condition that is relative to its immediate
 * dominator. The root node has a "true" condition.
 *
 * Conditions can be accessed by the vc_node_get_rel_cond function. This will
 * return the dominator and the relative condition.
 *
 * Calculation requires dominance information and the formula subsystem. Access
 * to those is provided by vc_get_vd_info and vc_get_vf_info. So it is easy to
 * stack further analysis/modification subsystems on top of the results.
 */

typedef struct vc_info vc_info;

/* A condition relative to a given parent node. */
typedef struct vc_rel_cond {
	vf_cond  cond;
	ir_node *idom;
} vc_rel_cond;

/**
 * Compute the gating conditions below the given root. If "keep_block" is true,
 * analysis will be restricted to the root nodes block.
 */
vc_info *vc_init_root(ir_node *root, int keep_block);

/** Free the gating conditions for the given tree. */
void vc_free(vc_info *vci);

/** Get the associated graph. */
ir_graph *vc_get_irg(vc_info *vci);

/** Get the computation root node. */
ir_node *vc_get_root(vc_info *vci);

/** Get the computed dominance information. */
vd_info *vc_get_vd_info(vc_info *vci);

/** Get the formula information. */
vf_info *vc_get_vf_info(vc_info *vci);

/** Dumps gating condition debug information to the specified file. */
void vc_dump(vc_info *vci, FILE *f);

/** Gets the relative condition of irn and stores it in cond (if found). */
int vc_node_get_rel_cond(vc_info *vci, ir_node *irn, vc_rel_cond *rel_cond);

#endif
