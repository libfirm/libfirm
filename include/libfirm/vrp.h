/*
 * Copyright (C) 1995-2009 University of Karlsruhe.  All right reserved.
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
 * @brief   Analyse the graph with value range propagation
 * @author  Jonas Fietz
 * @version $Id$
 *
 */
#include "firm_types.h"

#ifndef VRP_H
#define VRP_H

enum range_types {
	VRP_UNDEFINED, /* No information could be derived so far */
	VRP_RANGE, /* bottom and top form a range, including both values */
	VRP_ANTIRANGE, /* range from bottom to top can not be, but borders might
					  be */
	VRP_VARYING /* information can not be derived */
};

enum range_ops {
	VRP_NONE, /* range is defined absolute */
	VRP_ADD, /* range + range_node are the possible values */
	VRP_SUB /* range - range_node are the possible values */
};

/**
 * Set vrp data on the graph irg
 * @param irg graph on which to set vrp data
 */
void set_vrp_data(ir_graph *irg);

/**
 * Creates an ir_prog_pass for vrp
 *
 * @param name the name of this pass or NULL
 */
ir_graph_pass_t *set_vrp_pass(const char *name);

/**
 * Test, if the two nodes can be compared with their vrp information
 *
 * @param left: the left node
 * @param right: the right node
 *
 * @return the pn_Cmp, if one can be derived
 */
pn_Cmp vrp_cmp(ir_node *left, ir_node *right);

#endif
