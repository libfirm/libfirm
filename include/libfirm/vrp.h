/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
	VRP_UNDEFINED, /**< No information could be derived so far */
	VRP_RANGE,     /**< bottom and top form a range, including both values */
	VRP_ANTIRANGE, /**< range from bottom to top can not be, but borders might
	                  be */
	VRP_VARYING    /**< information can not be derived */
};

/** VRP information */
typedef struct {
	int valid;                   /**< This node has valid vrp information */
	tarval *bits_set;            /**< The bits which, by analysis, are  definitely set.
	                                  0: may be not set, 1: definitely set*/
	tarval *bits_not_set;        /**< The bits which by analysis are definitely
	                                  not set, 1 for may be set, 0: definitely not set  */
	enum range_types range_type; /**< The range represented by range_top, range_bottom */
	tarval *range_bottom, *range_top;
} vrp_attr;

/**
 * Set vrp data on the graph irg
 * @param irg graph on which to set vrp data
 */
void set_vrp_data(ir_graph *irg);

/**
 * Test, if the two nodes can be compared with their vrp information
 *
 * @param left: the left node
 * @param right: the right node
 *
 * @return the pn_Cmp, if one can be derived
 */
pn_Cmp vrp_cmp(const ir_node *left, const ir_node *right);

/*
 * Return the vrp data for this node
 *
 * @param n: the node for which to return the vrp information
 *
 * @return a pointer to the vrp data or NULL if there is none
 */
vrp_attr *vrp_get_info(const ir_node *n);

#endif
