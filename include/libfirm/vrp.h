/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Analyse the graph with value range propagation
 * @author  Jonas Fietz
 */
#ifndef VRP_H
#define VRP_H

#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup irana
 * @defgroup vrp  Value Information
 * Information about SSA-values (ranges, known bits, ...)
 * @{
 */

/** Type of a value range */
enum range_types {
	VRP_UNDEFINED, /**< No information could be derived so far */
	VRP_RANGE,     /**< bottom and top form a range, including both values */
	VRP_ANTIRANGE, /**< range from bottom to top cannot be, but borders might
	                    be */
	VRP_VARYING    /**< information cannot be derived */
};

/** VRP information for a single node */
typedef struct {
	ir_tarval *bits_set;         /**< The bits which, by analysis, are
	                                  definitely set:
	                                  0: may be not set, 1: definitely set */
	ir_tarval *bits_not_set;     /**< The bits which by analysis are definitely
	                                  not set:
	                                  1 for may be set, 0: definitely not set */
	enum range_types range_type; /**< The range represented by range_top, range_bottom */
	ir_tarval *range_bottom;     /**< lower end of the value range */
	ir_tarval *range_top;        /**< upper end of the value range */
} vrp_attr;

/**
 * Sets vrp data on the graph irg
 * @param irg graph on which to set vrp data
 */
FIRM_API void set_vrp_data(ir_graph *irg);

/**
 * free vrp infos in an irg
 */
FIRM_API void free_vrp_data(ir_graph *irg);

/**
 * Test, if the two nodes can be compared with their vrp information
 *
 * @param left: the left node
 * @param right: the right node
 * @return all possible relations
 */
FIRM_API ir_relation vrp_cmp(const ir_node *left, const ir_node *right);

/**
 * Returns the vrp data for this node
 * Note: only allowed for nodes with an integer mode!
 *
 * @param n: the node for which to return the vrp information
 * @return a pointer to the vrp data or NULL if there is none
 */
FIRM_API vrp_attr *vrp_get_info(const ir_node *n);

/** @} */

#include "end.h"

#endif
