/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   control dependence analysis
 * @author  Christoph Mallon
 */
#ifndef FIRM_ANA_CDEP_H
#define FIRM_ANA_CDEP_H

#include "firm_types.h"

#include "begin.h"

/** @ingroup irana
 * @defgroup ir_cdep Control Dependence
 * @{
 */

/** Computes the control dependence graph for a graph. */
FIRM_API void compute_cdep(ir_graph *irg);

/** Frees the control dependence info. */
FIRM_API void free_cdep(ir_graph *irg);

/** Returns control dependent block */
FIRM_API ir_node *get_cdep_node(const ir_cdep *cdep);

/** Returns next entry in a list of cdeps */
FIRM_API ir_cdep *get_cdep_next(const ir_cdep *cdep);

/**
 * Returns a list of all control dependences of a block.
 */
FIRM_API ir_cdep *find_cdep(const ir_node *block);

/**
 * Replaces the control dependence info of old by the info of nw.
 */
FIRM_API void exchange_cdep(ir_node *old, const ir_node *nw);

/**
 * Checks whether dependee is (directly) control dependent on candidate.
 *
 * @param dependee   the (possible) dependent block
 * @param candidate  the (possible) block on which dependee is dependent
 */
FIRM_API int is_cdep_on(const ir_node *dependee, const ir_node *candidate);

/**
 * If block is control dependent on exactly one node, returns this node,
 * else NULL.
 *
 * @param block  the block to check
 */
FIRM_API ir_node *get_unique_cdep(const ir_node *block);

/**
 * Checks if the given block is control dependent of more than one node.
 *
 * @param block   the block to check
 */
FIRM_API int has_multiple_cdep(const ir_node *block);

/** @} */

#include "end.h"

#endif
