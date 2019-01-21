/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Some often needed tool-functions
 * @author    Michael Beck
 */
#ifndef FIRM_COMMON_IRTOOLS_H
#define FIRM_COMMON_IRTOOLS_H

#include "firm_types.h"
#include "lc_opts.h"
#include "pset.h"

/**
 * Return root commandlineoptions for libfirm library
 */
lc_opt_entry_t *firm_opt_get_root(void);

/**
 * The famous clear_link() walker-function.
 * Sets all links fields of visited nodes to NULL.
 * Do not implement it by yourself, use this one.
 */
void firm_clear_link(ir_node *n, void *env);

/**
 * The famous clear_link_and_block_lists() walker-function.
 * Sets all links fields of visited nodes to NULL.
 * Additionally, clear all Phi-lists of visited blocks.
 * Do not implement it by yourself, use this one.
 */
void firm_clear_node_and_phi_links(ir_node *n, void *env);

/**
 * Walker function, sets all phi list heads fields of visited Blocks
 * to NULL.
 * Use in conjunction with firm_collect_block_phis().
 */
void firm_clear_block_phis(ir_node *node, void *env);

/**
 * Walker function, links all visited Phi nodes into its block links.
 * Use in conjunction with firm_clear_block_phis().
 */
void firm_collect_block_phis(ir_node *node, void *env);

/**
 * This is a helper function used by some routines copying irg graphs
 * This assumes that we have "old" nodes which have been copied to "new"
 * nodes; The inputs of the new nodes still point to old nodes.
 *
 * Given an old(!) node this function rewires the matching new_node
 * so that all its inputs point to new nodes afterwards.
 */
void irn_rewire_inputs(ir_node *node);

#endif
