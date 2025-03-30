/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Loop data structure and access functions -- private stuff.
 * @author   Goetz Lindenmaier
 * @date     7.2002
 */
#ifndef FIRM_ANA_IRLOOP_T_H
#define FIRM_ANA_IRLOOP_T_H

#include "firm_common.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irloop.h"

#define is_ir_loop(thing)         _is_ir_loop(thing)
#define set_irg_loop(irg, loop)   _set_irg_loop(irg, loop)
#define get_irg_loop(irg)         _get_irg_loop(irg)
#define get_loop_outer_loop(loop) _get_loop_outer_loop(loop)
#define get_loop_depth(loop)      _get_loop_depth(loop)
#define get_irn_loop(n)           _get_irn_loop(n)

/**
 * The loops data structure.
 *
 * The loops data structure represents circles in the intermediate
 * representation.  It does not represent loops in the terms of a
 * source program.
 * Each ir_graph can contain one outermost loop data structure.
 * loop is the entry point to the nested loops.
 * The loop data structure contains a field indicating the depth of
 * the loop within the nesting.  Further it contains a list of the
 * loops with nesting depth -1.  Finally it contains a list of all
 * nodes in the loop.
 */
struct ir_loop {
	firm_kind       kind;             /**< A type tag, set to k_ir_loop. */
	unsigned        depth;            /**< Nesting depth */
	struct ir_loop *outer_loop;       /**< The outer loop */
	loop_element   *children;         /**< Mixed flexible array: Contains sons and loop_nodes */
	void *link;                       /**< link field. */
#ifdef DEBUG_libfirm
	long loop_nr;                     /**< A unique node number for each loop node to make output
	                                       readable. */
#endif
};

/**
 * Allocates a new loop as son of father on the given obstack.
 * If father is equal NULL, a new root loop is created.
 */
ir_loop *alloc_loop(ir_loop *father, struct obstack *obst);

/** Add a son loop to a father loop. */
void add_loop_son(ir_loop *loop, ir_loop *son);

/** Add a node to a loop. */
void add_loop_node(ir_loop *loop, ir_node *n);

/** Add an IR graph to a loop. */
void add_loop_irg(ir_loop *loop, ir_graph *irg);

/** Sets the loop a node belonging to. */
void set_irn_loop(ir_node *n, ir_loop *loop);

/**
 * Mature all loops by removing the flexible arrays of a loop tree
 * and putting them on the given obstack.
 */
void mature_loops(ir_loop *loop, struct obstack *obst);

/* -------- inline functions -------- */

static inline int _is_ir_loop(const void *thing)
{
	return get_kind(thing) == k_ir_loop;
}

static inline void _set_irg_loop(ir_graph *irg, ir_loop *loop)
{
	assert(irg);
	irg->loop = loop;
}

static inline ir_loop *_get_irg_loop(const ir_graph *irg)
{
	assert(irg);
	return irg->loop;
}

static inline ir_loop *_get_loop_outer_loop(const ir_loop *loop)
{
	assert(_is_ir_loop(loop));
	return loop->outer_loop;
}

static inline unsigned _get_loop_depth(const ir_loop *loop)
{
	assert(_is_ir_loop(loop));
	return loop->depth;
}

/* Uses temporary information to get the loop */
static inline ir_loop *_get_irn_loop(const ir_node *n)
{
	return n->loop;
}

#endif
