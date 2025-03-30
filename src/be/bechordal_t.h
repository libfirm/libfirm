/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Internal data structures for the chordal register allocator.
 * @author      Sebastian Hack
 * @date        25.01.2005
 */
#ifndef FIRM_BE_BECHORDAL_T_H
#define FIRM_BE_BECHORDAL_T_H

#include "firm_types.h"
#include "list.h"
#include "pmap.h"
#include "bitset.h"
#include "obst.h"

#include "bechordal.h"
#include "beifg.h"

/**
 * A liveness interval border.
 */
struct border_t {
	struct list_head  list;         /**< list head for queuing. */
	ir_node          *irn;          /**< The node. */
	unsigned         is_def  : 1;   /**< Does this border denote a use or a def. */
	unsigned         is_real : 1;   /**< Is the def/use real? Or is it just
	                                     inserted at block beginnings or ends
	                                     to ensure that inside a block, each
	                                     value has one begin and one end. */
};

/**
 * Environment for each of the chordal register allocator phases
 */
struct be_chordal_env_t {
	struct obstack        obst;         /**< An obstack for temporary storage. */
	ir_graph             *irg;          /**< The graph under examination. */
	const arch_register_class_t *cls;   /**< The current register class. */
	pmap                 *border_heads; /**< Maps blocks to border heads. */
	be_ifg_t             *ifg;          /**< The interference graph. */
	bitset_t             *allocatable_regs; /**< set of allocatable registers */
};

static inline struct list_head *get_block_border_head(be_chordal_env_t const *const inf, ir_node *const bl)
{
  return pmap_get(list_head, inf->border_heads, bl);
}

#define foreach_border_head(head, pos)      list_for_each_entry_reverse(border_t, pos, head, list)

enum {
	/* Dump flags */
	BE_CH_DUMP_NONE     = 1 << 0,
	BE_CH_DUMP_SPILL    = 1 << 1,
	BE_CH_DUMP_COLOR    = 1 << 2,
	BE_CH_DUMP_COPYMIN  = 1 << 3,
	BE_CH_DUMP_SSADESTR = 1 << 4,
	BE_CH_DUMP_CONSTR   = 1 << 5,
	BE_CH_DUMP_LOWER    = 1 << 6,
	BE_CH_DUMP_ALL      = 2 * BE_CH_DUMP_LOWER - 1,

	/* lower perm options */
	BE_CH_LOWER_PERM_SWAP   = 1,
	BE_CH_LOWER_PERM_COPY   = 2,
};

struct be_ra_chordal_opts_t {
	unsigned dump_flags;
	int      lower_perm_opt;
};

void be_chordal_dump(unsigned mask, ir_graph *irg, arch_register_class_t const *cls, char const *suffix);

void check_for_memory_operands(ir_graph *irg, const regalloc_if_t *regif);

#endif
