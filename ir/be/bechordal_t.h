/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       Internal data structures for the chordal register allocator.
 * @author      Sebastian Hack
 * @date        25.01.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BECHORDAL_T_H
#define FIRM_BE_BECHORDAL_T_H

#include "firm_types.h"
#include "list.h"
#include "pmap.h"
#include "bitset.h"
#include "obst.h"
#include "debug.h"

#include "bechordal.h"
#include "beirg.h"
#include "beifg.h"

/** Defines an invalid register index. */
#define NO_COLOR (-1)

/**
 * A liveness interval border.
 */
struct border_t {
	DEBUG_ONLY(unsigned magic;)     /**< A magic number for checking. */
	struct list_head  list;         /**< list head for queuing. */
	border_t         *other_end;    /**< The other end of the border. */
	ir_node          *irn;          /**< The node. */
	unsigned         step;          /**< The number equal to the interval border. */
	unsigned         pressure;      /**< The pressure at this interval border. (The border itself is counting). */
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
	struct obstack       *obst;         /**< An obstack for temporary storage. */
	be_ra_chordal_opts_t *opts;         /**< A pointer to the chordal ra options. */
	be_irg_t             *birg;         /**< Back-end IRG session. */
	ir_graph             *irg;          /**< The graph under examination. */
	const arch_register_class_t *cls;   /**< The current register class. */
	pmap                 *border_heads; /**< Maps blocks to border heads. */
	be_ifg_t             *ifg;          /**< The interference graph. */
	bitset_t             *ignore_colors;/**< A set of colors which shall be ignored in register allocation. */
};

static INLINE struct list_head *_get_block_border_head(const be_chordal_env_t *inf, ir_node *bl) {
  return pmap_get(inf->border_heads, bl);
}

#define get_block_border_head(info, bl)     _get_block_border_head(info, bl)

#define foreach_border_head(head, pos)		list_for_each_entry_reverse(border_t, pos, head, list)
#define border_next(b)                      (list_entry((b)->list.next, border_t, list))
#define border_prev(b)                      (list_entry((b)->list.prev, border_t, list))

#define chordal_has_class(chordal_env, irn) \
	arch_irn_consider_in_reg_alloc(chordal_env->birg->main_env->arch_env, chordal_env->cls, irn)

void be_ra_chordal_color(be_chordal_env_t *chordal_env);

enum {
	/* Dump flags */
	BE_CH_DUMP_NONE       = (1 << 0),
	BE_CH_DUMP_SPILL      = (1 << 1),
	BE_CH_DUMP_LIVE       = (1 << 2),
	BE_CH_DUMP_COLOR      = (1 << 3),
	BE_CH_DUMP_COPYMIN    = (1 << 4),
	BE_CH_DUMP_SSADESTR   = (1 << 5),
	BE_CH_DUMP_TREE_INTV  = (1 << 6),
	BE_CH_DUMP_CONSTR     = (1 << 7),
	BE_CH_DUMP_SPILLSLOTS = (1 << 8),
	BE_CH_DUMP_LOWER      = (1 << 9),
	BE_CH_DUMP_APPEL      = (1 << 10),
	BE_CH_DUMP_ALL        = 2 * BE_CH_DUMP_APPEL - 1,

	/* lower perm options */
	BE_CH_LOWER_PERM_SWAP   = 1,
	BE_CH_LOWER_PERM_COPY   = 2,

	/* verify options */
	BE_CH_VRFY_OFF    = 1,
	BE_CH_VRFY_WARN   = 2,
	BE_CH_VRFY_ASSERT = 3,
};

struct be_ra_chordal_opts_t {
	int dump_flags;
	int lower_perm_opt;
	int vrfy_option;

	char ilp_server[128];
	char ilp_solver[128];
};

void be_pre_spill_prepare_constr(be_chordal_env_t *cenv);

#endif /* FIRM_BE_BECHORDAL_T_H */
