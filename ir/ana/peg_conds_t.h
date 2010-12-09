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
 * @brief   Compute gating conditions in AcPEG graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_ANA_PEG_CONDS_T_H
#define FIRM_ANA_PEG_CONDS_T_H

#include <stdio.h>
#include "firm_types.h"
#include "peg_dom_t.h"
#include "peg_loop_t.h"

typedef enum gc_type {
	gct_demand, /* lambda */
	gct_ignore, /* emptyset */
	gct_branch, /* gamma(g, a, b) */
	gct_union,  /* a u b */
	gct_repeat
} gc_type;

typedef struct gc_cond gc_cond;
typedef struct gc_info gc_info;
typedef void* gc_iter;

/** A gating condition and an associated target node. */
typedef struct gc_entry {
	gc_cond *cond;
	ir_node *dst;
} gc_entry;

/* Turn the PEG graph into an AcPEG. */
void peg_to_acpeg(ir_graph *irg, pl_info *pli);

/** Compute the gating conditions for the given irg. */
gc_info *gc_init(ir_graph *irg, pd_tree *pdt, pl_info *pli);

/** Free the gating conditions for the given tree. */
void gc_free(gc_info *gci);

/** Dumps the gating conditions to the specified file. */
void gc_dump(gc_info *gci, FILE* f);

/** Get the gating condition for dst, stored on src. */
gc_cond *gc_get_cond_for(gc_info *gci, ir_node *src, ir_node *dst);

/**
 * Get the first gating condition/target pair stored on the given irn. For the
 * "it" parameter either pass NULL (to only get one node), or a pointer to an
 * allocated gc_iter, to use with gc_iter_cond_next.
 *
 * If this behaves strangely, blame pmap for now.
 */
gc_entry gc_get_cond(gc_info *gci, ir_node *irn, gc_iter *it);

/** Get the next condition from the given condition iterator. */
gc_entry gc_cond_iter_next(gc_iter *it);

/** Get the number of gating condition/target pairs stored on the given irn. */
int gc_get_cond_count(gc_info *gci, ir_node *irn);

/** Get the type of the given gating condition. */
gc_type gc_get_cond_type(gc_cond *cond);

/** Get the irn associated with the branch. */
ir_node *gc_get_branch_irn(gc_cond *cond);
/** Get the left-hand side (true side) of the branch. */
gc_cond *gc_get_branch_lhs(gc_cond *cond);
/** Get the right-hand side (false side) of the branch. */
gc_cond *gc_get_branch_rhs(gc_cond *cond);

/** Get the irn associated with the repeat. */
ir_node *gc_get_repeat_irn(gc_cond *cond);
/** Get the inner condition of the repeat. */
gc_cond *gc_get_repeat_cond(gc_cond *cond);

/** Get the left-hand side of the union (may be a union itself). */
gc_cond *gc_get_union_lhs(gc_cond *cond);
/** Get the right-hand side of the union (will not be a union). */
gc_cond *gc_get_union_rhs(gc_cond *cond);

/**
 * Get the first gating condition in the given union. For the "it" parameter
 * either pass NULL (to only get one condition), or a pointer to an allocated
 * gc_iter, to use with gc_iter_union_next.
 */
gc_cond *gc_get_union_cond(gc_cond *cond, gc_iter *it);

/** Get the next condition from the given union iterator. */
gc_cond *gc_union_iter_next(gc_iter *it);

#define foreach_gc_union(gcu, it, cond) \
	for ((cond) = gc_get_union_cond((gcu), &(it)); \
		(cond); (cond) = gc_union_iter_next(&(it)))

#define foreach_gc_conds(gci, irn, it, entry) \
	for ((entry) = gc_get_cond((gci), (irn), &(it)); \
		(entry).cond; (entry) = gc_cond_iter_next(&(it)))

#endif
