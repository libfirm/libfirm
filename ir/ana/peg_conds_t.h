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
#include "pmap_new.h"
#include "plist.h"

typedef struct gc_cond  gc_cond;
typedef struct gc_union gc_union;
typedef struct gc_info  gc_info;

typedef pmap_new_iterator_t  gc_union_map_iter;
typedef plist_element_t     *gc_union_iter;

typedef struct gc_entry {
	ir_node  *dst;
	gc_union *uni;
} gc_entry;

typedef enum gc_type {
	gct_while_true  = 0,
	gct_if_true     = 1,
	gct_if_false    = 2,
	gct_last_cached = gct_if_false,
	gct_once        = 3,
	gct_invalid     = 4
} gc_type;

/* Turn the PEG graph into an AcPEG. */
void peg_to_acpeg(ir_graph *irg, pl_info *pli);

/** Compute the gating conditions for the given irg. */
gc_info *gc_init(ir_graph *irg, pd_tree *pdt, pl_info *pli);

/** Free the gating conditions for the given tree. */
void gc_free(gc_info *gci);

/** Dumps the gating conditions to the specified file. */
void gc_dump(gc_info *gci, FILE* f);

/** Initialize an iterator to iterate the union map on a node. */
void gc_union_map_iter_init(gc_info *gci, ir_node *irn, gc_union_map_iter *it);

/** Get the next element from the given iterator. */
gc_entry gc_union_map_iter_next(gc_union_map_iter *it);

/** Initialize an iterator to iterate a union. */
void gc_union_iter_init(gc_union *uni, gc_union_iter *it);

/** Get the next element from the given iterator. */
gc_cond *gc_union_iter_next(gc_union_iter *it);

/** Determine if the given entry is a null object. */
int gc_entry_is_null(gc_entry entry);

/** Get the type of the given gating condition. */
gc_type gc_get_cond_type(gc_cond *cond);

/** Get the node of the given gating condition. */
ir_node *gc_get_cond_irn(gc_cond *cond);

/** Get the tail of the gating condition. */
gc_cond *gc_get_cond_next(gc_cond *cond);

/** Check whether the given union is empty. */
int gc_union_is_empty(gc_union *uni);

#define foreach_gc_union_map(gci, irn, entry, it) \
	for(gc_union_map_iter_init((gci), (irn), &(it)), \
		(entry) = gc_union_map_iter_next(&(it)); \
		!gc_entry_is_null((entry)); \
		(entry) = gc_union_map_iter_next(&(it)))

#define foreach_gc_union(uni, cond, it) \
	for(gc_union_iter_init((uni), &(it)), \
		(cond) = gc_union_iter_next(&(it)); \
		(cond); \
		(cond) = gc_union_iter_next(&(it)))

#endif
