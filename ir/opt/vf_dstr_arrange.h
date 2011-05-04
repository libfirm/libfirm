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
 * @brief   Arranging VFirm graphs for Firm construction.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#ifndef FIRM_OPT_VF_DSTR_ARRANGE_H
#define FIRM_OPT_VF_DSTR_ARRANGE_H

#include "firm_types.h"
#include "plist.h"
#include "pmap_new.h"
#include "vf_cond.h"
#include "vf_depth.h"

typedef struct va_info    va_info;
typedef struct va_region  va_region;

typedef int va_loop_loop_it;

typedef struct va_region_child_it {
	char                has_map;
	pmap_new_iterator_t it;
} va_region_child_it;

/**
 * Calculate arrangement information. If "keep_block" is true, the analysis is
 * restricted to the root nodes block. The loop depth info can be provided and
 * will be used, to keep depth information up-to-date.
 */
va_info *va_init_root(vl_info *vli, ir_node *root, int keep_block);

/** Free the arrangement information. */
void va_free(va_info *vai);

/** Get the computation root node. */
ir_node *va_get_root(va_info *vai);

/** Get the associated graph. */
ir_graph *va_get_irg(va_info *vai);

/** Get the root region of the arranged regions. */
va_region *va_get_root_region(va_info *vai);

/** Get the computed condition information. */
vc_info *va_get_vc_info(va_info *vai);

/** Determine if the given region contains any loops. */
int va_region_has_loops(va_region *region);

/** Get the region that was assigned to the given node. */
va_region *va_node_get_region(va_info *vai, ir_node *irn);

/** Get the regions parent region. */
va_region *va_region_get_parent(va_region *region);

/** Get the condition node of the given region. NULL for the root regions. */
ir_node *va_region_get_cond(va_region *region);

/** Get the value that the condition has to satisfy for the region. */
int va_region_get_value(va_region *region);

/** Get the linked region for the given one or NULL. */
va_region *va_region_get_link(va_region *region);

/** Get the predecessor region for the given one or NULL. */
va_region *va_region_get_pred(va_region *region);

/** Get one of the branch regions associated with a gamma node. */
va_region *va_gamma_get_branch_region(va_info *vai, ir_node *gamma);

/** Iterate the child regions of a region. */
void va_region_child_it_init(va_region_child_it *it, va_region *region);
va_region *va_region_child_it_next(va_region_child_it *it);
int va_region_get_child_count(va_region *region);

/** Dump arrangement debug information to the given file. */
void va_dump(va_info *vai, FILE *f);

#define foreach_va_loop(info, value, it) \
	for(va_loop_it_init((info), &(it)), \
		(value) = va_loop_it_next(&(it)); \
		(value); \
		(value) = va_loop_it_next(&(it)))

#define foreach_va_region_child(container, value, it) \
	for(va_region_child_it_init(&(it), (container)), \
		(value) = va_region_child_it_next(&(it)); \
		(value); \
		(value) = va_region_child_it_next(&(it)))

/** Iteration macros for Loop nodes. They don't really fit anywhere else. */
#define foreach_va_loop_loop(first_loop, loop, it) \
	for ((loop) = (first_loop), (it) = 1; \
		(it); \
		(loop) = get_Loop_next(loop), (it) = ((loop) != (first_loop)))

#define foreach_va_loop_eta(first_loop, loop, eta, it) \
	for ((loop) = (first_loop), (it) = 1; \
		(eta) = get_Loop_eta(loop), (it); \
		(loop) = get_Loop_next(loop), (it) = ((loop) != (first_loop)))

#endif
