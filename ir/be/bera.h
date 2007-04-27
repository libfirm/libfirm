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
 * Register allocation functions.
 * @author Sebastian Hack
 * @date 13.1.2005
 */
#ifndef _BERA_H
#define _BERA_H

#include "firm_config.h"

#include <libcore/lc_timing.h>

#include "firm_types.h"

#include "be.h"
#include "belive.h"
#include "beirg.h"
#include "bemodule.h"

typedef struct {
	lc_timer_t *t_prolog;      /**< timer for prolog */
	lc_timer_t *t_epilog;      /**< timer for epilog */
	lc_timer_t *t_live;        /**< timer for liveness calculation */
	lc_timer_t *t_spill;       /**< timer for spilling */
	lc_timer_t *t_spillslots;  /**< spillslot coalescing */
	lc_timer_t *t_color;       /**< timer for graph coloring */
	lc_timer_t *t_ifg;         /**< timer for building interference graph */
	lc_timer_t *t_copymin;     /**< timer for copy minimization */
	lc_timer_t *t_ssa;         /**< timer for ssa destruction */
	lc_timer_t *t_verify;      /**< timer for verification runs */
	lc_timer_t *t_other;       /**< timer for remaining stuff */
} be_ra_timer_t;

extern be_ra_timer_t *global_ra_timer;

typedef struct be_ra_t {
	void (*allocate)(be_irg_t *bi);   /**< allocate registers on a graph */
} be_ra_t;

void be_register_allocator(const char *name, be_ra_t *allocator);

/**
 * Do register allocation with currently selected register allocator
 */
void be_allocate_registers(be_irg_t *birg);

/**
 * Check, if two values interfere.
 * @param lv Liveness information.
 * @param a The first value.
 * @param b The second value.
 * @return 1, if @p a and @p b interfere, 0 if not.
 */
int values_interfere(const be_lv_t *lv, const ir_node *a, const ir_node *b);

/**
 * Check, if a value dominates the other one.
 * Note, that this function also considers the schedule and does thus
 * more than block_dominates().
 *
 * @param a The first.
 * @param b The second value.
 * @return 1 if a dominates b, 0 else.
 */
int value_dominates(const ir_node *a, const ir_node *b);

/**
 * Like value_dominates(), but the nodes have to be in the same block
 */
int value_dominates_intrablock(const ir_node *a, const ir_node *b);

#endif /* _BERA_H */
