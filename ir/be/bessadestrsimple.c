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
 * @brief       Simple SSA destruction.
 * @author      Daniel Grund
 * @date        17.01.2006
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "set.h"
#include "pset.h"
#include "pmap.h"
#include "bitset.h"
#include "xmalloc.h"

#include "irprintf_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "irdom_t.h"
#include "phiclass.h"

#include "beraextern.h"
#include "beabi.h"
#include "bearch_t.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "beutil.h"
#include "belive_t.h"
#include "beinsn_t.h"

#include "bessadestrsimple.h"

#define DBG_LEVEL 2

typedef struct _ssa_destr_env_t {
	ir_graph                    *irg;
	const arch_register_class_t *cls;
	const arch_env_t            *aenv;
	set                         *vars;
} ssa_destr_env_t;

#define pset_foreach(pset, irn)  for(irn=pset_first(pset); irn; irn=pset_next(pset))
#define set_foreach(set, e)  for(e=set_first(set); e; e=set_next(set))

static phi_classes_t *pc = NULL;

/******************************************************************************
   __      __   _       ___   __      __
   \ \    / /  | |     |__ \  \ \    / /
    \ \  / /_ _| |___     ) |  \ \  / /_ _ _ __ ___
     \ \/ / _` | / __|   / /    \ \/ / _` | '__/ __|
      \  / (_| | \__ \  / /_     \  / (_| | |  \__ \
       \/ \__,_|_|___/ |____|     \/ \__,_|_|  |___/
 *****************************************************************************/

/**
 * The link field of an irn points to the var_info struct
 * representing the corresponding variable.
 */
#define set_var_info(irn, vi)				set_irn_link(irn, vi)

#define HASH_VAR_NR(var_nr) var_nr

static int compare_var_infos(const void *e1, const void *e2, size_t size) {
	const be_var_info_t *v1 = e1;
	const be_var_info_t *v2 = e2;

	if (v1->var_nr == SET_REMOVED || v2->var_nr == SET_REMOVED)
		return 1;

	return v1->var_nr != v2->var_nr;
}

be_var_info_t *be_var_find(set *vars, int var_nr) {
	be_var_info_t vi;
	vi.var_nr = var_nr;

	return set_find(vars, &vi, sizeof(vi), HASH_VAR_NR(var_nr));
}

be_var_info_t *be_var_find_or_insert(set *vars, int var_nr) {
	be_var_info_t vi, *found;
	memset(&vi, 0, sizeof(vi));
	vi.var_nr = var_nr;

	found = set_insert(vars, &vi, sizeof(vi), HASH_VAR_NR(var_nr));

	if (!found->values)
		found->values  = pset_new_ptr(1);

	return found;
}

/**
 * Adds a value to a variable. Sets all pointers accordingly.
 */
be_var_info_t *be_var_add_value(set *vars, int var_nr, ir_node *irn) {
	be_var_info_t *vi = be_var_find_or_insert(vars, var_nr);

	/* var 2 value mapping */
	pset_insert_ptr(vi->values, irn);

	/* value 2 var mapping */
	set_var_info(irn, vi);

	return vi;
}

pset *be_get_var_values(set *vars, int var_nr) {
	be_var_info_t *vi = be_var_find(vars, var_nr);
	assert(vi && "Variable does not exist");
	return vi->values;
}

static INLINE ir_node *get_first_phi(ir_node **s) {
	int i;

	for (i = ARR_LEN(s) - 1; i >= 0; --i) {
		if (is_Phi(s[i]))
			return s[i];
	}

	assert(0 && "There must be a phi in this");
	return NULL;
}


/**
 * Define variables (numbers) for all SSA-values.
 * All values in a phi class get assigned the same variable name.
 * The link field maps values to the var-name
 */
static void values_to_vars(ir_node *irn, void *env) {
	ssa_destr_env_t *sde = env;
	int             nr, i, build_vals = 0;
	ir_node         **vals;

	if (arch_get_irn_reg_class(sde->aenv, irn, -1) == NULL)
		return;

	vals = get_phi_class(pc, irn);

	if (vals) {
		nr = get_irn_node_nr(get_first_phi(vals));
	} else {
		/* not a phi class member, value == var */
		nr         = get_irn_node_nr(irn);
		vals       = NEW_ARR_F(ir_node *, 1);
		vals[0]    = irn;
		build_vals = 1;
	}

	/* values <--> var mapping */
	for (i = ARR_LEN(vals) - 1; i >= 0; --i) {
		be_var_add_value(sde->vars, nr, vals[i]);
	}

	if (build_vals)
		DEL_ARR_F(vals);
}

/******************************************************************************
     _____ _____              _____            _
    / ____/ ____|  /\        |  __ \          | |
   | (___| (___   /  \ ______| |  | | ___  ___| |_ _ __
    \___ \\___ \ / /\ \______| |  | |/ _ \/ __| __| '__|
    ____) |___) / ____ \     | |__| |  __/\__ \ |_| |
   |_____/_____/_/    \_\    |_____/ \___||___/\__|_|

 *****************************************************************************/

#define mark_as_done(irn, pos)			set_irn_link(irn, INT_TO_PTR(pos+1))
#define has_been_done(irn, pos)			(PTR_TO_INT(get_irn_link(irn)) > pos)

/**
 * Insert a copy for the argument of @p start_phi found at position @p pos.
 * Also searches a phi-loop of arbitrary length to detect and resolve
 *   the class of phi-swap-problems. To search for a loop recursion is used.
 *
 * 1) Simplest case (phi with a non-phi arg):
 *     A single copy is inserted.
 *
 * 2) Phi chain (phi (with phi-arg)* with non-phi arg):
 *     Several copies are placed, each after returning from recursion.
 *
 * 3) Phi-loop:
 *     On detection a loop breaker is inserted, which is a copy of the start_phi.
 *     This copy then pretends beeing the argumnent of the last phi.
 *     Now case 2) can be used.
 *
 * The values of @p start_phi and @p pos never change during recursion.
 *
 * @p start_phi  Phi node to process
 * @p pos        Argument position to insert copy/copies for
 * @p curr_phi   Phi node currently processed during recursion. Equals start_phi on initial call
 *
 * @return NULL  If no copy is necessary
 *         NULL  If the phi has already been processed at this pos
 *               Link field is used to keep track of processed positions
 *         In all other cases the ir_node *copy which was placed is returned.
 */
static ir_node *insert_copies(ssa_destr_env_t *sde, const arch_register_class_t *cls, ir_node *start_phi, int pos, ir_node *curr_phi) {
	ir_node *arg = get_irn_n(curr_phi, pos);
	ir_node *arg_blk = get_nodes_block(arg);
	ir_node *pred_blk = get_Block_cfgpred_block(get_nodes_block(curr_phi), pos);
	ir_node *curr_cpy, *last_cpy;

	assert(is_Phi(start_phi) && is_Phi(curr_phi));

	if (has_been_done(start_phi, pos))
		return NULL;

	/* In case this is a 'normal' phi we insert at the
	 * end of the pred block before cf nodes */
	last_cpy = sched_skip(pred_blk, 0, sched_skip_cf_predicator, (void *)sde->aenv);
	last_cpy = sched_next(last_cpy);

	/* If we detect a loop stop recursion. */
	if (arg == start_phi) {
		ir_node *loop_breaker;
		if (start_phi == curr_phi) {
			/* Phi directly uses itself. No copy necessary */
			return NULL;
		}

		/* At least 2 phis are involved */
		/* Insert a loop breaking copy (an additional variable T) */
		loop_breaker = be_new_Copy(cls, sde->irg, pred_blk, start_phi);
		sched_add_before(last_cpy, loop_breaker);

		arg = loop_breaker;
	}

	/* If arg is a phi in the same block we have to continue search */
	if (is_Phi(arg) && arg_blk == get_nodes_block(start_phi))
		last_cpy = insert_copies(sde, cls, start_phi, pos, arg);

	/* Insert copy of argument (may be the loop-breaker) */
	curr_cpy = be_new_Copy(cls, sde->irg, pred_blk, arg);
	set_irn_n(curr_phi, pos, curr_cpy);
	mark_as_done(curr_phi, pos);
	sched_add_before(last_cpy, curr_cpy);
	return curr_cpy;
}


/**
 * Perform simple SSA-destruction with copies.
 * The order of processing _must_ be
 *  for all positions {
 *    for all phis {
 *      doit
 *    }
 *  }
 * else the magic to keep track of processed phi-positions will fail in
 * function 'insert_copies'
 */
static void ssa_destr_simple_walker(ir_node *blk, void *env) {
	ssa_destr_env_t *sde = env;
	int pos, max;
	ir_node *phi;
	const arch_register_class_t *cls;

	/* for all argument positions of the phis */
	for (pos=0, max=get_irn_arity(blk); pos<max; ++pos) {

		/* for all phi nodes (which are scheduled first) */
		sched_foreach(blk, phi) {
			if (!is_Phi(phi))
				break;

			if (arch_irn_is(sde->aenv, phi, ignore))
				continue;

			cls = arch_get_irn_reg_class(sde->aenv, phi, -1);
			insert_copies(sde, cls, phi, pos, phi);
		}
	}
}


set *be_ssa_destr_simple(ir_graph *irg, const arch_env_t *aenv) {
	ssa_destr_env_t sde;

	sde.irg = irg;
	sde.aenv = aenv;
	sde.vars = new_set(compare_var_infos, 16);

	be_clear_links(irg);
	irg_block_walk_graph(irg, ssa_destr_simple_walker, NULL, &sde);

	/* Mapping of SSA-Values <--> Variables */
	pc = phi_class_new_from_irg(irg, 0);
	be_clear_links(irg);
	irg_walk_graph(irg, values_to_vars, NULL, &sde);

	return sde.vars;
}

void free_ssa_destr_simple(set *vars)
{
  be_var_info_t *vi;

  set_foreach(vars, vi)
    del_pset(vi->values);

  del_set(vars);
  phi_class_free(pc);
}
