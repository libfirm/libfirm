/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Lowering of Switches if necessary or advantageous.
 * @author  Moritz Kroll
 * @version $Id$
 */
#include "config.h"

#include <limits.h>
#include <stdbool.h>

#include "array_t.h"
#include "ircons.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irouts.h"
#include "irpass_t.h"
#include "lowering.h"
#include "error.h"
#include "irnodeset.h"

#define foreach_out_irn(irn, i, outirn) for (i = get_irn_n_outs(irn) - 1;\
	i >= 0 && (outirn = get_irn_out(irn, i)); --i)

typedef struct walk_env_t {
	unsigned      spare_size; /**< the allowed spare size for table switches */
	bool          allow_out_of_bounds;
	bool          changed;    /**< indicates whether a change was performed */
	ir_nodeset_t  processed;
} walk_env_t;

typedef struct case_data_t {
	long     value;
	ir_node *target;
} case_data_t;

typedef struct cond_env_t {
	ir_node  *sel;
	long      switch_min;
	long      switch_max;
	ir_node  *default_block;
	unsigned  num_cases;
	ir_node **defusers;           /**< the Projs pointing to the default case */
} cond_env_t;

static void analyse_switch(cond_env_t *env, ir_node *cond)
{
	long     default_pn    = get_Cond_default_proj(cond);
	long     switch_min    = LONG_MAX;
	long     switch_max    = LONG_MIN;
	ir_node *default_block = NULL;
	unsigned num_cases     = 0;
	int      i;
	ir_node *proj;

	foreach_out_irn(cond, i, proj) {
		long pn = get_Proj_proj(proj);
		if (pn == default_pn) {
			ir_node *target = get_irn_out(proj, 0);
			assert(default_block == NULL || default_block == target);
			default_block = target;
			continue;
		}

		if (pn < switch_min)
			switch_min = pn;
		if (pn > switch_max)
			switch_max = pn;
		++num_cases;
	}
	assert(default_block != NULL);

	env->switch_min    = switch_min;
	env->switch_max    = switch_max;
	env->num_cases     = num_cases;
	env->default_block = default_block;
}

static int casecmp(const void *a, const void *b)
{
	const case_data_t *cda = (const case_data_t*)a;
	const case_data_t *cdb = (const case_data_t*)b;

	/*
	 * Enforce unsigned sorting. Signed comparison will behave differently for
	 * 32-bit values, depending on sizeof(long). This will make the resulting
	 * array deterministic.
	 */
	return ((unsigned long)cda->value > (unsigned long)cdb->value) -
	       ((unsigned long)cda->value < (unsigned long)cdb->value);
}

/**
 * Creates an if cascade realizing binary search.
 */
static void create_if_cascade(cond_env_t *env, dbg_info *dbgi, ir_node *block,
                              case_data_t *curcases, unsigned numcases)
{
	ir_graph *irg = get_irn_irg(block);
    ir_mode  *cmp_mode;
    ir_node  *cmp_sel;
    ir_node  *sel_block;

    /* Get the mode and sel node for the comparison. */
    cmp_mode  = get_irn_mode(env->sel);
    cmp_sel   = env->sel;
    sel_block = get_nodes_block(cmp_sel);

    /*
     * Make sure that an unsigned comparison is used, by converting the sel
     * node to an unsigned mode and using that mode for the constants, too.
     * This is important, because the qsort applied to the case labels uses
     * an unsigned comparison and both comparison methods have to match.
     */
    if (mode_is_signed(cmp_mode)) {
        cmp_mode = find_unsigned_mode(cmp_mode);
        cmp_sel  = new_r_Conv(sel_block, cmp_sel, cmp_mode);
    }

	if (numcases == 0) {
		/* zero cases: "goto default;" */
		ARR_APP1(ir_node*, env->defusers, new_r_Jmp(block));
	} else if (numcases == 1) {
		/* only one case: "if (sel == val) goto target else goto default;" */
		ir_node *val       = new_r_Const_long(irg, cmp_mode, curcases[0].value);
		ir_node *cmp       = new_rd_Cmp(dbgi, block, cmp_sel, val);
		ir_node *proj      = new_r_Proj(cmp, mode_b, pn_Cmp_Eq);
		ir_node *cond      = new_rd_Cond(dbgi, block, proj);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);

		set_Block_cfgpred(curcases[0].target, 0, trueproj);
		ARR_APP1(ir_node*, env->defusers, falseproj);
	} else if (numcases == 2) {
		/* only two cases: "if (sel == val[0]) goto target[0];" */
		ir_node *val       = new_r_Const_long(irg, cmp_mode, curcases[0].value);
		ir_node *cmp       = new_rd_Cmp(dbgi, block, cmp_sel, val);
		ir_node *proj      = new_r_Proj(cmp, mode_b, pn_Cmp_Eq);
		ir_node *cond      = new_rd_Cond(dbgi, block, proj);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);
		ir_node *in[1];
		ir_node *neblock;

		set_Block_cfgpred(curcases[0].target, 0, trueproj);

		in[0] = falseproj;
		neblock = new_r_Block(irg, 1, in);

		/* second part: "else if (sel == val[1]) goto target[1] else goto default;" */
		val       = new_r_Const_long(irg, cmp_mode, curcases[1].value);
		cmp       = new_rd_Cmp(dbgi, neblock, cmp_sel, val);
		proj      = new_r_Proj(cmp, mode_b, pn_Cmp_Eq);
		cond      = new_rd_Cond(dbgi, neblock, proj);
		trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);
		set_Block_cfgpred(curcases[1].target, 0, trueproj);
		ARR_APP1(ir_node*, env->defusers, falseproj);
	} else {
		/* recursive case: split cases in the middle */
		int midcase = numcases / 2;
		ir_node *val  = new_r_Const_long(irg, cmp_mode,
		                                 curcases[midcase].value);
		ir_node *cmp  = new_rd_Cmp(dbgi, block, cmp_sel, val);
		ir_node *proj = new_r_Proj(cmp, mode_b, pn_Cmp_Lt);
		ir_node *cond = new_rd_Cond(dbgi, block, proj);
		ir_node *in[1];
		ir_node *ltblock;
		ir_node *geblock;

		in[0]   = new_r_Proj(cond, mode_X, pn_Cond_true);
		ltblock = new_r_Block(irg, 1, in);

		in[0]   = new_r_Proj(cond, mode_X, pn_Cond_false);
		geblock = new_r_Block(irg, 1, in);

		create_if_cascade(env, dbgi, ltblock, curcases, midcase);
		create_if_cascade(env, dbgi, geblock, curcases + midcase,
		                  numcases - midcase);
	}
}

static void create_out_of_bounds_check(cond_env_t *env, ir_node *cond)
{
	ir_graph      *irg           = get_irn_irg(cond);
	dbg_info      *dbgi          = get_irn_dbg_info(cond);
	ir_node       *sel           = get_Cond_selector(cond);
	ir_node       *block         = get_nodes_block(cond);
	ir_mode       *cmp_mode      = get_irn_mode(sel);
	ir_node      **default_preds = NEW_ARR_F(ir_node*, 0);
	unsigned long  default_pn    = get_Cond_default_proj(cond);
	long           delta         = 0;
	ir_node       *max_const;
	ir_node       *proj_true;
	ir_node       *proj_false;
	ir_node       *cmp;
	ir_node       *proj_cmp;
	ir_node       *oob_cond;
	ir_node       *in[1];
	ir_node       *new_block;
	size_t         n_default_preds;
	int            i;
	ir_node       *proj;

	if (mode_is_signed(cmp_mode)) {
		cmp_mode = find_unsigned_mode(cmp_mode);
		sel      = new_r_Conv(block, sel, cmp_mode);
	}

	/* normalize so switch_min is at 0 */
	if (env->switch_min != 0) {
		ir_node *min_const  = new_r_Const_long(irg, cmp_mode, env->switch_min);
		sel = new_rd_Sub(dbgi, block, sel, min_const, cmp_mode);

		delta            = env->switch_min;
		env->switch_min  = 0;
		env->switch_max -= delta;
		set_Cond_selector(cond, sel);
	}

	/* check for out-of-bounds */
	max_const  = new_r_Const_long(irg, cmp_mode, env->switch_max);
	cmp        = new_rd_Cmp(dbgi, block, sel, max_const);
	proj_cmp   = new_r_Proj(cmp, mode_b, pn_Cmp_Le);
	oob_cond   = new_rd_Cond(dbgi, block, proj_cmp);
	proj_true  = new_r_Proj(oob_cond, mode_X, pn_Cond_true);
	proj_false = new_r_Proj(oob_cond, mode_X, pn_Cond_false);

	ARR_APP1(ir_node*, default_preds, proj_false);

	/* create new block containing the cond */
	in[0] = proj_true;
	new_block = new_r_Block(irg, 1, in);
	set_nodes_block(cond, new_block);

	/* adapt projs */
	foreach_out_irn(cond, i, proj) {
		unsigned long pn     = get_Proj_proj(proj);
		unsigned long new_pn = pn - delta;
		if (pn == default_pn) {
			/* we might have to choose a new default_pn */
			if (pn < (unsigned long) env->switch_max) {
				new_pn = env->switch_max + 1;
				set_Cond_default_proj(cond, new_pn);
			} else {
				new_pn = default_pn;
			}
			ARR_APP1(ir_node*, default_preds, proj);
		}

		set_Proj_proj(proj, new_pn);
		set_nodes_block(proj, new_block);
	}

	/* adapt default block */
	n_default_preds = ARR_LEN(default_preds);
	if (n_default_preds > 1) {
		size_t i;

		/* create new intermediate blocks so we don't have critical edges */
		for (i = 0; i < n_default_preds; ++i) {
			ir_node *proj = default_preds[i];
			ir_node *block;
			ir_node *in[1];

			in[0] = proj;
			block = new_r_Block(irg, 1, in);

			default_preds[i] = new_r_Jmp(block);
		}
	}
	set_irn_in(env->default_block, n_default_preds, default_preds);

	DEL_ARR_F(default_preds);
}

/**
 * Block-Walker: searches for Cond nodes with a non-boolean mode
 */
static void find_cond_nodes(ir_node *block, void *ctx)
{
	walk_env_t  *env = (walk_env_t *)ctx;
	ir_node     *projx;
	ir_node     *cond;
	ir_node     *sel;
	ir_mode     *sel_mode;
	long         default_pn;
	int          i;
	unsigned     j = 0;
	unsigned     numcases;
	ir_node     *proj;
	case_data_t *cases;
	ir_node     *condblock;
	ir_node     *defblock = NULL;
	dbg_info    *dbgi;
	cond_env_t   cond_env;
	unsigned long spare;

	/* because we split critical blocks only blocks with 1 predecessors may
	 * contain Proj->Cond nodes */
	if (get_Block_n_cfgpreds(block) != 1)
		return;

	projx = get_Block_cfgpred(block, 0);
	if (!is_Proj(projx))
		return;
	assert(get_irn_mode(projx) == mode_X);

	cond = get_Proj_pred(projx);
	if (!is_Cond(cond))
		return;

	sel      = get_Cond_selector(cond);
	sel_mode = get_irn_mode(sel);

	if (sel_mode == mode_b)
		return;

	if (ir_nodeset_contains(&env->processed, cond))
		return;
	ir_nodeset_insert(&env->processed, cond);

	/* the algorithms here don't work reliable for modes bigger than 32
	 * since we operate with long numbers */
	assert(get_mode_size_bits(sel_mode) <= 32);

	analyse_switch(&cond_env, cond);

	/*
	 * Here we have: num_cases and [switch_min, switch_max] interval.
	 * We do an if-cascade if there are too many spare numbers.
	 */
	spare = (unsigned long) cond_env.switch_max
		- (unsigned long) cond_env.switch_min
		- (unsigned long) cond_env.num_cases + 1;
	if (spare < env->spare_size) {
		/* we won't decompose the switch. But we might have to add
		 * out-of-bounds checking */
		if (!env->allow_out_of_bounds) {
			create_out_of_bounds_check(&cond_env, cond);
			env->changed = true;
		}
		return;
	}

	/*
	 * Switch should be transformed into an if cascade.
	 * So first order the cases, so we can do a binary search on them.
	 */
	env->changed = true;

	numcases = get_irn_n_outs(cond) - 1; /* does not contain default case */
	cases    = XMALLOCN(case_data_t, numcases);

	default_pn        = get_Cond_default_proj(cond);
	cond_env.sel      = sel;
	cond_env.defusers = NEW_ARR_F(ir_node*, 0);

	foreach_out_irn(cond, i, proj) {
		long pn = get_Proj_proj(proj);
		ir_node *target = get_irn_out(proj, 0);
		assert(get_Block_n_cfgpreds(target) == 1);

		if (pn == default_pn) {
			defblock = target;
			continue;
		}

		cases[j].value  = pn;
		cases[j].target = target;
		j++;
	}
	assert(j == numcases);

	if (defblock == NULL)
		panic("Switch %+F has no default proj", cond);

	qsort(cases, numcases, sizeof(cases[0]), casecmp);

	/* Now create the if cascade */
	condblock = get_nodes_block(cond);
	dbgi      = get_irn_dbg_info(cond);
	create_if_cascade(&cond_env, dbgi, condblock, cases, numcases);

	/* Connect new default case users */
	set_irn_in(defblock, ARR_LEN(cond_env.defusers), cond_env.defusers);

	xfree(cases);
	DEL_ARR_F(cond_env.defusers);
}

void lower_switch(ir_graph *irg, unsigned spare_size, int allow_out_of_bounds)
{
	walk_env_t env;
	env.changed             = false;
	env.spare_size          = spare_size;
	env.allow_out_of_bounds = allow_out_of_bounds;
	ir_nodeset_init(&env.processed);

	remove_critical_cf_edges(irg);
	assure_irg_outs(irg);

	irg_block_walk_graph(irg, find_cond_nodes, NULL, &env);
	ir_nodeset_destroy(&env.processed);

	if (env.changed) {
		/* control flow changed */
		set_irg_outs_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}
}
