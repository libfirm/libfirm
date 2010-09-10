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

#define foreach_out_irn(irn, i, outirn) for (i = get_irn_n_outs(irn) - 1;\
	i >= 0 && (outirn = get_irn_out(irn, i)); --i)

typedef struct walk_env {
	unsigned         spare_size;        /**< the allowed spare size for table switches */
	bool             changed;           /**< indicates whether a change was performed */
} walk_env_t;

typedef struct case_data {
	long     value;
	ir_node *target;
} case_data_t;

typedef struct ifcas_env {
	ir_node  *sel;
	int       defindex;
	ir_node **defusers;           /**< the Projs pointing to the default case */
} ifcas_env_t;

/**
 * Evaluate a switch and decide whether we should build a table switch.
 *
 * @param cond       The Cond node representing the switch.
 * @param spare_size Allowed spare size for table switches in machine words.
 *                   (Default in edgfe: 128)
 */
static bool should_do_table_switch(ir_node *cond, unsigned spare_size)
{
	long     default_pn;
	int      i;
	ir_node *proj;
	long switch_min = LONG_MAX;
	long switch_max = LONG_MIN;
	unsigned long spare;
	unsigned long num_cases = 0;

	/* TODO: Minimum size for jump table? */
	if (get_irn_n_outs(cond) <= 4)
		return false;

	default_pn = get_Cond_default_proj(cond);

	foreach_out_irn(cond, i, proj) {
		long pn = get_Proj_proj(proj);
		if (pn == default_pn)
			continue;

		if (pn < switch_min)
			switch_min = pn;
		if (pn > switch_max)
			switch_max = pn;
		++num_cases;
	}

	/*
	 * Here we have: num_cases and [switch_min, switch_max] interval.
	 * We do an if-cascade if there are too many spare numbers.
	 */
	spare = (unsigned long) switch_max - (unsigned long) switch_min - num_cases + 1;
	return spare < spare_size;
}

static int casecmp(const void *a, const void *b)
{
	const case_data_t *cda = a;
	const case_data_t *cdb = b;

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
static void create_if_cascade(ifcas_env_t *env, dbg_info *dbgi, ir_node *block,
                              case_data_t *curcases, unsigned numcases)
{
	ir_graph *irg = get_irn_irg(block);
    ir_mode *cmp_mode;
    ir_node *cmp_sel;
    ir_node *sel_block;

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
		env->defusers[env->defindex++] = new_Jmp();
	} else if (numcases == 1) {
		/* only one case: "if (sel == val) goto target else goto default;" */
		ir_node *val       = new_r_Const_long(irg, cmp_mode, curcases[0].value);
		ir_node *cmp       = new_rd_Cmp(dbgi, block, cmp_sel, val);
		ir_node *proj      = new_r_Proj(cmp, mode_b, pn_Cmp_Eq);
		ir_node *cond      = new_rd_Cond(dbgi, block, proj);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);

		set_Block_cfgpred(curcases[0].target, 0, trueproj);
		env->defusers[env->defindex++] = falseproj;
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
		env->defusers[env->defindex++] = falseproj;
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

/**
 * Block-Walker: searches for Cond nodes with a non-boolean mode
 */
static void find_cond_nodes(ir_node *block, void *ctx)
{
	walk_env_t  *env = ctx;
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
	ifcas_env_t  ifcas_env;

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

	/* the algorithms here don't work reliable for modes bigger than 32
	 * since we operate with long numbers */
	assert(get_mode_size_bits(sel_mode) <= 32);

	/* ok, we have found a switch cond */

	/* no need to do anything if backend handles out-of-bounds and the switch
	 * is small enough */
	if (should_do_table_switch(cond, env->spare_size))
		return;

	/*
	 * Switch should be transformed into an if cascade.
	 * So first order the cases, so we can do a binary search on them.
	 */
	env->changed = true;

	numcases = get_irn_n_outs(cond) - 1; /* does not contain default case */
	cases    = XMALLOCN(case_data_t, numcases);

	default_pn         = get_Cond_default_proj(cond);
	ifcas_env.sel      = sel;
	ifcas_env.defindex = 0;
	ifcas_env.defusers = XMALLOCN(ir_node*, numcases);

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
	create_if_cascade(&ifcas_env, dbgi, condblock, cases, numcases);

	/* Connect new default case users */
	set_irn_in(defblock, ifcas_env.defindex, ifcas_env.defusers);

	xfree(cases);
	xfree(ifcas_env.defusers);
}

void lower_switch(ir_graph *irg, unsigned spare_size)
{
	walk_env_t env;
	env.changed             = false;
	env.spare_size          = spare_size;

	remove_critical_cf_edges(irg);
	assure_irg_outs(irg);

	irg_block_walk_graph(irg, find_cond_nodes, NULL, &env);

	if (env.changed) {
		/* control flow changed */
		set_irg_outs_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}
}
