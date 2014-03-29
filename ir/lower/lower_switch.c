/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lowering of Switches if necessary or advantageous.
 * @author  Moritz Kroll
 */
#include <limits.h>
#include <stdbool.h>

#include "../../adt/util.h"
#include "array.h"
#include "ircons.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irouts_t.h"
#include "lowering.h"
#include "error.h"
#include "irnodeset.h"

typedef struct walk_env_t {
	ir_nodeset_t  processed;
	ir_mode      *selector_mode;
	unsigned      spare_size; /**< the allowed spare size for table switches */
	unsigned      small_switch;
	bool          changed;    /**< indicates whether a change was performed */
} walk_env_t;

typedef struct case_data_t {
	const ir_switch_table_entry *entry;
	ir_node                     *target;
} case_data_t;

typedef struct switch_info_t {
	ir_node     *switchn;
	ir_tarval   *switch_min;
	ir_tarval   *switch_max;
	ir_node     *default_block;
	unsigned     num_cases;
	case_data_t *cases;
	ir_node    **defusers;    /**< the Projs pointing to the default case */
} switch_info_t;

/**
 * analyze enough to decide if we should lower the switch
 */
static void analyse_switch0(switch_info_t *info, ir_node *switchn)
{
	const ir_switch_table *table      = get_Switch_table(switchn);
	size_t                 n_entries  = ir_switch_table_get_n_entries(table);
	ir_mode               *mode       = get_irn_mode(get_Switch_selector(switchn));
	ir_tarval             *switch_min = get_mode_max(mode);
	ir_tarval             *switch_max = get_mode_min(mode);
	unsigned               num_cases  = 0;

	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		if (entry->pn == 0)
			continue;

		if (tarval_cmp(entry->min, switch_min) == ir_relation_less)
			switch_min = entry->min;
		if (tarval_cmp(entry->max, switch_max) == ir_relation_greater)
			switch_max = entry->max;

		++num_cases;
	}

	info->switchn    = switchn;
	info->switch_min = switch_min;
	info->switch_max = switch_max;
	info->num_cases  = num_cases;
}

static int casecmp(const void *a, const void *b)
{
	const case_data_t           *cda = (const case_data_t*)a;
	const case_data_t           *cdb = (const case_data_t*)b;
	const ir_switch_table_entry *ea  = cda->entry;
	const ir_switch_table_entry *eb  = cdb->entry;

	if (ea == eb)
		return 0;

	if (tarval_cmp(ea->max, eb->min) == ir_relation_less)
		return -1;
	/* cases must be non overlapping, so the only remaining case is greater */
	assert(tarval_cmp(ea->min, eb->max) == ir_relation_greater);
	return 1;
}

/**
 * Analyse the stuff that anayse_switch0() left out
 */
static void analyse_switch1(switch_info_t *info)
{
	const ir_node         *switchn   = info->switchn;
	const ir_switch_table *table     = get_Switch_table(switchn);
	size_t                 n_entries = ir_switch_table_get_n_entries(table);
	unsigned               n_outs    = get_Switch_n_outs(switchn);
	ir_node              **targets   = XMALLOCNZ(ir_node*, n_outs);
	unsigned               num_cases = info->num_cases;
	case_data_t           *cases     = XMALLOCN(case_data_t, num_cases);
	unsigned               c         = 0;
	size_t                 e;

	foreach_irn_out_r(switchn, i, proj) {
		long     pn     = get_Proj_proj(proj);
		ir_node *target = get_irn_out(proj, 0);

		assert((unsigned)pn < n_outs);
		assert(targets[(unsigned)pn] == NULL);
		targets[(unsigned)pn] = target;
	}

	for (e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		if (entry->pn == 0)
			continue;

		cases[c].entry  = entry;
		cases[c].target = targets[entry->pn];
		++c;
	}
	assert(c == num_cases);

	/*
	 * Switch should be transformed into an if cascade.
	 * So first order the cases, so we can do a binary search on them.
	 */
	QSORT(cases, num_cases, casecmp);

	info->default_block = targets[pn_Switch_default];
	info->cases         = cases;
	free(targets);
}

static void normalize_table(ir_node *switchn, ir_mode *new_mode,
                            ir_tarval *delta)
{
	ir_switch_table *table     = get_Switch_table(switchn);
	size_t           n_entries = ir_switch_table_get_n_entries(table);
	size_t           e;
	/* adapt switch_table */
	for (e = 0; e < n_entries; ++e) {
		ir_switch_table_entry *entry = ir_switch_table_get_entry(table, e);
		ir_tarval *min = entry->min;

		if (entry->pn == 0)
			continue;

		min = tarval_convert_to(min, new_mode);
		if (delta != NULL)
			min = tarval_sub(min, delta, NULL);

		if (entry->min == entry->max) {
			entry->min = min;
			entry->max = min;
		} else {
			ir_tarval *max = entry->max;
			max = tarval_convert_to(max, new_mode);
			if (delta != NULL)
				max = tarval_sub(max, delta, NULL);
			entry->min = min;
			entry->max = max;
		}
	}
}

static void create_out_of_bounds_check(switch_info_t *info)
{
	ir_node    *switchn       = info->switchn;
	ir_graph   *irg           = get_irn_irg(switchn);
	dbg_info   *dbgi          = get_irn_dbg_info(switchn);
	ir_node    *selector      = get_Switch_selector(switchn);
	ir_node    *block         = get_nodes_block(switchn);
	ir_node   **default_preds = NEW_ARR_F(ir_node*, 0);
	ir_node    *default_block = NULL;
	ir_node    *max_const;
	ir_node    *proj_true;
	ir_node    *proj_false;
	ir_node    *cmp;
	ir_node    *oob_cond;
	ir_node    *in[1];
	ir_node    *new_block;
	size_t      n_default_preds;

	assert(tarval_is_null(info->switch_min));

	/* check for out-of-bounds */
	max_const  = new_r_Const(irg, info->switch_max);
	cmp        = new_rd_Cmp(dbgi, block, selector, max_const, ir_relation_less_equal);
	oob_cond   = new_rd_Cond(dbgi, block, cmp);
	proj_true  = new_r_Proj(oob_cond, mode_X, pn_Cond_true);
	proj_false = new_r_Proj(oob_cond, mode_X, pn_Cond_false);

	ARR_APP1(ir_node*, default_preds, proj_false);

	/* create new block containing the switch */
	in[0] = proj_true;
	new_block = new_r_Block(irg, 1, in);
	set_nodes_block(switchn, new_block);

	/* adjust projs */
	foreach_irn_out_r(switchn, i, proj) {
		long pn = get_Proj_proj(proj);
		if (pn == pn_Switch_default) {
			assert(default_block == NULL);
			default_block = get_irn_out(proj, 0);
			ARR_APP1(ir_node*, default_preds, proj);
		}
		set_nodes_block(proj, new_block);
	}

	/* adapt default block */
	n_default_preds = ARR_LEN(default_preds);
	if (n_default_preds > 1) {
		/* create new intermediate blocks so we don't have critical edges */
		size_t p;
		for (p = 0; p < n_default_preds; ++p) {
			ir_node *pred = default_preds[p];
			ir_node *split_block;
			ir_node *block_in[1];

			block_in[0] = pred;
			split_block = new_r_Block(irg, 1, block_in);

			default_preds[p] = new_r_Jmp(split_block);
		}
	}
	set_irn_in(default_block, n_default_preds, default_preds);

	DEL_ARR_F(default_preds);
}

/**
 * normalize switch to work on an unsigned input with the first case at 0
 */
static bool normalize_switch(switch_info_t *info, ir_mode *selector_mode)
{
	ir_node   *switchn         = info->switchn;
	ir_graph  *irg             = get_irn_irg(switchn);
	ir_node   *block           = get_nodes_block(switchn);
	ir_node   *selector        = get_Switch_selector(switchn);
	ir_mode   *mode            = get_irn_mode(selector);
	ir_tarval *delta           = NULL;
	bool       needs_normalize = false;

	ir_tarval *min = info->switch_min;
	if (mode_is_signed(mode)) {
		mode             = find_unsigned_mode(mode);
		selector         = new_r_Conv(block, selector, mode);
		min              = tarval_convert_to(min, mode);
		info->switch_min = min;
		info->switch_max = tarval_convert_to(info->switch_max, mode);
		needs_normalize  = true;
	}

	/* normalize so switch_min is at 0 */
	if (min != get_mode_null(mode)) {
		ir_node  *min_const = new_r_Const(irg, min);
		dbg_info *dbgi      = get_irn_dbg_info(switchn);
		selector = new_rd_Sub(dbgi, block, selector, min_const, mode);

		info->switch_max = tarval_sub(info->switch_max, min, mode);
		info->switch_min = get_mode_null(mode);
		delta            = min;

		needs_normalize = true;
	}

	/* if we have a selector_mode set, then the we will have a switch node,
	 * we have to construct an out-of-bounds check then and after that convert
	 * the switch/selector to the backends desired switch mode */
	if (selector_mode != NULL) {
		set_Switch_selector(switchn, selector);
		create_out_of_bounds_check(info);

		selector = new_r_Conv(block, selector, selector_mode);
		mode     = selector_mode;
		info->switch_min = tarval_convert_to(info->switch_min, mode);
		info->switch_max = tarval_convert_to(info->switch_max, mode);
		if (delta != NULL)
			delta = tarval_convert_to(delta, mode);
		needs_normalize = true;
	}

	if (!needs_normalize)
		return false;

	set_Switch_selector(switchn, selector);
	normalize_table(switchn, mode, delta);
	return true;
}

/**
 * Create an if (selector == caseval) Cond node (and handle the special case
 * of ranged cases)
 */
static ir_node *create_case_cond(const ir_switch_table_entry *entry,
                                 dbg_info *dbgi, ir_node *block,
                                 ir_node *selector)
{
	ir_graph *irg      = get_irn_irg(block);
	ir_node  *minconst = new_r_Const(irg, entry->min);
	ir_node  *cmp;

	if (entry->min == entry->max) {
		cmp = new_rd_Cmp(dbgi, block, selector, minconst, ir_relation_equal);
	} else {
		ir_tarval *adjusted_max = tarval_sub(entry->max, entry->min, NULL);
		ir_node   *sub          = new_rd_Sub(dbgi, block, selector, minconst,
		                                     get_tarval_mode(adjusted_max));
		ir_node   *maxconst     = new_r_Const(irg, adjusted_max);
		cmp = new_rd_Cmp(dbgi, block, sub, maxconst, ir_relation_less_equal);
	}

	return new_rd_Cond(dbgi, block, cmp);
}

/**
 * Creates an if cascade realizing binary search.
 */
static void create_if_cascade(switch_info_t *info, ir_node *block,
                              case_data_t *curcases, unsigned numcases)
{
	ir_graph      *irg      = get_irn_irg(block);
	const ir_node *switchn  = info->switchn;
	dbg_info      *dbgi     = get_irn_dbg_info(switchn);
	ir_node       *selector = get_Switch_selector(switchn);

	if (numcases == 0) {
		/* zero cases: "goto default;" */
		ARR_APP1(ir_node*, info->defusers, new_r_Jmp(block));
	} else if (numcases == 1) {
		/*only one case: "if (sel == val) goto target else goto default;"*/
		const ir_switch_table_entry *entry = curcases[0].entry;
		ir_node *cond      = create_case_cond(entry, dbgi, block, selector);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);

		set_Block_cfgpred(curcases[0].target, 0, trueproj);
		ARR_APP1(ir_node*, info->defusers, falseproj);
	} else if (numcases == 2) {
		/* only two cases: "if (sel == val[0]) goto target[0];" */
		const ir_switch_table_entry *entry0 = curcases[0].entry;
		const ir_switch_table_entry *entry1 = curcases[1].entry;
		ir_node *cond      = create_case_cond(entry0, dbgi, block, selector);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);
		ir_node *in[1];
		ir_node *neblock;

		set_Block_cfgpred(curcases[0].target, 0, trueproj);

		in[0] = falseproj;
		neblock = new_r_Block(irg, 1, in);

		/* second part: "else if (sel == val[1]) goto target[1] else goto default;" */
		cond      = create_case_cond(entry1, dbgi, neblock, selector);
		trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);
		set_Block_cfgpred(curcases[1].target, 0, trueproj);
		ARR_APP1(ir_node*, info->defusers, falseproj);
	} else {
		/* recursive case: split cases in the middle */
		unsigned midcase = numcases / 2;
		const ir_switch_table_entry *entry = curcases[midcase].entry;
		ir_node *val = new_r_Const(irg, entry->min);
		ir_node *cmp = new_rd_Cmp(dbgi, block, selector, val, ir_relation_less);
		ir_node *cond = new_rd_Cond(dbgi, block, cmp);
		ir_node *in[1];
		ir_node *ltblock;
		ir_node *geblock;

		in[0]   = new_r_Proj(cond, mode_X, pn_Cond_true);
		ltblock = new_r_Block(irg, 1, in);

		in[0]   = new_r_Proj(cond, mode_X, pn_Cond_false);
		geblock = new_r_Block(irg, 1, in);

		create_if_cascade(info, ltblock, curcases, midcase);
		create_if_cascade(info, geblock, curcases + midcase, numcases - midcase);
	}
}

/**
 * Block-Walker: searches for Switch nodes
 */
static void find_switch_nodes(ir_node *block, void *ctx)
{
	walk_env_t   *env = (walk_env_t *)ctx;
	ir_node      *projx;
	ir_node      *switchn;
	switch_info_t info;

	/* because we split critical blocks only blocks with 1 predecessors may
	 * contain Proj->Cond nodes */
	if (get_Block_n_cfgpreds(block) != 1)
		return;

	projx = get_Block_cfgpred(block, 0);
	if (!is_Proj(projx))
		return;
	assert(get_irn_mode(projx) == mode_X);

	switchn = get_Proj_pred(projx);
	if (!is_Switch(switchn))
		return;

	if (!ir_nodeset_insert(&env->processed, switchn))
		return;

	analyse_switch0(&info, switchn);

	/*
	 * Here we have: num_cases and [switch_min, switch_max] interval.
	 * We do an if-cascade if there are too many spare numbers.
	 */
	ir_mode   *mode  = get_irn_mode(get_Switch_selector(switchn));
	ir_tarval *spare = tarval_sub(info.switch_max, info.switch_min, mode);
	mode  = find_unsigned_mode(mode);
	spare = tarval_convert_to(spare, mode);
	ir_tarval *num_cases_minus_one
		= new_tarval_from_long(info.num_cases-1, mode);
	spare = tarval_sub(spare, num_cases_minus_one, mode);
	ir_tarval *spare_size = new_tarval_from_long(env->spare_size, mode);
	bool lower_switch = (info.num_cases <= env->small_switch
	 || (tarval_cmp(spare, spare_size) & ir_relation_greater_equal));

	if (!lower_switch) {
		/* we won't decompose the switch. But we must add an out-of-bounds
		 * check */
		env->changed |= normalize_switch(&info, env->selector_mode);
		return;
	}

	normalize_switch(&info, NULL);
	analyse_switch1(&info);

	/* Now create the if cascade */
	env->changed  = true;
	info.defusers = NEW_ARR_F(ir_node*, 0);
	block         = get_nodes_block(switchn);
	create_if_cascade(&info, block, info.cases, info.num_cases);

	/* Connect new default case users */
	set_irn_in(info.default_block, ARR_LEN(info.defusers), info.defusers);

	DEL_ARR_F(info.defusers);
	free(info.cases);
}

void lower_switch(ir_graph *irg, unsigned small_switch, unsigned spare_size,
                  ir_mode *selector_mode)
{
	if (mode_is_signed(selector_mode))
		panic("expected unsigned mode for switch selector");

	walk_env_t env;
	env.selector_mode       = selector_mode;
	env.spare_size          = spare_size;
	env.small_switch        = small_switch;
	env.changed             = false;
	ir_nodeset_init(&env.processed);

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	irg_block_walk_graph(irg, find_switch_nodes, NULL, &env);
	ir_nodeset_destroy(&env.processed);

	confirm_irg_properties(irg, env.changed ? IR_GRAPH_PROPERTIES_NONE
	                                        : IR_GRAPH_PROPERTIES_ALL);
}
