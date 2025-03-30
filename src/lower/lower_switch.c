/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lowering of Switches if necessary or advantageous.
 * @author  Moritz Kroll
 */
#include "array.h"
#include "ircons.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodeset.h"
#include "irouts_t.h"
#include "lowering.h"
#include "panic.h"
#include "util.h"
#include <stdbool.h>

typedef struct walk_env_t {
	ir_nodeset_t  processed;
	ir_mode      *selector_mode;
	unsigned      spare_size; /**< the allowed spare size for table switches */
	unsigned      small_switch;
	bool          changed;    /**< indicates whether a change was performed */
} walk_env_t;

typedef struct target_t {
	ir_node *block;     /**< block that is targetted */
	uint16_t n_entries; /**< number of table entries targetting this block */
	uint16_t i;
} target_t;

typedef struct switch_info_t {
	ir_node     *switchn;
	ir_tarval   *switch_min;
	ir_tarval   *switch_max;
	ir_node     *default_block;
	unsigned     num_cases;
	target_t    *targets;
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
		if (entry->pn == pn_Switch_default)
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

/**
 * Analyse the stuff that anayse_switch0() left out
 */
static void analyse_switch1(switch_info_t *info)
{
	const ir_node  *switchn   = info->switchn;
	unsigned        n_outs    = get_Switch_n_outs(switchn);
	target_t       *targets   = XMALLOCNZ(target_t, n_outs);
	foreach_irn_out_r(switchn, i, proj) {
		unsigned pn     = get_Proj_num(proj);
		ir_node *target = get_irn_out(proj, 0);

		assert((unsigned)pn < n_outs);
		assert(targets[(unsigned)pn].block == NULL);
		targets[(unsigned)pn].block = target;
	}

	const ir_switch_table *table = get_Switch_table(switchn);
	for (size_t e = 0, n_entries = ir_switch_table_get_n_entries(table);
	     e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		if (entry->pn == pn_Switch_default)
			continue;

		target_t *target = &targets[entry->pn];
		++target->n_entries;
	}

	info->default_block = targets[pn_Switch_default].block;
	info->targets       = targets;
}

static int compare_entries(const void *a, const void *b)
{
	const ir_switch_table_entry *entry0 = (const ir_switch_table_entry*)a;
	const ir_switch_table_entry *entry1 = (const ir_switch_table_entry*)b;

	/* sort default targets to the end */
	if (entry0->pn == pn_Switch_default)
		return 1;
	if (entry1->pn == pn_Switch_default)
		return -1;

	if (tarval_cmp(entry0->max, entry1->min) == ir_relation_less)
		return -1;
	/* cases must be non overlapping, so the only remaining case is greater */
	assert(tarval_cmp(entry0->min, entry1->max) == ir_relation_greater);
	return 1;
}

static void normalize_table(ir_node *switchn, ir_mode *new_mode,
                            ir_tarval *delta)
{
	ir_switch_table *table = get_Switch_table(switchn);
	QSORT(table->entries, table->n_entries, compare_entries);

	/* subtract delta from min/max values and remove default pn entries */
	for (size_t e = 0, n_entries = ir_switch_table_get_n_entries(table);
	     e < n_entries; ++e) {
		ir_switch_table_entry *entry = ir_switch_table_get_entry(table, e);

		/* default entries have been sorted to the end, cut off list there */
		if (entry->pn == pn_Switch_default) {
			table->n_entries = e;
			break;
		}

		ir_tarval *min = entry->min;
		min = tarval_convert_to(min, new_mode);
		if (delta != NULL)
			min = tarval_sub(min, delta);

		if (entry->min == entry->max) {
			entry->min = min;
			entry->max = min;
		} else {
			ir_tarval *max = entry->max;
			max = tarval_convert_to(max, new_mode);
			if (delta != NULL)
				max = tarval_sub(max, delta);
			entry->min = min;
			entry->max = max;
		}
	}
}

static void create_out_of_bounds_check(switch_info_t *info)
{
	/* should already be normalized to zero */
	assert(tarval_is_null(info->switch_min));

	/* create out-of-bounds check */
	ir_node  *switchn    = info->switchn;
	ir_graph *irg        = get_irn_irg(switchn);
	dbg_info *dbgi       = get_irn_dbg_info(switchn);
	ir_node  *selector   = get_Switch_selector(switchn);
	ir_node  *block      = get_nodes_block(switchn);
	ir_node  *max_const  = new_r_Const(irg, info->switch_max);
	ir_node  *cmp        = new_rd_Cmp(dbgi, block, selector, max_const,
	                                  ir_relation_less_equal);
	ir_node  *oob_cond   = new_rd_Cond(dbgi, block, cmp);
	ir_node  *proj_true  = new_r_Proj(oob_cond, mode_X, pn_Cond_true);
	ir_node  *proj_false = new_r_Proj(oob_cond, mode_X, pn_Cond_false);

	ir_node **default_preds = NEW_ARR_F(ir_node*, 0);
	ARR_APP1(ir_node*, default_preds, proj_false);

	/* create new block containing the switch */
	ir_node *in[]      = { proj_true };
	ir_node *new_block = new_r_Block(irg, ARRAY_SIZE(in), in);
	set_nodes_block(switchn, new_block);

	/* adjust projs */
	ir_node *default_block = NULL;
	foreach_irn_out_r(switchn, i, proj) {
		unsigned pn = get_Proj_num(proj);
		if (pn == pn_Switch_default) {
			assert(default_block == NULL);
			default_block = get_irn_out(proj, 0);
			ARR_APP1(ir_node*, default_preds, proj);
		}
		set_nodes_block(proj, new_block);
	}

	/* adapt default block */
	size_t n_default_preds = ARR_LEN(default_preds);
	if (n_default_preds > 1) {
		/* create new intermediate blocks so we don't have critical edges */
		for (size_t p = 0; p < n_default_preds; ++p) {
			ir_node *pred        = default_preds[p];
			ir_node *bin[]       = { pred };
			ir_node *split_block = new_r_Block(irg, ARRAY_SIZE(bin), bin);

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
	ir_node   *switchn  = info->switchn;
	ir_node   *block    = get_nodes_block(switchn);
	ir_node   *selector = get_Switch_selector(switchn);
	ir_mode   *mode     = get_irn_mode(selector);
	ir_tarval *min      = info->switch_min;
	if (mode_is_signed(mode)) {
		mode             = find_unsigned_mode(mode);
		selector         = new_r_Conv(block, selector, mode);
		min              = tarval_convert_to(min, mode);
		info->switch_min = min;
		info->switch_max = tarval_convert_to(info->switch_max, mode);
	}

	/* normalize so switch_min is at 0 */
	ir_tarval *delta = NULL;
	if (min != get_mode_null(mode)) {
		ir_graph *irg       = get_irn_irg(switchn);
		ir_node  *min_const = new_r_Const(irg, min);
		dbg_info *dbgi      = get_irn_dbg_info(switchn);
		selector = new_rd_Sub(dbgi, block, selector, min_const);

		info->switch_max = tarval_sub(info->switch_max, min);
		info->switch_min = get_mode_null(mode);
		delta            = min;
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
		set_Switch_selector(switchn, selector);
	}

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
		ir_tarval *adjusted_max = tarval_sub(entry->max, entry->min);
		ir_node   *sub          = new_rd_Sub(dbgi, block, selector, minconst);
		ir_node   *maxconst     = new_r_Const(irg, adjusted_max);
		cmp = new_rd_Cmp(dbgi, block, sub, maxconst, ir_relation_less_equal);
	}
	return new_rd_Cond(dbgi, block, cmp);
}

static void connect_to_target(target_t *target, ir_node *cf)
{
	ir_node *block     = target->block;
	unsigned n_entries = target->n_entries;
	if (get_Block_n_cfgpreds(block) != (int)n_entries) {
		ir_node **new_in = ALLOCAN(ir_node*, n_entries);
		ir_graph *irg    = get_irn_irg(block);
		ir_node  *dummy  = new_r_Dummy(irg, mode_X);
		for (unsigned i = 0; i < n_entries; ++i) {
			new_in[i] = dummy;
		}
		set_irn_in(block, n_entries, new_in);
		assert(target->i == 0);
	}
	assert(target->i < n_entries);
	set_Block_cfgpred(target->block, target->i++, cf);
}

/**
 * Creates an if cascade realizing binary search.
 */
static void create_if_cascade(switch_info_t *info, ir_node *block,
                              ir_switch_table_entry *curcases,
                              unsigned numcases)
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
		const ir_switch_table_entry *entry = &curcases[0];
		ir_node *cond      = create_case_cond(entry, dbgi, block, selector);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);

		connect_to_target(&info->targets[entry->pn], trueproj);
		ARR_APP1(ir_node*, info->defusers, falseproj);
	} else if (numcases == 2) {
		/* only two cases: "if (sel == val[0]) goto target[0];" */
		const ir_switch_table_entry *entry0 = &curcases[0];
		const ir_switch_table_entry *entry1 = &curcases[1];
		ir_node *cond      = create_case_cond(entry0, dbgi, block, selector);
		ir_node *trueproj  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *falseproj = new_r_Proj(cond, mode_X, pn_Cond_false);
		connect_to_target(&info->targets[entry0->pn], trueproj);

		ir_node *in[]    = { falseproj };
		ir_node *neblock = new_r_Block(irg, ARRAY_SIZE(in), in);

		/* second part: "else if (sel == val[1]) goto target[1] else goto default;" */
		ir_node *cond1      = create_case_cond(entry1, dbgi, neblock, selector);
		ir_node *trueproj1  = new_r_Proj(cond1, mode_X, pn_Cond_true);
		ir_node *falseproj1 = new_r_Proj(cond1, mode_X, pn_Cond_false);
		connect_to_target(&info->targets[entry1->pn], trueproj1);
		ARR_APP1(ir_node*, info->defusers, falseproj1);
	} else {
		/* recursive case: split cases in the middle */
		unsigned midcase = numcases / 2;
		const ir_switch_table_entry *entry = &curcases[midcase];
		ir_node *val = new_r_Const(irg, entry->min);
		ir_node *cmp = new_rd_Cmp(dbgi, block, selector, val, ir_relation_less);
		ir_node *cond = new_rd_Cond(dbgi, block, cmp);

		ir_node *ltin[]  = { new_r_Proj(cond, mode_X, pn_Cond_true) };
		ir_node *ltblock = new_r_Block(irg, ARRAY_SIZE(ltin), ltin);

		ir_node *gein[]  = { new_r_Proj(cond, mode_X, pn_Cond_false) };
		ir_node *geblock = new_r_Block(irg, ARRAY_SIZE(gein), gein);

		create_if_cascade(info, ltblock, curcases, midcase);
		create_if_cascade(info, geblock, curcases + midcase, numcases - midcase);
	}
}

/**
 * Block-Walker: searches for Switch nodes
 */
static void find_switch_nodes(ir_node *block, void *ctx)
{
	walk_env_t *env = (walk_env_t*)ctx;

	/* because we split critical blocks only blocks with 1 predecessors may
	 * contain Proj->Cond nodes */
	if (get_Block_n_cfgpreds(block) != 1)
		return;

	ir_node *projx = get_Block_cfgpred(block, 0);
	if (!is_Proj(projx))
		return;
	assert(get_irn_mode(projx) == mode_X);

	ir_node *switchn = get_Proj_pred(projx);
	if (!is_Switch(switchn))
		return;
	if (!ir_nodeset_insert(&env->processed, switchn))
		return;

	switch_info_t info;
	analyse_switch0(&info, switchn);

	/*
	 * Here we have: num_cases and [switch_min, switch_max] interval.
	 * We do an if-cascade if there are too many spare numbers.
	 */
	ir_mode   *selector_mode = get_irn_mode(get_Switch_selector(switchn));
	ir_tarval *spare = tarval_sub(info.switch_max, info.switch_min);
	ir_mode   *mode  = find_unsigned_mode(selector_mode);
	spare = tarval_convert_to(spare, mode);
	ir_tarval *num_cases_minus_one
		= new_tarval_from_long(info.num_cases-1, mode);
	spare = tarval_sub(spare, num_cases_minus_one);
	ir_tarval *spare_size = new_tarval_from_long(env->spare_size, mode);
	bool lower_switch = info.num_cases <= env->small_switch
		|| (tarval_cmp(spare, spare_size) & ir_relation_greater_equal);

	if (!lower_switch) {
		/* we won't decompose the switch. But we must add an out-of-bounds
		 * check */
		env->changed |= normalize_switch(&info, env->selector_mode);
		return;
	}

	normalize_table(switchn, selector_mode, NULL);
	analyse_switch1(&info);

	/* Now create the if cascade */
	env->changed  = true;
	info.defusers = NEW_ARR_F(ir_node*, 0);
	block         = get_nodes_block(switchn);
	ir_switch_table *table = get_Switch_table(switchn);
	create_if_cascade(&info, block, table->entries, table->n_entries);

	/* Connect new default case users */
	set_irn_in(info.default_block, ARR_LEN(info.defusers), info.defusers);

	DEL_ARR_F(info.defusers);
	free(info.targets);
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

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	irg_block_walk_graph(irg, find_switch_nodes, NULL, &env);
	ir_nodeset_destroy(&env.processed);

	confirm_irg_properties(irg, env.changed ? IR_GRAPH_PROPERTIES_NONE
	                                        : IR_GRAPH_PROPERTIES_ALL);
}
