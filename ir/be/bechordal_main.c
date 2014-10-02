/*
#include "irdump_t.h"
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Driver for the chordal register allocator.
 * @author      Sebastian Hack
 * @date        29.11.2005
 */
#include <stdlib.h>
#include <time.h>

#include "obst.h"
#include "bitset.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

#include "ircons_t.h"
#include "irmode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irdump.h"
#include "irdom.h"
#include "ircons.h"
#include "irnode.h"
#include "ircons.h"
#include "irtools.h"
#include "debug.h"
#include "execfreq.h"
#include "iredges_t.h"
#include "panic.h"

#include "bechordal_t.h"
#include "beutil.h"
#include "besched.h"
#include "besched.h"
#include "belive.h"
#include "bearch.h"
#include "beifg.h"
#include "benode.h"
#include "statev_t.h"
#include "bestat.h"
#include "bemodule.h"
#include "be_t.h"
#include "bera.h"
#include "beirg.h"
#include "bestack.h"

#include "bespillslots.h"
#include "bespill.h"
#include "belower.h"

#include "becopystat.h"
#include "becopyopt.h"
#include "bessadestr.h"
#include "beverify.h"
#include "benode.h"

#include "bepbqpcoloring.h"

static be_ra_chordal_opts_t options = {
	BE_CH_DUMP_NONE,
	BE_CH_LOWER_PERM_SWAP,
};

static const lc_opt_enum_int_items_t lower_perm_items[] = {
	{ "copy", BE_CH_LOWER_PERM_COPY },
	{ "swap", BE_CH_LOWER_PERM_SWAP },
	{ NULL, 0 }
};

static const lc_opt_enum_mask_items_t dump_items[] = {
	{ "none",       BE_CH_DUMP_NONE       },
	{ "spill",      BE_CH_DUMP_SPILL      },
	{ "live",       BE_CH_DUMP_LIVE       },
	{ "color",      BE_CH_DUMP_COLOR      },
	{ "copymin",    BE_CH_DUMP_COPYMIN    },
	{ "ssadestr",   BE_CH_DUMP_SSADESTR   },
	{ "tree",       BE_CH_DUMP_TREE_INTV  },
	{ "split",      BE_CH_DUMP_SPLIT      },
	{ "constr",     BE_CH_DUMP_CONSTR     },
	{ "lower",      BE_CH_DUMP_LOWER      },
	{ "spillslots", BE_CH_DUMP_SPILLSLOTS },
	{ "appel",      BE_CH_DUMP_APPEL      },
	{ "all",        BE_CH_DUMP_ALL        },
	{ NULL, 0 }
};

static lc_opt_enum_int_var_t lower_perm_var = {
	&options.lower_perm_opt, lower_perm_items
};

static lc_opt_enum_mask_var_t dump_var = {
	&options.dump_flags, dump_items
};

static const lc_opt_table_entry_t be_chordal_options[] = {
	LC_OPT_ENT_ENUM_INT ("perm",          "perm lowering options", &lower_perm_var),
	LC_OPT_ENT_ENUM_MASK("dump",          "select dump phases", &dump_var),
	LC_OPT_LAST
};

static be_module_list_entry_t *colorings = NULL;
static const be_ra_chordal_coloring_t *selected_coloring = NULL;

void be_register_chordal_coloring(const char *name, be_ra_chordal_coloring_t *coloring)
{
	if (selected_coloring == NULL)
		selected_coloring = coloring;

	be_add_module_to_list(&colorings, name, coloring);
}

static void be_ra_chordal_coloring(be_chordal_env_t *env)
{
	selected_coloring->allocate(env);
}

static void dump(unsigned mask, ir_graph *irg,
				 const arch_register_class_t *cls,
				 const char *suffix)
{
	if ((options.dump_flags & mask) == mask) {
		if (cls != NULL) {
			char buf[256];
			snprintf(buf, sizeof(buf), "%s-%s", cls->name, suffix);
			dump_ir_graph(irg, buf);
		} else {
			dump_ir_graph(irg, suffix);
		}
	}
}

/**
 * Post-Walker: Checks for the given reload if has only one user that can
 * perform the reload as part of its address mode.
 * Fold the reload into the user it that is possible.
 */
static void memory_operand_walker(ir_node *irn, void *env)
{
	(void)env;
	for (int i = 0, arity = get_irn_arity(irn); i < arity; ++i) {
		ir_node *in = get_irn_n(irn, i);
		if (!arch_irn_is(skip_Proj(in), reload))
			continue;
		if (get_nodes_block(in) != get_nodes_block(irn))
			continue;
		/* only use memory operands, if the reload is only used by 1 node */
		if (get_irn_n_edges(in) > 1)
			continue;
		if (arch_possible_memory_operand(irn, i)) {
			arch_perform_memory_operand(irn, i);
		}
	}
}

/**
 * Starts a walk for memory operands if supported by the backend.
 */
void check_for_memory_operands(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, memory_operand_walker, NULL);
}

static be_node_stats_t last_node_stats;

/**
 * Perform things which need to be done per register class before spilling.
 */
static void pre_spill(be_chordal_env_t *const chordal_env,
                      arch_register_class_t const *const cls,
                      ir_graph *const irg)
{
	chordal_env->cls              = cls;
	chordal_env->border_heads     = pmap_create();
	chordal_env->allocatable_regs = bitset_malloc(cls->n_regs);
	/* put all ignore registers into the ignore register set. */
	be_get_allocatable_regs(irg, cls, chordal_env->allocatable_regs->data);
	be_assure_live_chk(irg);
}

/**
 * Perform things which need to be done per register class after spilling.
 */
static void post_spill(be_chordal_env_t *const chordal_env, ir_graph *const irg)
{
	/* If we have a backend provided spiller, post spill is
	 * called in a loop after spilling for each register class.
	 * But we only need to fix stack nodes once in this case. */
	be_timer_push(T_RA_SPILL_APPLY);
	check_for_memory_operands(irg);
	be_timer_pop(T_RA_SPILL_APPLY);

	/* verify schedule and register pressure */
	if (be_options.do_verify) {
		be_timer_push(T_VERIFY);
		be_verify_schedule(irg);
		be_verify_register_pressure(irg, chordal_env->cls);
		be_timer_pop(T_VERIFY);
	}

	/* Color the graph. */
	be_timer_push(T_RA_COLOR);
	be_ra_chordal_coloring(chordal_env);
	be_timer_pop(T_RA_COLOR);

	dump(BE_CH_DUMP_COLOR, irg, chordal_env->cls, "color");

	/* Create the ifg with the selected flavor */
	be_timer_push(T_RA_IFG);
	chordal_env->ifg = be_create_ifg(chordal_env);
	be_timer_pop(T_RA_IFG);

	if (stat_ev_enabled) {
		be_ifg_stat_t stat;
		be_ifg_stat(irg, chordal_env->ifg, &stat);
		stat_ev_dbl("bechordal_ifg_nodes", stat.n_nodes);
		stat_ev_dbl("bechordal_ifg_edges", stat.n_edges);
		stat_ev_dbl("bechordal_ifg_comps", stat.n_comps);

		be_node_stats_t node_stats;
		be_collect_node_stats(&node_stats, irg);
		be_subtract_node_stats(&node_stats, &last_node_stats);

		stat_ev_dbl("bechordal_perms_before_coal",  node_stats[BE_STAT_PERMS]);
		stat_ev_dbl("bechordal_copies_before_coal", node_stats[BE_STAT_COPIES]);
	}

	be_timer_push(T_RA_COPYMIN);
	co_driver(chordal_env);
	be_timer_pop(T_RA_COPYMIN);

	dump(BE_CH_DUMP_COPYMIN, irg, chordal_env->cls, "copymin");

	/* ssa destruction */
	be_timer_push(T_RA_SSA);
	be_ssa_destruction(chordal_env->irg, chordal_env->cls);
	be_timer_pop(T_RA_SSA);

	dump(BE_CH_DUMP_SSADESTR, irg, chordal_env->cls, "ssadestr");

	if (be_options.do_verify) {
		be_timer_push(T_VERIFY);
		be_ssa_destruction_check(chordal_env->irg, chordal_env->cls);
		be_timer_pop(T_VERIFY);
	}

	/* the ifg exists only if there are allocatable regs */
	be_ifg_free(chordal_env->ifg);

	/* free some always allocated data structures */
	pmap_destroy(chordal_env->border_heads);
	free(chordal_env->allocatable_regs);
}

/**
 * Performs chordal register allocation for each register class on given irg.
 *
 * @param irg    the graph
 * @return Structure containing timer for the single phases or NULL if no
 *         timing requested.
 */
static void be_ra_chordal_main(ir_graph *irg)
{
	be_timer_push(T_RA_OTHER);

	be_chordal_env_t chordal_env;
	obstack_init(&chordal_env.obst);
	chordal_env.opts             = &options;
	chordal_env.irg              = irg;
	chordal_env.border_heads     = NULL;
	chordal_env.ifg              = NULL;
	chordal_env.allocatable_regs = NULL;

	if (stat_ev_enabled)
		be_collect_node_stats(&last_node_stats, irg);

	/* use one of the generic spiller */

	/* Perform the following for each register class. */
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);
	for (int j = 0, m = arch_env->n_register_classes; j < m; ++j) {
		const arch_register_class_t *cls = &arch_env->register_classes[j];
		if (arch_register_class_flags(cls) & arch_register_class_flag_manual_ra)
			continue;

		stat_ev_ctx_push_str("bechordal_cls", cls->name);

		double pre_spill_cost = 0;
		if (stat_ev_enabled) {
			be_do_stat_reg_pressure(irg, cls);
			pre_spill_cost = be_estimate_irg_costs(irg);
		}

		pre_spill(&chordal_env, cls, irg);

		be_timer_push(T_RA_SPILL);
		be_do_spill(irg, cls);
		be_timer_pop(T_RA_SPILL);
		dump(BE_CH_DUMP_SPILL, irg, cls, "spill");
		stat_ev_dbl("bechordal_spillcosts", be_estimate_irg_costs(irg) - pre_spill_cost);

		post_spill(&chordal_env, irg);

		if (stat_ev_enabled) {
			be_node_stats_t node_stats;
			be_collect_node_stats(&node_stats, irg);
			be_subtract_node_stats(&node_stats, &last_node_stats);
			be_emit_node_stats(&node_stats, "bechordal_");
			be_copy_node_stats(&last_node_stats, &node_stats);
			stat_ev_ctx_pop("bechordal_cls");
		}
	}

	be_timer_push(T_RA_EPILOG);
	lower_nodes_after_ra(irg, options.lower_perm_opt == BE_CH_LOWER_PERM_COPY);
	dump(BE_CH_DUMP_LOWER, irg, NULL, "belower-after-ra");

	obstack_free(&chordal_env.obst, NULL);
	be_invalidate_live_sets(irg);
	be_timer_pop(T_RA_EPILOG);

	be_timer_pop(T_RA_OTHER);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal_main)
void be_init_chordal_main(void)
{
	lc_opt_entry_t *be_grp      = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp      = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");

	static be_ra_t be_ra_chordal_allocator = {
		be_ra_chordal_main,
	};
	be_register_allocator("chordal", &be_ra_chordal_allocator);

	lc_opt_add_table(chordal_grp, be_chordal_options);
	be_add_module_list_opt(chordal_grp, "coloring", "select coloring method",
	                       &colorings, (void**) &selected_coloring);
}
