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
 * @brief       Driver for the chordal register allocator.
 * @author      Sebastian Hack
 * @date        29.11.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <time.h>

#include "obst.h"
#include "pset.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"
#include "firm_config.h"

#include "ircons_t.h"
#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irdump.h"
#include "irdom.h"
#include "ircons.h"
#include "irbitset.h"
#include "irnode.h"
#include "ircons.h"
#include "debug.h"
#include "xmalloc.h"
#include "execfreq.h"
#include "iredges_t.h"

#include "bechordal_t.h"
#include "beabi.h"
#include "bejavacoal.h"
#include "beutil.h"
#include "besched.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bearch_t.h"
#include "beifg_t.h"
#include "beifg_impl.h"
#include "benode_t.h"
#include "bestatevent.h"
#include "bestat.h"
#include "bemodule.h"
#include "be_t.h"
#include "bera.h"
#include "beirg_t.h"

#include "bespillbelady.h"
#include "bespillmorgan.h"
#include "bespillslots.h"
#include "bespilloptions.h"
#include "belower.h"

#ifdef WITH_ILP
#include "bespillremat.h"
#endif /* WITH_ILP */

#include "bejavacoal.h"
#include "becopystat.h"
#include "becopyopt.h"
#include "bessadestr.h"
#include "beverify.h"
#include "benode_t.h"

static be_ra_chordal_opts_t options = {
	BE_CH_DUMP_NONE,
	BE_CH_LOWER_PERM_SWAP,
	BE_CH_VRFY_WARN,
	"",
	""
};

typedef struct _post_spill_env_t {
	be_chordal_env_t            cenv;
	be_irg_t                    *birg;
	const arch_register_class_t *cls;
	double                      pre_spill_cost;
} post_spill_env_t;

static be_ra_timer_t ra_timer = {
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>

static const lc_opt_enum_int_items_t lower_perm_items[] = {
	{ "copy", BE_CH_LOWER_PERM_COPY },
	{ "swap", BE_CH_LOWER_PERM_SWAP },
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t lower_perm_stat_items[] = {
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t dump_items[] = {
	{ "none",       BE_CH_DUMP_NONE       },
	{ "spill",      BE_CH_DUMP_SPILL      },
	{ "live",       BE_CH_DUMP_LIVE       },
	{ "color",      BE_CH_DUMP_COLOR      },
	{ "copymin",    BE_CH_DUMP_COPYMIN    },
	{ "ssadestr",   BE_CH_DUMP_SSADESTR   },
	{ "tree",       BE_CH_DUMP_TREE_INTV  },
	{ "constr",     BE_CH_DUMP_CONSTR     },
	{ "lower",      BE_CH_DUMP_LOWER      },
	{ "spillslots", BE_CH_DUMP_SPILLSLOTS },
	{ "appel",      BE_CH_DUMP_APPEL      },
	{ "all",        BE_CH_DUMP_ALL        },
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t be_ch_vrfy_items[] = {
	{ "off",    BE_CH_VRFY_OFF    },
	{ "warn",   BE_CH_VRFY_WARN   },
	{ "assert", BE_CH_VRFY_ASSERT },
	{ NULL, 0 }
};

static lc_opt_enum_int_var_t lower_perm_var = {
	&options.lower_perm_opt, lower_perm_items
};

static lc_opt_enum_int_var_t dump_var = {
	&options.dump_flags, dump_items
};

static lc_opt_enum_int_var_t be_ch_vrfy_var = {
	&options.vrfy_option, be_ch_vrfy_items
};

static const lc_opt_table_entry_t be_chordal_options[] = {
	LC_OPT_ENT_ENUM_PTR ("perm",          "perm lowering options", &lower_perm_var),
	LC_OPT_ENT_ENUM_MASK("dump",          "select dump phases", &dump_var),
	LC_OPT_ENT_ENUM_PTR ("vrfy",          "verify options", &be_ch_vrfy_var),
	LC_OPT_LAST
};
#endif /* WITH_LIBCORE */

static void dump(unsigned mask, ir_graph *irg,
				 const arch_register_class_t *cls,
				 const char *suffix,
				 void (*dump_func)(ir_graph *, const char *))
{
	if((options.dump_flags & mask) == mask) {
		if (cls) {
			char buf[256];
			snprintf(buf, sizeof(buf), "-%s%s", cls->name, suffix);
			be_dump(irg, buf, dump_func);
		}
		else
			be_dump(irg, suffix, dump_func);
	}
}

/**
 * Checks for every reload if it's user can perform the load on itself.
 */
static void memory_operand_walker(ir_node *irn, void *env) {
	be_chordal_env_t *cenv = env;
	const arch_env_t *aenv = cenv->birg->main_env->arch_env;
	const ir_edge_t  *edge, *ne;
	ir_node          *block;
	ir_node          *spill;

	if (! be_is_Reload(irn))
		return;

	/* only use memory operands, if the reload is only used by 1 node */
	if(get_irn_n_edges(irn) > 1)
		return;

	spill = be_get_Reload_mem(irn);
	block = get_nodes_block(irn);

	foreach_out_edge_safe(irn, edge, ne) {
		ir_node *src = get_edge_src_irn(edge);
		int     pos  = get_edge_src_pos(edge);

		assert(src && "outedges broken!");

		if (get_nodes_block(src) == block && arch_possible_memory_operand(aenv, src, pos)) {
			arch_perform_memory_operand(aenv, src, spill, pos);
		}
	}

	/* kill the Reload */
	if (get_irn_n_edges(irn) == 0) {
		sched_remove(irn);
		set_irn_n(irn, be_pos_Reload_mem, new_Bad());
		set_irn_n(irn, be_pos_Reload_frame, new_Bad());
	}
}

/**
 * Starts a walk for memory operands if supported by the backend.
 */
static INLINE void check_for_memory_operands(be_chordal_env_t *chordal_env) {
	irg_walk_graph(chordal_env->irg, NULL, memory_operand_walker, chordal_env);
}

/**
 * Sorry for doing stats again...
 */
typedef struct _node_stat_t {
	unsigned int n_phis;      /**< Phis of the current register class. */
	unsigned int n_mem_phis;  /**< Memory Phis (Phis with spill operands). */
	unsigned int n_copies;    /**< Copies */
	unsigned int n_perms;     /**< Perms */
	unsigned int n_spills;    /**< Spill nodes */
	unsigned int n_reloads;   /**< Reloads */
} node_stat_t;

struct node_stat_walker {
	node_stat_t      *stat;
	const arch_env_t *arch_env;
	bitset_t         *mem_phis;
	const arch_register_class_t *cls;
};

static void node_stat_walker(ir_node *irn, void *data)
{
	struct node_stat_walker *env  = data;
	const arch_env_t        *aenv = env->arch_env;

	if (arch_irn_consider_in_reg_alloc(aenv, env->cls, irn)) {

		/* if the node is a normal phi */
		if(is_Phi(irn))
			env->stat->n_phis++;

		else if(arch_irn_classify(aenv, irn) & arch_irn_class_spill)
			++env->stat->n_spills;

		else if(arch_irn_classify(aenv, irn) & arch_irn_class_reload)
			++env->stat->n_reloads;

		else if(arch_irn_classify(aenv, irn) & arch_irn_class_copy)
			++env->stat->n_copies;

		else if(arch_irn_classify(aenv, irn) & arch_irn_class_perm)
			++env->stat->n_perms;
	}

	/* a mem phi is a PhiM with a mem phi operand or a Spill operand */
	else if(is_Phi(irn) && get_irn_mode(irn) == mode_M) {
		int i;

		for(i = get_irn_arity(irn) - 1; i >= 0; --i) {
			ir_node *op = get_irn_n(irn, i);

			if((is_Phi(op) && bitset_contains_irn(env->mem_phis, op)) || (arch_irn_classify(aenv, op) & arch_irn_class_spill)) {
				bitset_add_irn(env->mem_phis, irn);
				env->stat->n_mem_phis++;
				break;
			}
		}
	}
}

static void node_stats(be_irg_t *birg, const arch_register_class_t *cls, node_stat_t *stat)
{
	struct node_stat_walker env;

	memset(stat, 0, sizeof(stat[0]));
	env.arch_env = birg->main_env->arch_env;
	env.mem_phis = bitset_irg_malloc(birg->irg);
	env.stat     = stat;
	env.cls      = cls;
	irg_walk_graph(birg->irg, NULL, node_stat_walker, &env);
	bitset_free(env.mem_phis);
}

static void insn_count_walker(ir_node *irn, void *data)
{
	int *cnt = data;

	switch(get_irn_opcode(irn)) {
	case iro_Proj:
	case iro_Phi:
	case iro_Start:
	case iro_End:
		break;
	default:
		(*cnt)++;
	}
}

static unsigned int count_insns(ir_graph *irg)
{
	int cnt = 0;
	irg_walk_graph(irg, insn_count_walker, NULL, &cnt);
	return cnt;
}

/**
 * Initialize all timers.
 */
static void be_init_timer(be_options_t *main_opts)
{
	if (main_opts->timing == BE_TIME_ON) {
		ra_timer.t_prolog     = lc_timer_register("ra_prolog",     "regalloc prolog");
		ra_timer.t_epilog     = lc_timer_register("ra_epilog",     "regalloc epilog");
		ra_timer.t_live       = lc_timer_register("ra_liveness",   "be liveness");
		ra_timer.t_spill      = lc_timer_register("ra_spill",      "spiller");
		ra_timer.t_spillslots = lc_timer_register("ra_spillslots", "spillslots");
		ra_timer.t_color      = lc_timer_register("ra_color",      "graph coloring");
		ra_timer.t_ifg        = lc_timer_register("ra_ifg",        "interference graph");
		ra_timer.t_copymin    = lc_timer_register("ra_copymin",    "copy minimization");
		ra_timer.t_ssa        = lc_timer_register("ra_ssadestr",   "ssa destruction");
		ra_timer.t_verify     = lc_timer_register("ra_verify",     "graph verification");
		ra_timer.t_other      = lc_timer_register("ra_other",      "other time");

		LC_STOP_AND_RESET_TIMER(ra_timer.t_prolog);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_epilog);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_live);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_spill);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_spillslots);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_color);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_ifg);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_copymin);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_ssa);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_verify);
		LC_STOP_AND_RESET_TIMER(ra_timer.t_other);

		global_ra_timer = &ra_timer;
	}
}

#define BE_TIMER_INIT(main_opts)	be_init_timer(main_opts)

#define BE_TIMER_PUSH(timer)                                                            \
	if (main_opts->timing == BE_TIME_ON) {                                              \
		if (! lc_timer_push(timer)) {                                                   \
			if (options.vrfy_option == BE_CH_VRFY_ASSERT)                               \
				assert(!"Timer already on stack, cannot be pushed twice.");             \
			else if (options.vrfy_option == BE_CH_VRFY_WARN)                            \
				fprintf(stderr, "Timer %s already on stack, cannot be pushed twice.\n", \
					lc_timer_get_name(timer));                                          \
		}                                                                               \
	}
#define BE_TIMER_POP(timer)                                                                    \
	if (main_opts->timing == BE_TIME_ON) {                                                     \
		lc_timer_t *tmp = lc_timer_pop();                                                      \
		if (options.vrfy_option == BE_CH_VRFY_ASSERT)                                          \
			assert(tmp == timer && "Attempt to pop wrong timer.");                             \
		else if (options.vrfy_option == BE_CH_VRFY_WARN && tmp != timer)                       \
			fprintf(stderr, "Attempt to pop wrong timer. %s is on stack, trying to pop %s.\n", \
				lc_timer_get_name(tmp), lc_timer_get_name(timer));                             \
		timer = tmp;                                                                           \
	}

/**
 * Perform things which need to be done per register class before spilling.
 */
static void pre_spill(const arch_isa_t *isa, int cls_idx, post_spill_env_t *pse) {
	be_chordal_env_t *chordal_env = &pse->cenv;
	be_irg_t         *birg        = pse->birg;
	node_stat_t      node_stat;

	pse->cls                   = arch_isa_get_reg_class(isa, cls_idx);
	chordal_env->cls           = pse->cls;
	chordal_env->border_heads  = pmap_create();
	chordal_env->ignore_colors = bitset_malloc(chordal_env->cls->n_regs);

	be_assure_liveness(birg);
	be_liveness_assure_chk(be_get_birg_liveness(birg));
	stat_ev_ctx_push("cls", pse->cls->name);
	stat_ev_dbl("phis_before_spill", node_stat.n_phis);
	stat_ev_do(node_stats(birg, pse->cls, &node_stat));

	/* put all ignore registers into the ignore register set. */
	be_put_ignore_regs(birg, pse->cls, chordal_env->ignore_colors);

	be_pre_spill_prepare_constr(chordal_env);
	dump(BE_CH_DUMP_CONSTR, birg->irg, pse->cls, "-constr-pre", dump_ir_block_graph_sched);

	stat_ev_ctx_pop();
}

/**
 * Perform things which need to be done per register class after spilling.
 */
static void post_spill(post_spill_env_t *pse, int iteration) {
	be_chordal_env_t    *chordal_env = &pse->cenv;
	be_irg_t            *birg        = pse->birg;
	ir_graph            *irg         = birg->irg;
	const be_main_env_t *main_env    = birg->main_env;
	be_options_t        *main_opts   = main_env->options;
	node_stat_t         node_stat;
	int                 colors_n     = arch_register_class_n_regs(chordal_env->cls);
	int             allocatable_regs = colors_n - be_put_ignore_regs(birg, chordal_env->cls, NULL);

	/* some special classes contain only ignore regs, no work to be done */
	if (allocatable_regs > 0) {

		stat_ev_ctx_push("cls", pse->cls->name);
		stat_ev_do(node_stats(birg, pse->cls, &node_stat));
		stat_ev_dbl("spillcosts", be_estimate_irg_costs(irg, main_env->arch_env, birg->exec_freq) - pse->pre_spill_cost);
		stat_ev_dbl("phis_after_spill", node_stat.n_phis);
		stat_ev_dbl("mem_phis", node_stat.n_mem_phis);
		stat_ev_dbl("reloads", node_stat.n_reloads);
		stat_ev_dbl("spills", node_stat.n_spills);

		/*
			If we have a backend provided spiller, post spill is
			called in a loop after spilling for each register class.
			But we only need to fix stack nodes once in this case.
		*/
		if (iteration == 0) {
			check_for_memory_operands(chordal_env);
			be_abi_fix_stack_nodes(birg->abi);
		}

		BE_TIMER_PUSH(ra_timer.t_verify);

		/* verify schedule and register pressure */
		if (chordal_env->opts->vrfy_option == BE_CH_VRFY_WARN) {
			be_verify_schedule(birg);
			be_verify_register_pressure(birg, pse->cls, irg);
		}
		else if (chordal_env->opts->vrfy_option == BE_CH_VRFY_ASSERT) {
			assert(be_verify_schedule(birg) && "Schedule verification failed");
			assert(be_verify_register_pressure(birg, pse->cls, irg)
				&& "Register pressure verification failed");
		}
		BE_TIMER_POP(ra_timer.t_verify);

		/* Color the graph. */
		BE_TIMER_PUSH(ra_timer.t_color);
		be_ra_chordal_color(chordal_env);
		BE_TIMER_POP(ra_timer.t_color);

		dump(BE_CH_DUMP_CONSTR, irg, pse->cls, "-color", dump_ir_block_graph_sched);

		/* Create the ifg with the selected flavor */
		BE_TIMER_PUSH(ra_timer.t_ifg);
		chordal_env->ifg = be_create_ifg(chordal_env);
		BE_TIMER_POP(ra_timer.t_ifg);

		{
			be_ifg_stat_t stat;

			stat_ev_do(be_ifg_stat(birg, chordal_env->ifg, &stat));
			stat_ev_dbl("ifg_nodes", stat.n_nodes);
			stat_ev_dbl("ifg_edges", stat.n_edges);
			stat_ev_dbl("ifg_comps", stat.n_comps);

			stat_ev_do(node_stats(birg, pse->cls, &node_stat));
			stat_ev_dbl("perms_before_coal", node_stat.n_perms);
			stat_ev_dbl("copies_before_coal", node_stat.n_copies);
		}

		/* copy minimization */
		BE_TIMER_PUSH(ra_timer.t_copymin);
		co_driver(chordal_env);
		BE_TIMER_POP(ra_timer.t_copymin);

		dump(BE_CH_DUMP_COPYMIN, irg, pse->cls, "-copymin", dump_ir_block_graph_sched);

		BE_TIMER_PUSH(ra_timer.t_ssa);

		/* ssa destruction */
		be_ssa_destruction(chordal_env);

		BE_TIMER_POP(ra_timer.t_ssa);

		dump(BE_CH_DUMP_SSADESTR, irg, pse->cls, "-ssadestr", dump_ir_block_graph_sched);

		BE_TIMER_PUSH(ra_timer.t_verify);
		if (chordal_env->opts->vrfy_option != BE_CH_VRFY_OFF) {
			be_ssa_destruction_check(chordal_env);
		}
		BE_TIMER_POP(ra_timer.t_verify);

		stat_ev_do(node_stats(birg, pse->cls, &node_stat));
		stat_ev_dbl("perms_after_coal", node_stat.n_perms);
		stat_ev_dbl("copies_after_coal", node_stat.n_copies);
		stat_ev_ctx_pop();

		/* the ifg exists only if there are allocatable regs */
		be_ifg_free(chordal_env->ifg);
	}

	/* free some always allocated data structures */
	pmap_destroy(chordal_env->border_heads);
	bitset_free(chordal_env->ignore_colors);
}

/**
 * Performs chordal register allocation for each register class on given irg.
 *
 * @param birg  Backend irg object
 * @return Structure containing timer for the single phases or NULL if no timing requested.
 */
static void be_ra_chordal_main(be_irg_t *birg)
{
	const be_main_env_t *main_env  = birg->main_env;
	const arch_isa_t    *isa       = arch_env_get_isa(main_env->arch_env);
	ir_graph            *irg       = birg->irg;
	be_options_t        *main_opts = main_env->options;
	int                 j, m;
	be_chordal_env_t    chordal_env;
	struct obstack      obst;

	BE_TIMER_INIT(main_opts);
	BE_TIMER_PUSH(ra_timer.t_other);
	BE_TIMER_PUSH(ra_timer.t_prolog);

	be_assure_dom_front(birg);
	be_assure_liveness(birg);

	chordal_env.obst          = &obst;
	chordal_env.opts          = &options;
	chordal_env.irg           = irg;
	chordal_env.birg          = birg;
	chordal_env.border_heads  = NULL;
	chordal_env.ifg           = NULL;
	chordal_env.ignore_colors = NULL;

	obstack_init(&obst);

	BE_TIMER_POP(ra_timer.t_prolog);

	be_stat_ev("insns_before", count_insns(irg));

	if (! arch_code_generator_has_spiller(birg->cg)) {
		/* use one of the generic spiller */

		/* Perform the following for each register class. */
		for (j = 0, m = arch_isa_get_n_reg_class(isa); j < m; ++j) {
			post_spill_env_t pse;

			memcpy(&pse.cenv, &chordal_env, sizeof(chordal_env));
			pse.birg = birg;
			pre_spill(isa, j, &pse);

			BE_TIMER_PUSH(ra_timer.t_spill);
			be_do_spill(birg, pse.cls);
			BE_TIMER_POP(ra_timer.t_spill);

			dump(BE_CH_DUMP_SPILL, irg, pse.cls, "-spill", dump_ir_block_graph_sched);

			post_spill(&pse, 0);
		}
	} else {
		post_spill_env_t *pse;

		/* the backend has it's own spiller */
		m = arch_isa_get_n_reg_class(isa);

		pse = alloca(m * sizeof(pse[0]));

		for (j = 0; j < m; ++j) {
			memcpy(&pse[j].cenv, &chordal_env, sizeof(chordal_env));
			pse[j].birg = birg;
			pre_spill(isa, j, &pse[j]);
		}

		BE_TIMER_PUSH(ra_timer.t_spill);
		arch_code_generator_spill(birg->cg, birg);
		BE_TIMER_POP(ra_timer.t_spill);
		dump(BE_CH_DUMP_SPILL, irg, NULL, "-spill", dump_ir_block_graph_sched);

		for (j = 0; j < m; ++j) {
			post_spill(&pse[j], j);
		}
	}

	be_verify_register_allocation(main_env->arch_env, irg);


	BE_TIMER_PUSH(ra_timer.t_epilog);
	lower_nodes_after_ra(birg, options.lower_perm_opt & BE_CH_LOWER_PERM_COPY ? 1 : 0);
	dump(BE_CH_DUMP_LOWER, irg, NULL, "-belower-after-ra", dump_ir_block_graph_sched);

	obstack_free(&obst, NULL);
	be_liveness_invalidate(be_get_birg_liveness(birg));
	BE_TIMER_POP(ra_timer.t_epilog);

	BE_TIMER_POP(ra_timer.t_other);

	be_stat_ev("insns_after", count_insns(irg));

	return;
}

static be_ra_t be_ra_chordal_allocator = {
	be_ra_chordal_main,
};

void be_init_chordal_main(void)
{
#ifdef WITH_LIBCORE
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");

	lc_opt_add_table(chordal_grp, be_chordal_options);
#endif
	be_register_allocator("chordal", &be_ra_chordal_allocator);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_chordal_main);
