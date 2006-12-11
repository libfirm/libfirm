/**
 * @file   bechordal_main.c
 * @date   29.11.2005
 * @author Sebastian Hack
 * @cvs-id $Id$
 *
 * Copyright (C) 2005-2006 Universitaet Karlsruhe
 * Released under the GPL
 *
 * Driver for the chordal register allocator.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <time.h>

#include "obst.h"
#include "pset.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"
#include "firm_config.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>
#endif /* WITH_LIBCORE */

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

#include "bechordal_t.h"
#include "beabi.h"
#include "bejavacoal.h"
#include "beutil.h"
#include "besched.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bearch.h"
#include "beifg_t.h"
#include "beifg_impl.h"
#include "benode_t.h"
#include "bestatevent.h"
#include "bestat.h"

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
#include "bespillcost.h"
#include "benode_t.h"

void be_ra_chordal_check(be_chordal_env_t *chordal_env) {
	const arch_env_t *arch_env = chordal_env->birg->main_env->arch_env;
	struct obstack ob;
	pmap_entry *pme;
	ir_node **nodes, *n1, *n2;
	int i, o;
	be_lv_t *lv = chordal_env->birg->lv;
	DEBUG_ONLY(firm_dbg_module_t *dbg = chordal_env->dbg;)

	/* Collect all irns */
	obstack_init(&ob);
	pmap_foreach(chordal_env->border_heads, pme) {
		border_t *curr;
		struct list_head *head = pme->value;
		list_for_each_entry(border_t, curr, head, list)
			if (curr->is_def && curr->is_real)
				if (arch_get_irn_reg_class(arch_env, curr->irn, -1) == chordal_env->cls)
					obstack_ptr_grow(&ob, curr->irn);
	}
	obstack_ptr_grow(&ob, NULL);
	nodes = (ir_node **) obstack_finish(&ob);

	/* Check them */
	for (i = 0, n1 = nodes[i]; n1; n1 = nodes[++i]) {
		const arch_register_t *n1_reg, *n2_reg;

		n1_reg = arch_get_irn_register(arch_env, n1);
		if (!arch_reg_is_allocatable(arch_env, n1, -1, n1_reg)) {
			DBG((dbg, 0, "Register %s assigned to %+F is not allowed\n", n1_reg->name, n1));
			assert(0 && "Register constraint does not hold");
		}
		for (o = i+1, n2 = nodes[o]; n2; n2 = nodes[++o]) {
			n2_reg = arch_get_irn_register(arch_env, n2);
			if (values_interfere(lv, n1, n2) && n1_reg == n2_reg) {
				DBG((dbg, 0, "Values %+F and %+F interfere and have the same register assigned: %s\n", n1, n2, n1_reg->name));
				assert(0 && "Interfering values have the same color!");
			}
		}
	}
	obstack_free(&ob, NULL);
}

int nodes_interfere(const be_chordal_env_t *env, const ir_node *a, const ir_node *b)
{
	if(env->ifg)
		return be_ifg_connected(env->ifg, a, b);
	else
		return values_interfere(env->birg->lv, a, b);
}


static be_ra_chordal_opts_t options = {
	BE_CH_DUMP_NONE,
	BE_CH_SPILL_BELADY,
	BE_CH_IFG_STD,
	BE_CH_LOWER_PERM_SWAP,
	BE_CH_VRFY_WARN,
};

/** Enable extreme live range splitting. */
static int be_elr_split = 0;

#ifdef WITH_LIBCORE
/** Assumed loop iteration count for execution frequency estimation. */
static int be_loop_weight = 9;

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

static const lc_opt_enum_int_items_t spill_items[] = {
	{ "morgan", BE_CH_SPILL_MORGAN },
	{ "belady", BE_CH_SPILL_BELADY },
#ifdef WITH_ILP
	{ "remat",  BE_CH_SPILL_REMAT },
#endif /* WITH_ILP */
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t ifg_flavor_items[] = {
	{ "std",     BE_CH_IFG_STD     },
	{ "fast",    BE_CH_IFG_FAST    },
	{ "clique",  BE_CH_IFG_CLIQUE  },
	{ "pointer", BE_CH_IFG_POINTER },
	{ "list",    BE_CH_IFG_LIST    },
	{ "check",   BE_CH_IFG_CHECK   },
	{ NULL,      0                 }
};

static const lc_opt_enum_int_items_t lower_perm_items[] = {
	{ "copy", BE_CH_LOWER_PERM_COPY },
	{ "swap", BE_CH_LOWER_PERM_SWAP },
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t lower_perm_stat_items[] = {
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t dump_items[] = {
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

static lc_opt_enum_int_var_t spill_var = {
	&options.spill_method, spill_items
};

static lc_opt_enum_int_var_t ifg_flavor_var = {
	&options.ifg_flavor, ifg_flavor_items
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
	LC_OPT_ENT_ENUM_INT ("spill",	      "spill method", &spill_var),
	LC_OPT_ENT_ENUM_PTR ("ifg",           "interference graph flavour", &ifg_flavor_var),
	LC_OPT_ENT_ENUM_PTR ("perm",          "perm lowering options", &lower_perm_var),
	LC_OPT_ENT_ENUM_MASK("dump",          "select dump phases", &dump_var),
	LC_OPT_ENT_ENUM_PTR ("vrfy",          "verify options", &be_ch_vrfy_var),
	LC_OPT_ENT_BOOL     ("elrsplit",      "enable extreme live range splitting", &be_elr_split),
	LC_OPT_ENT_INT      ("loop_weight",   "assumed amount of loop iterations for guessing the execution frequency", &be_loop_weight),
	{ NULL }
};

extern void be_spill_remat_register_options(lc_opt_entry_t *ent);


static void be_ra_chordal_register_options(lc_opt_entry_t *grp)
{
	static int run_once = 0;
	lc_opt_entry_t *chordal_grp;

	if (! run_once) {
		run_once    = 1;
		chordal_grp = lc_opt_get_grp(grp, "chordal");

		lc_opt_add_table(chordal_grp, be_chordal_options);

		co_register_options(chordal_grp);
#ifdef WITH_JVM
		be_java_coal_register_options(chordal_grp);
#endif
#ifdef WITH_ILP
		be_spill_remat_register_options(chordal_grp);
#endif
		be_spill_register_options(chordal_grp);
	}
}
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

static void put_ignore_colors(be_chordal_env_t *chordal_env)
{
	int n_colors = chordal_env->cls->n_regs;
	int i;

	bitset_clear_all(chordal_env->ignore_colors);
	be_abi_put_ignore_regs(chordal_env->birg->abi, chordal_env->cls, chordal_env->ignore_colors);
	for(i = 0; i < n_colors; ++i)
		if(arch_register_type_is(&chordal_env->cls->regs[i], ignore))
			bitset_set(chordal_env->ignore_colors, i);
}

FILE *be_chordal_open(const be_chordal_env_t *env, const char *prefix, const char *suffix)
{
	char buf[1024];

	ir_snprintf(buf, sizeof(buf), "%s%F_%s%s", prefix, env->irg, env->cls->name, suffix);
	return fopen(buf, "wt");
}

void check_ifg_implementations(be_chordal_env_t *chordal_env)
{
	FILE *f;

	f = be_chordal_open(chordal_env, "std", ".log");
	chordal_env->ifg = be_ifg_std_new(chordal_env);
	be_ifg_check_sorted_to_file(chordal_env->ifg, f);
	fclose(f);

	f = be_chordal_open(chordal_env, "list", ".log");
	be_ifg_free(chordal_env->ifg);
	chordal_env->ifg = be_ifg_list_new(chordal_env);
	be_ifg_check_sorted_to_file(chordal_env->ifg, f);
	fclose(f);

	f = be_chordal_open(chordal_env, "clique", ".log");
	be_ifg_free(chordal_env->ifg);
	chordal_env->ifg = be_ifg_clique_new(chordal_env);
	be_ifg_check_sorted_to_file(chordal_env->ifg, f);
	fclose(f);

	f = be_chordal_open(chordal_env, "pointer", ".log");
	be_ifg_free(chordal_env->ifg);
	chordal_env->ifg = be_ifg_pointer_new(chordal_env);
	be_ifg_check_sorted_to_file(chordal_env->ifg, f);
	fclose(f);

	chordal_env->ifg = NULL;
};

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

	/* always use addressmode, it's good for x86 */
#if 0
	/* only use memory operands, if the reload is only used by 1 node */
	if(get_irn_n_edges(irn) > 1)
		return;
#endif

	spill = be_get_Reload_mem(irn);
	block = get_nodes_block(irn);

	foreach_out_edge_safe(irn, edge, ne) {
		ir_node *src = get_edge_src_irn(edge);
		int     pos  = get_edge_src_pos(edge);

		assert(src && "outedges broken!");

		if (get_nodes_block(src) == block && arch_possible_memory_operand(aenv, src, pos)) {
			DBG((cenv->dbg, LEVEL_3, "performing memory operand %+F at %+F\n", irn, src));
			//arch_perform_memory_operand(aenv, src, spill, pos);
		}
	}

	/* kill the Reload */
	if (get_irn_n_edges(irn) == 0) {
		sched_remove(irn);
		set_irn_n(irn, 0, new_Bad());
		set_irn_n(irn, 1, new_Bad());
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
	node_stat_t *stat;
	const be_chordal_env_t *cenv;
	bitset_t *mem_phis;
};

static void node_stat_walker(ir_node *irn, void *data)
{
	struct node_stat_walker *env = data;
	const arch_env_t *aenv       = env->cenv->birg->main_env->arch_env;

	if(arch_irn_consider_in_reg_alloc(aenv, env->cenv->cls, irn)) {

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

static void node_stats(const be_chordal_env_t *cenv, node_stat_t *stat)
{
	struct node_stat_walker env;

	memset(stat, 0, sizeof(stat[0]));
	env.cenv     = cenv;
	env.mem_phis = bitset_irg_malloc(cenv->irg);
	env.stat     = stat;
	irg_walk_graph(cenv->irg, NULL, node_stat_walker, &env);
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

#ifdef WITH_LIBCORE
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
#else

#define BE_TIMER_INIT(main_opts)
#define BE_TIMER_PUSH(timer)
#define BE_TIMER_POP(timer)

#endif /* WITH_LIBCORE */

/**
 * Performs chordal register allocation for each register class on given irg.
 *
 * @param birg  Backend irg object
 * @return Structure containing timer for the single phases or NULL if no timing requested.
 */
static be_ra_timer_t *be_ra_chordal_main(be_irg_t *birg)
{
	const be_main_env_t *main_env  = birg->main_env;
	const arch_isa_t    *isa       = arch_env_get_isa(main_env->arch_env);
	ir_graph            *irg       = birg->irg;
	be_options_t        *main_opts = main_env->options;
	int                  splitted = 0;

	int j, m;
	be_chordal_env_t chordal_env;

	BE_TIMER_INIT(main_opts);
	BE_TIMER_PUSH(ra_timer.t_other);
	BE_TIMER_PUSH(ra_timer.t_prolog);

	be_assure_dom_front(birg);
	be_assure_liveness(birg);

	chordal_env.opts      = &options;
	chordal_env.irg       = irg;
	chordal_env.birg      = birg;
	FIRM_DBG_REGISTER(chordal_env.dbg, "firm.be.chordal");

	obstack_init(&chordal_env.obst);

	BE_TIMER_POP(ra_timer.t_prolog);

	be_stat_ev("insns_before", count_insns(irg));

	/* Perform the following for each register class. */
	for (j = 0, m = arch_isa_get_n_reg_class(isa); j < m; ++j) {
		node_stat_t node_stat;
		double spillcosts = 0;

		chordal_env.cls           = arch_isa_get_reg_class(isa, j);
		chordal_env.border_heads  = pmap_create();
		chordal_env.ignore_colors = bitset_malloc(chordal_env.cls->n_regs);

#ifdef FIRM_STATISTICS
		if(be_stat_ev_is_active()) {
			be_stat_tags[STAT_TAG_CLS] = chordal_env.cls->name;
			be_stat_ev_push(be_stat_tags, STAT_TAG_LAST, be_stat_file);

			/* perform some node statistics. */
			node_stats(&chordal_env, &node_stat);
			be_stat_ev("phis_before_spill", node_stat.n_phis);
		}
#endif

		/* put all ignore registers into the ignore register set. */
		put_ignore_colors(&chordal_env);

		be_pre_spill_prepare_constr(&chordal_env);
		dump(BE_CH_DUMP_CONSTR, irg, chordal_env.cls, "-constr-pre", dump_ir_block_graph_sched);

		if(be_stat_ev_is_active()) {
			spillcosts = be_estimate_irg_costs(irg, main_env->arch_env, birg->exec_freq);
		}

		BE_TIMER_PUSH(ra_timer.t_spill);

		/* spilling */
		switch(options.spill_method) {
		case BE_CH_SPILL_MORGAN:
			be_spill_morgan(&chordal_env);
			break;
		case BE_CH_SPILL_BELADY:
			be_spill_belady(&chordal_env);
			break;
#ifdef WITH_ILP
		case BE_CH_SPILL_REMAT:
			be_spill_remat(&chordal_env);
			break;
#endif /* WITH_ILP */
		default:
			fprintf(stderr, "no valid spiller selected. falling back to belady\n");
			be_spill_belady(&chordal_env);
		}

		BE_TIMER_POP(ra_timer.t_spill);

		if(be_stat_ev_is_active()) {
			spillcosts = be_estimate_irg_costs(irg, main_env->arch_env, birg->exec_freq) - spillcosts;
			be_stat_ev_l("spillcosts", (long) spillcosts);

			node_stats(&chordal_env, &node_stat);
			be_stat_ev("phis_after_spill", node_stat.n_phis);
			be_stat_ev("mem_phis", node_stat.n_mem_phis);
			be_stat_ev("reloads", node_stat.n_reloads);
			be_stat_ev("spills", node_stat.n_spills);
		}

		DBG((chordal_env.dbg, LEVEL_1, "spill costs for %+F in regclass %s: %g\n",irg, chordal_env.cls->name, get_irg_spill_cost(&chordal_env)));

		dump(BE_CH_DUMP_SPILL, irg, chordal_env.cls, "-spill", dump_ir_block_graph_sched);

		check_for_memory_operands(&chordal_env);

		be_abi_fix_stack_nodes(birg->abi, birg->lv);

		BE_TIMER_PUSH(ra_timer.t_verify);

		/* verify schedule and register pressure */
		if (options.vrfy_option == BE_CH_VRFY_WARN) {
			be_verify_schedule(irg);
			be_verify_register_pressure(chordal_env.birg, chordal_env.cls, irg);
		}
		else if (options.vrfy_option == BE_CH_VRFY_ASSERT) {
			assert(be_verify_schedule(irg) && "Schedule verification failed");
			assert(be_verify_register_pressure(chordal_env.birg, chordal_env.cls, irg)
				&& "Register pressure verification failed");
		}
		BE_TIMER_POP(ra_timer.t_verify);

		if (be_elr_split && ! splitted) {
			extreme_liverange_splitting(&chordal_env);
			splitted = 1;
		}


		/* Color the graph. */
		BE_TIMER_PUSH(ra_timer.t_color);
		be_ra_chordal_color(&chordal_env);
		BE_TIMER_POP(ra_timer.t_color);

		dump(BE_CH_DUMP_CONSTR, irg, chordal_env.cls, "-color", dump_ir_block_graph_sched);

		/* Create the ifg with the selected flavor */
		BE_TIMER_PUSH(ra_timer.t_ifg);
		switch (options.ifg_flavor) {
			default:
				fprintf(stderr, "no valid ifg flavour selected. falling back to std\n");
			case BE_CH_IFG_STD:
			case BE_CH_IFG_FAST:
				chordal_env.ifg = be_ifg_std_new(&chordal_env);
				break;
			case BE_CH_IFG_CLIQUE:
				chordal_env.ifg = be_ifg_clique_new(&chordal_env);
				break;
			case BE_CH_IFG_POINTER:
				chordal_env.ifg = be_ifg_pointer_new(&chordal_env);
				break;
			case BE_CH_IFG_LIST:
				chordal_env.ifg = be_ifg_list_new(&chordal_env);
				break;
			case BE_CH_IFG_CHECK:
				check_ifg_implementations(&chordal_env);
				/* Build the interference graph. */
				chordal_env.ifg = be_ifg_std_new(&chordal_env);
				break;
		}
		BE_TIMER_POP(ra_timer.t_ifg);

		if(be_stat_ev_is_active()) {
			be_ifg_stat_t stat;
			be_ifg_stat(&chordal_env, &stat);
			be_stat_ev("ifg_nodes", stat.n_nodes);
			be_stat_ev("ifg_edges", stat.n_edges);
			be_stat_ev("ifg_comps", stat.n_comps);
		}

		BE_TIMER_PUSH(ra_timer.t_verify);
		if (options.vrfy_option != BE_CH_VRFY_OFF) {
			//be_ra_chordal_check(&chordal_env);
		}

		BE_TIMER_POP(ra_timer.t_verify);

		if(be_stat_ev_is_active()) {
			node_stats(&chordal_env, &node_stat);
			be_stat_ev("perms_before_coal", node_stat.n_perms);
			be_stat_ev("copies_before_coal", node_stat.n_copies);
		}

		/* copy minimization */
		BE_TIMER_PUSH(ra_timer.t_copymin);
		co_driver(&chordal_env);
		BE_TIMER_POP(ra_timer.t_copymin);

		dump(BE_CH_DUMP_COPYMIN, irg, chordal_env.cls, "-copymin", dump_ir_block_graph_sched);

		BE_TIMER_PUSH(ra_timer.t_verify);

		if (options.vrfy_option != BE_CH_VRFY_OFF) {
			//be_ra_chordal_check(&chordal_env);
		}

		BE_TIMER_POP(ra_timer.t_verify);
		BE_TIMER_PUSH(ra_timer.t_ssa);

		/* ssa destruction */
		be_ssa_destruction(&chordal_env);

		BE_TIMER_POP(ra_timer.t_ssa);

		dump(BE_CH_DUMP_SSADESTR, irg, chordal_env.cls, "-ssadestr", dump_ir_block_graph_sched);

		BE_TIMER_PUSH(ra_timer.t_verify);
		if (options.vrfy_option != BE_CH_VRFY_OFF) {
			be_ssa_destruction_check(&chordal_env);
			//be_ra_chordal_check(&chordal_env);
		}
		BE_TIMER_POP(ra_timer.t_verify);

		be_ifg_free(chordal_env.ifg);
		pmap_destroy(chordal_env.border_heads);
		bitset_free(chordal_env.ignore_colors);

		if(be_stat_ev_is_active()) {
			node_stats(&chordal_env, &node_stat);
			be_stat_ev("perms_after_coal", node_stat.n_perms);
			be_stat_ev("copies_after_coal", node_stat.n_copies);
		}

		be_stat_ev_pop();
	}

	BE_TIMER_PUSH(ra_timer.t_spillslots);

	be_coalesce_spillslots(&chordal_env);
	dump(BE_CH_DUMP_SPILLSLOTS, irg, NULL, "-spillslots", dump_ir_block_graph_sched);

	BE_TIMER_POP(ra_timer.t_spillslots);

	BE_TIMER_PUSH(ra_timer.t_verify);

	/* verify spillslots */
	if (options.vrfy_option == BE_CH_VRFY_WARN) {
		be_verify_spillslots(main_env->arch_env, irg);
	}
	else if (options.vrfy_option == BE_CH_VRFY_ASSERT) {
		assert(be_verify_spillslots(main_env->arch_env, irg) && "Spillslot verification failed");
	}
	BE_TIMER_POP(ra_timer.t_verify);

	BE_TIMER_PUSH(ra_timer.t_epilog);

	dump(BE_CH_DUMP_LOWER, irg, NULL, "-spilloff", dump_ir_block_graph_sched);

	lower_nodes_after_ra(&chordal_env, options.lower_perm_opt & BE_CH_LOWER_PERM_COPY ? 1 : 0);
	dump(BE_CH_DUMP_LOWER, irg, NULL, "-belower-after-ra", dump_ir_block_graph_sched);

	obstack_free(&chordal_env.obst, NULL);

	BE_TIMER_POP(ra_timer.t_epilog);
	BE_TIMER_POP(ra_timer.t_other);

	be_stat_ev("insns_after", count_insns(irg));

#ifdef WITH_LIBCORE
	return main_opts->timing == BE_TIME_ON ? &ra_timer : NULL;
#endif /* WITH_LIBCORE */
	return NULL;
}

const be_ra_t be_ra_chordal_allocator = {
#ifdef WITH_LIBCORE
	be_ra_chordal_register_options,
#else
	NULL,
#endif
	be_ra_chordal_main,
};
