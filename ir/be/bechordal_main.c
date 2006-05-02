/**
 * @file   bechordal_main.c
 * @date   29.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 *
 * Driver for the chordal register allocator.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "obst.h"
#include "pset.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#endif /* WITH_LIBCORE */

#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom.h"
#include "debug.h"
#include "xmalloc.h"

#include "bechordal_t.h"
#include "beabi.h"
#include "beutil.h"
#include "besched.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bearch.h"
#include "beifg_t.h"
#include "beifg_impl.h"

#include "bespillbelady.h"
#include "belower.h"

#ifdef WITH_ILP
#include "bespillilp.h"
#endif /* WITH_ILP */

#include "becopystat.h"
#include "becopyopt.h"
#include "bessadestr.h"


void be_ra_chordal_check(be_chordal_env_t *chordal_env) {
	const arch_env_t *arch_env = chordal_env->birg->main_env->arch_env;
	struct obstack ob;
	pmap_entry *pme;
	ir_node **nodes, *n1, *n2;
	int i, o;
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
			if (values_interfere(n1, n2) && n1_reg == n2_reg) {
				DBG((dbg, 0, "Values %+F and %+F interfere and have the same register assigned\n", n1, n2));
				assert(0 && "Interfering values have the same color!");
			}
		}
	}
	obstack_free(&ob, NULL);
}

static void check_pressure_walker(ir_node *bl, void *data)
{
	be_chordal_env_t *env = data;
	int n_regs = arch_register_class_n_regs(env->cls);

	pset *live = pset_new_ptr_default();
	int step = 0;
	ir_node *irn;
	irn_live_t *li;
	DEBUG_ONLY(firm_dbg_module_t *dbg = env->dbg;)

	live_foreach(bl, li) {
		if(live_is_end(li) && chordal_has_class(env, li->irn)) {
			ir_node *irn = (ir_node *) li->irn;
			pset_insert_ptr(live, irn);
		}
	}

	DBG((dbg, LEVEL_1, "end set for %+F\n", bl));
	for(irn = pset_first(live); irn; irn = pset_next(live))
		DBG((dbg, LEVEL_1, "\t%+F\n", irn));

	sched_foreach_reverse(bl, irn) {
		int i, n;
		int pressure = pset_count(live);

		DBG((dbg, LEVEL_1, "%+10F@%+10F: pressure %d\n", bl, irn, pressure));

		if(pressure > n_regs) {
			ir_node *x;
			ir_printf("%+10F@%+10F: pressure to high: %d\n", bl, irn, pressure);
			for(x = pset_first(live); x; x = pset_next(live))
				ir_printf("\t%+10F\n", x);
		}

		if(chordal_has_class(env, irn))
			pset_remove_ptr(live, irn);

		for(i = 0, n = get_irn_arity(irn); i < n; i++) {
			ir_node *op = get_irn_n(irn, i);
			if(chordal_has_class(env, op) && !is_Phi(irn))
				pset_insert_ptr(live, op);
		}
		step++;
	}
}

void be_check_pressure(const be_chordal_env_t *env)
{
	irg_block_walk_graph(env->irg, check_pressure_walker, NULL, (void *) env);
}

int nodes_interfere(const be_chordal_env_t *env, const ir_node *a, const ir_node *b)
{
	if(env->ifg)
		return be_ifg_connected(env->ifg, a, b);
	else
		return values_interfere(a, b);
}


static be_ra_chordal_opts_t options = {
	BE_CH_DUMP_NONE,
	BE_CH_SPILL_BELADY,
	BE_CH_COPYMIN_HEUR1,
	BE_CH_IFG_STD,
	BE_CH_LOWER_PERM_SWAP,
};

#ifdef WITH_LIBCORE
static const lc_opt_enum_int_items_t spill_items[] = {
	{ "belady", BE_CH_SPILL_BELADY },
#ifdef WITH_ILP
	{ "ilp",	BE_CH_SPILL_ILP },
#endif
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t copymin_items[] = {
	{ "none",  BE_CH_COPYMIN_NONE },
	{ "heur1", BE_CH_COPYMIN_HEUR1 },
	{ "heur2", BE_CH_COPYMIN_HEUR2 },
	{ "stat",  BE_CH_COPYMIN_STAT  },
#ifdef WITH_ILP
	{ "ilp1",  BE_CH_COPYMIN_ILP1 },
	{ "ilp2",  BE_CH_COPYMIN_ILP2 },
#endif
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t ifg_flavor_items[] = {
	{ "std",  BE_CH_IFG_STD },
	{ "fast", BE_CH_IFG_FAST },
	{ NULL, 0 }
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
	{ "spill",    BE_CH_DUMP_SPILL },
	{ "live",     BE_CH_DUMP_LIVE },
	{ "color",    BE_CH_DUMP_COLOR },
	{ "copymin",  BE_CH_DUMP_COPYMIN },
	{ "ssadestr", BE_CH_DUMP_SSADESTR },
	{ "tree",     BE_CH_DUMP_TREE_INTV },
	{ "constr",   BE_CH_DUMP_CONSTR },
	{ "lower",    BE_CH_DUMP_LOWER },
	{ NULL, 0 }
};

static lc_opt_enum_int_var_t spill_var = {
	&options.spill_method, spill_items
};

static lc_opt_enum_int_var_t copymin_var = {
	&options.copymin_method, copymin_items
};

static lc_opt_enum_int_var_t ifg_flavor_var = {
	&options.spill_method, ifg_flavor_items
};

static lc_opt_enum_int_var_t lower_perm_var = {
	&options.lower_perm_opt, lower_perm_items
};

static lc_opt_enum_int_var_t dump_var = {
	&options.dump_flags, dump_items
};

static const lc_opt_table_entry_t be_chordal_options[] = {
	LC_OPT_ENT_ENUM_MASK("spill", "spill method (belady or ilp)", &spill_var),
	LC_OPT_ENT_ENUM_PTR("copymin", "copymin method (none, heur1, heur2, ilp1 or ilp2)", &copymin_var),
	LC_OPT_ENT_ENUM_PTR("ifg", "interference graph flavour (std or fast)", &ifg_flavor_var),
	LC_OPT_ENT_ENUM_MASK("perm", "perm lowering options (copy or swap)", &lower_perm_var),
	LC_OPT_ENT_ENUM_MASK("dump", "select dump phases", &dump_var),
	{ NULL }
};

static void be_ra_chordal_register_options(lc_opt_entry_t *grp)
{
	static int run_once = 0;
	lc_opt_entry_t *chordal_grp;

	if (! run_once) {
		run_once    = 1;
		chordal_grp = lc_opt_get_grp(grp, "chordal");

		lc_opt_add_table(chordal_grp, be_chordal_options);
	}
}
#endif

static void dump(unsigned mask, ir_graph *irg,
				 const arch_register_class_t *cls,
				 const char *suffix,
				 void (*dump_func)(ir_graph *, const char *))
{
	if(1 || ((options.dump_flags & mask) == mask)) {
		if(cls) {
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

static void be_ra_chordal_main(const be_irg_t *bi)
{
	const be_main_env_t *main_env = bi->main_env;
	const arch_isa_t    *isa      = arch_env_get_isa(main_env->arch_env);
	ir_graph            *irg      = bi->irg;
	copy_opt_t          *co       = NULL;

	int j, m;
	be_chordal_env_t chordal_env;

	compute_doms(irg);

	chordal_env.opts          = &options;
	chordal_env.irg           = irg;
	chordal_env.birg          = bi;
	chordal_env.dom_front     = be_compute_dominance_frontiers(irg);
	FIRM_DBG_REGISTER(chordal_env.dbg, "firm.be.chordal");

	obstack_init(&chordal_env.obst);

	/* Perform the following for each register class. */
	for(j = 0, m = arch_isa_get_n_reg_class(isa); j < m; ++j) {
		chordal_env.cls           = arch_isa_get_reg_class(isa, j);
		chordal_env.border_heads  = pmap_create();
		chordal_env.ignore_colors = bitset_malloc(chordal_env.cls->n_regs);

		/* put all ignore registers into the ignore register set. */
		put_ignore_colors(&chordal_env);

		be_liveness(irg);
		dump(BE_CH_DUMP_LIVE, irg, chordal_env.cls, "-live", dump_ir_block_graph_sched);

		/* spilling */
		switch(options.spill_method) {
		case BE_CH_SPILL_BELADY:
			be_spill_belady(&chordal_env);
			break;
#ifdef WITH_ILP
		case BE_CH_SPILL_ILP:
			be_spill_ilp(&chordal_env);
			break;
#endif /* WITH_ILP */
		default:
			fprintf(stderr, "no valid spiller selected. falling back to belady\n");
			be_spill_belady(&chordal_env);
		}
		dump(BE_CH_DUMP_SPILL, irg, chordal_env.cls, "-spill", dump_ir_block_graph_sched);
		be_liveness(irg);
		be_check_pressure(&chordal_env);

		/* Color the graph. */
		be_ra_chordal_color(&chordal_env);
		dump(BE_CH_DUMP_CONSTR, irg, chordal_env.cls, "-color", dump_ir_block_graph_sched);

		/* Build the interference graph. */
		chordal_env.ifg = be_ifg_std_new(&chordal_env);
		be_ifg_check(chordal_env.ifg);

		/* copy minimization */
		if (options.copymin_method != BE_CH_COPYMIN_NONE && options.copymin_method != BE_CH_COPYMIN_STAT) {
			co = new_copy_opt(&chordal_env, co_get_costs_loop_depth);
			co_build_ou_structure(co);
		}

		switch(options.copymin_method) {
			case BE_CH_COPYMIN_HEUR1:
				co_solve_heuristic(co);
				break;
			case BE_CH_COPYMIN_HEUR2:
				co_solve_heuristic_new(co);
				break;
			case BE_CH_COPYMIN_STAT:
				co_compare_solvers(&chordal_env);
				break;
#ifdef WITH_ILP
			case BE_CH_COPYMIN_ILP1:
				printf("FIXME: %s:%d ILP1 not yet implemented!\n", __FILE__, __LINE__);
				co_solve_ilp1(co, 60.0);
				break;
			case BE_CH_COPYMIN_ILP2:
				co_build_graph_structure(co);
				co_solve_ilp2(co, 60.0);
				co_free_graph_structure(co);
				break;
#endif /* WITH_ILP */
			case BE_CH_COPYMIN_NONE:
			default:
				break;
		}

		if (co) {
			co_free_ou_structure(co);
			free_copy_opt(co);
		}

		dump(BE_CH_DUMP_COPYMIN, irg, chordal_env.cls, "-copymin", dump_ir_block_graph_sched);
		be_ra_chordal_check(&chordal_env);

		/* ssa destruction */
		be_ssa_destruction(&chordal_env);
		dump(BE_CH_DUMP_SSADESTR, irg, chordal_env.cls, "-ssadestr", dump_ir_block_graph_sched);
		be_ssa_destruction_check(&chordal_env);
		be_ra_chordal_check(&chordal_env);

		copystat_dump(irg);

		be_ifg_free(chordal_env.ifg);
		pmap_destroy(chordal_env.border_heads);
		bitset_free(chordal_env.ignore_colors);
	}

	be_compute_spill_offsets(&chordal_env);

	dump(BE_CH_DUMP_LOWER, irg, NULL, "-spilloff", dump_ir_block_graph_sched);

	lower_nodes_after_ra(&chordal_env, options.lower_perm_opt & BE_CH_LOWER_PERM_COPY ? 1 : 0);
	dump(BE_CH_DUMP_LOWER, irg, NULL, "-belower-after-ra", dump_ir_block_graph_sched);

	obstack_free(&chordal_env.obst, NULL);
	be_free_dominance_frontiers(chordal_env.dom_front);
}

const be_ra_t be_ra_chordal_allocator = {
#ifdef WITH_LIBCORE
	be_ra_chordal_register_options,
#endif
	be_ra_chordal_main
};
