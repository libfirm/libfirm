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
#include "beutil.h"
#include "besched.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bearch.h"
#include "beifg.h"
#include "beifg_impl.h"

#include "bespillbelady.h"
#include "bespillilp.h"
#include "beconstrperm.h"
#include "belower.h"

#define DO_SSADESTR

#ifdef DO_SSADESTR
#include "bessadestr.h"
#include "becopystat.h"
#include "becopyoptmain.h"
#endif /* DO_SSADESTR */


void be_ra_chordal_check(be_chordal_env_t *chordal_env) {
	firm_dbg_module_t *dbg = chordal_env->dbg;
	const arch_env_t *arch_env = chordal_env->main_env->arch_env;
	struct obstack ob;
	pmap_entry *pme;
	ir_node **nodes, *n1, *n2;
	int i, o;

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
//			assert(0 && "Register constraint does not hold");
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
	firm_dbg_module_t *dbg = env->dbg;
	int n_regs = arch_register_class_n_regs(env->cls);

	pset *live = pset_new_ptr_default();
	int step = 0;
	ir_node *irn;
	irn_live_t *li;

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
	BE_CH_COPYMIN_HEUR,
	BE_CH_IFG_STD,
	BE_CH_LOWER_PERM_SWAP
};

#ifdef WITH_LIBCORE
static const lc_opt_enum_int_items_t spill_items[] = {
	{ "belady", BE_CH_SPILL_BELADY },
	{ "ilp",	BE_CH_SPILL_ILP },
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t copymin_items[] = {
	{ "heur", BE_CH_COPYMIN_HEUR },
	{ "ilp",  BE_CH_COPYMIN_ILP },
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t ifg_flavor_items[] = {
	{ "std",  BE_CH_IFG_STD },
	{ "fast", BE_CH_IFG_FAST },
	{ NULL, 0 }
};

static const lc_opt_enum_int_items_t lower_perm_items[] = {
	{ "swap", BE_CH_LOWER_PERM_SWAP },
	{ "copy", BE_CH_LOWER_PERM_COPY },
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
	&options.lower_perm_method, lower_perm_items
};

static void be_ra_chordal_register_options(lc_opt_entry_t *grp)
{
	lc_opt_entry_t *dump;
	grp = lc_opt_get_grp(grp, "chordal");

}
#endif

static void dump(int mask, ir_graph *irg, const char *suffix,
				 void (*dump)(ir_graph *, const char *))
{
	if((options.dump_flags & mask) == mask)
		dump(irg, suffix);
}

static void be_ra_chordal_main(const be_main_env_t *main_env, ir_graph *irg)
{
	int j, m;
	be_chordal_env_t chordal_env;
	const arch_isa_t *isa = arch_env_get_isa(main_env->arch_env);

	compute_doms(irg);

	chordal_env.irg          = irg;
	chordal_env.dbg          = firm_dbg_register("firm.be.chordal");
	chordal_env.main_env     = main_env;
	chordal_env.dom_front    = be_compute_dominance_frontiers(irg);

	obstack_init(&chordal_env.obst);

	/* Perform the following for each register class. */
	for(j = 0, m = arch_isa_get_n_reg_class(isa); j < m; ++j) {
		chordal_env.border_heads = pmap_create();
		chordal_env.cls = arch_isa_get_reg_class(isa, j);

		be_liveness(irg);

		/* spilling */
		switch(options.spill_method) {
		case BE_CH_SPILL_BELADY:
			be_spill_belady(&chordal_env);
			break;
		case BE_CH_SPILL_ILP:
			be_spill_ilp(&chordal_env);
			break;
		default:
			fprintf(stderr, "no valid spiller selected. falling back to belady\n");
			be_spill_belady(&chordal_env);
		}
		dump(BE_CH_DUMP_SPILL, irg, "-spill", dump_ir_block_graph_sched);
		be_liveness(irg);
		be_check_pressure(&chordal_env);

		/* Insert perms before reg-constrained instructions */
		be_insert_constr_perms(&chordal_env);
		dump(BE_CH_DUMP_CONSTR, irg, "-constr", dump_ir_block_graph_sched);

		be_liveness(irg);
		be_numbering(irg);
		be_check_pressure(&chordal_env);

		/* Color the graph. */
		be_ra_chordal_color(&chordal_env);

		/* Build the interference graph. */
		chordal_env.ifg = be_ifg_std_new(&chordal_env);

#ifdef DO_SSADESTR
		/* copy minimization */
		copystat_collect_cls(&chordal_env);
		be_copy_opt(&chordal_env);
		dump(BE_CH_DUMP_COPYMIN, irg, "-copymin", dump_ir_block_graph_sched);

		/* ssa destruction */
		be_ssa_destruction(&chordal_env);
		be_ssa_destruction_check(&chordal_env);
		be_ra_chordal_check(&chordal_env);
		dump(BE_CH_DUMP_SSADESTR, irg, "-ssadestr", dump_ir_block_graph_sched);

		copystat_dump(irg);
#endif /* DO_SSADESTR */

		be_ifg_free(chordal_env.ifg);
		be_numbering_done(irg);

		pmap_destroy(chordal_env.border_heads);
	}

#ifdef DO_SSADESTR
	lower_perms(&chordal_env, options.lower_perm_method == BE_CH_LOWER_PERM_COPY ? 1 : 0);
#endif /* DO_SSADESTR */

	be_free_dominance_frontiers(chordal_env.dom_front);
	obstack_free(&chordal_env.obst, NULL);
}

const be_ra_t be_ra_chordal_allocator = {
#ifdef WITH_LIBCORE
	be_ra_chordal_register_options,
#endif
	be_ra_chordal_main
};
